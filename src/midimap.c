/* midimap -- LV2 midi event mapper
 *
 * Copyright (C) 2016 Robin Gareus <robin@gareus.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>

#include <lv2/lv2plug.in/ns/lv2core/lv2.h>
#include <lv2/lv2plug.in/ns/ext/atom/atom.h>
#include <lv2/lv2plug.in/ns/ext/atom/forge.h>
#include <lv2/lv2plug.in/ns/ext/urid/urid.h>
#include <lv2/lv2plug.in/ns/ext/midi/midi.h>
#include <lv2/lv2plug.in/ns/ext/patch/patch.h>
#include <lv2/lv2plug.in/ns/ext/state/state.h>
#include <lv2/lv2plug.in/ns/ext/worker/worker.h>
#include <lv2/lv2plug.in/ns/ext/log/logger.h>

#define MEM_URI "http://gareus.org/oss/lv2/midimap"

#ifndef MAX_CFG_LINE_LEN
#define MAX_CFG_LINE_LEN 1024
#endif


typedef struct {
	LV2_URID atom_Blank;
	LV2_URID atom_Object;
	LV2_URID atom_Sequence;
	LV2_URID midi_MidiEvent;
	LV2_URID atom_URID;
	LV2_URID atom_Path;
	LV2_URID patch_Get;
	LV2_URID patch_Set;
	LV2_URID patch_property;
	LV2_URID patch_value;

	LV2_URID mem_state;
	LV2_URID mem_cfgfile;
} MidiMapURIs;


typedef struct {
	uint32_t len;
	uint8_t  mask[3];
	uint8_t  match[3];

	//uint32_t tx_len; // TODO allow to map length/type
	uint8_t  tx_mask[3];
	uint8_t  tx_set[3];
} Rule;

typedef struct {
	unsigned int count;
	bool forward_unmatched;
	bool match_all;
	Rule* rule;
} RuleSet;

typedef struct {
	const LV2_Atom_Sequence* midiin;
	LV2_Atom_Sequence* midiout;
	float* p_rulecount;

	/* atom-forge and URI mapping */
	LV2_URID_Map* map;
	MidiMapURIs uris;
	LV2_Atom_Forge forge;
	LV2_Atom_Forge_Frame frame;

	/* LV2 worker */
  LV2_Worker_Schedule* schedule;

	/* LV2 Output */
  LV2_Log_Log* log;
  LV2_Log_Logger logger;

	/* internal state */
	bool inform_ui;
	char* cfg_file_path;

	RuleSet* rules; // active rules
	RuleSet* state; // state load

} MidiMap;


/* *****************************************************************************
 * helper functions
 */

/** map uris */
static void
map_mem_uris (LV2_URID_Map* map, MidiMapURIs* uris)
{
	uris->atom_Blank         = map->map (map->handle, LV2_ATOM__Blank);
	uris->atom_Object        = map->map (map->handle, LV2_ATOM__Object);
	uris->midi_MidiEvent     = map->map (map->handle, LV2_MIDI__MidiEvent);
	uris->atom_Sequence      = map->map (map->handle, LV2_ATOM__Sequence);
	uris->atom_URID          = map->map (map->handle, LV2_ATOM__URID);
	uris->atom_Path          = map->map (map->handle, LV2_ATOM__Path);
	uris->patch_Get          = map->map (map->handle, LV2_PATCH__Get);
	uris->patch_Set          = map->map (map->handle, LV2_PATCH__Set);
	uris->patch_property     = map->map (map->handle, LV2_PATCH__property);
	uris->patch_value        = map->map (map->handle, LV2_PATCH__value);

	uris->mem_state          = map->map (map->handle, MEM_URI "#state");
	uris->mem_cfgfile        = map->map (map->handle, MEM_URI "#cfgfile");
}

/**
 * Get the file path from a message like:
 *   a patch:Set ;
 *   patch:property midimap:cfgfile ;
 *   patch:value </home/me/foo.cfg> .
 */
static const LV2_Atom*
parse_patch_msg (const MidiMapURIs* uris, const LV2_Atom_Object* obj)
{
	const LV2_Atom* property = NULL;
	const LV2_Atom* file_path = NULL;

	if (obj->body.otype != uris->patch_Set) {
		return NULL;
	}

	lv2_atom_object_get (obj, uris->patch_property, &property, 0);
	if (!property || property->type != uris->atom_URID) {
		return NULL;
	} else if (((LV2_Atom_URID*)property)->body != uris->mem_cfgfile) {
		return NULL;
	}

	lv2_atom_object_get(obj, uris->patch_value, &file_path, 0);
	if (!file_path || file_path->type != uris->atom_Path) {
		return NULL;
	}

	return file_path;
}

/** generate a file path message,
 * send current state to GUI
 */
static void inform_ui (MidiMap* self)
{
	if (!self->cfg_file_path) { return; }
	lv2_atom_forge_frame_time (&self->forge, 0);
	const MidiMapURIs* uris = &self->uris;

	LV2_Atom_Forge_Frame frame;
	lv2_atom_forge_object (&self->forge, &frame, 1, uris->patch_Set);
	lv2_atom_forge_property_head (&self->forge, uris->patch_property, 0);
	lv2_atom_forge_urid (&self->forge, uris->mem_cfgfile);
	lv2_atom_forge_property_head (&self->forge, uris->patch_value, 0);
	lv2_atom_forge_path (&self->forge, self->cfg_file_path, strlen (self->cfg_file_path));

	lv2_atom_forge_pop (&self->forge, &frame);
}

static void clear_rule (Rule *r)
{
	r->len = 0;
	for (size_t b = 0; b < 3; ++b) {
		r->mask[b]    = 0x00;
		r->match[b]   = 0x00;
		r->tx_mask[b] = 0xff;
		r->tx_set[b]  = 0x00;
	}
}

static void add_rule (RuleSet* rs, Rule *r)
{
	rs->rule = (Rule*) realloc (rs->rule, (rs->count + 1) * sizeof (Rule));
	if (!rs->rule) {
		fprintf (stderr, "MidiMap.lv2 error: Out Of Memory\n");
		rs->count = 0;
		return;
	}
	memcpy (&rs->rule[rs->count], r, sizeof (Rule));
	++rs->count;
}

static bool parse_status (Rule* r, const char* arg)
{
	int param[2];
	if        (!strcasecmp (arg, "ANY")) {
		r->mask[0] = 0x00; r->match[0] = 0x00;
	} else if (1 == sscanf (arg, "NOTEC%i", &param[0])) {
		r->mask[0] = 0xef; r->match[0] = 0x80 | (param[0] & 0xf);
	} else if (!strcasecmp (arg, "NOTE")) {
		r->mask[0] = 0xe0; r->match[0] = 0x80;
	} else if (!strcasecmp (arg, "NOTEOFF")) {
		r->mask[0] = 0xf0; r->match[0] = 0x80;
	} else if (!strcasecmp (arg, "NOTEON")) {
		r->mask[0] = 0xf0; r->match[0] = 0x90;
	} else if (!strcasecmp (arg, "Aftertouch")) {
		r->mask[0] = 0xf0; r->match[0] = 0xa0;
	} else if (!strcasecmp (arg, "CC")) {
		r->mask[0] = 0xf0; r->match[0] = 0xb0;
	} else if (!strcasecmp (arg, "Pitch")) {
		r->mask[0] = 0xf0; r->match[0] = 0xe0;
	} else if (!strcasecmp (arg, "PGM")) {
		r->mask[0] = 0xf0; r->match[0] = 0xc0;
	} else if (!strcasecmp (arg, "ChanPressure")) {
		r->mask[0] = 0xf0; r->match[0] = 0xd0; // Aftertouch, 2 bytes
	} else if (!strcasecmp (arg, "Pos")) {
		r->mask[0] = 0xff; r->match[0] = 0xf2; // Song Position Pointer, 3 bytes
	} else if (!strcasecmp (arg, "Song")) {
		r->mask[0] = 0xff; r->match[0] = 0xf3; // Song select 2 bytes
	} else if (!strcasecmp (arg, "Start")) {
		r->mask[0] = 0xff; r->match[0] = 0xfa; // rt 1 byte
	} else if (!strcasecmp (arg, "Cont")) {
		r->mask[0] = 0xff; r->match[0] = 0xfb; // rt 1 byte
	} else if (!strcasecmp (arg, "Stop")) {
		r->mask[0] = 0xff; r->match[0] = 0xfc; // rt 1 byte
	} else if (2 == sscanf (arg, "%i/%i", &param[0], &param[1])) {
		r->mask[0] = param[1] & 0xff;
		r->match[0] = param[0] & 0xff;
	} else if (1 == sscanf (arg, "%i", &param[0])) {
		r->mask[0] = 0xff;
		r->match[0] = param[0] & 0xff;
	} else {
		return false;
	}
	return true;
}

static uint8_t parse_note (const char* arg)
{
	if (strlen (arg) < 2) {
		return 0xff;
	}

	int pos = 1;

	int key;
	switch (arg[0]) {
		case 'c': case 'C': key =  0; break;
		case 'd': case 'D': key =  2; break;
		case 'e': case 'E': key =  4; break;
		case 'f': case 'F': key =  5; break;
		case 'g': case 'G': key =  7; break;
		case 'a': case 'A': key =  9; break;
		case 'b': case 'B': key = 11; break;
		default: return 0xff;
	}

	if (arg[1] == '#') { ++key; ++pos;}
	else if (arg[1] == 'b') { --key; ++pos;}

	if (strlen (arg) < pos + 1) {
		return 0xff;
	}

	int octave = atoi (&arg[pos]);

	// c-2 == 0
	int note = (octave + 1) * 12 + key;
	if (note >= 0 && note <= 127) {
		return note;
	}
	return 0xff;
}

static bool parse_match (Rule* r, unsigned int i, const char* arg)
{
	int param[2];
	if (!strcasecmp (arg, "ANY")) {
		r->mask[i] = 0x00; r->match[i] = 0x00;
	} else if (2 == sscanf (arg, "%i/%i", &param[0], &param[1])) {
		r->mask[i] = param[1] & 0xff;
		r->match[i] = param[0] & 0xff;
	} else if (1 == sscanf (arg, "%i", &param[0])) {
		r->mask[i] = (i == 0) ? 0xff : 0x7f;
		r->match[i] = param[0] & 0xff;
	} else {
		return false;
	}
	return true;
}

static bool parse_replacement (Rule* r, unsigned int i, const char* arg)
{
	int param[2];
	if (!strcasecmp (arg, "SAME")) {
		r->tx_mask[i] = 0xff;
		r->tx_set[i] = 0x00;
	} else if ((i == 0) && 1 == sscanf (arg, "CHN%i", &param[0])) {
		r->tx_mask[i] = 0xf0;
		r->tx_set[i] = param[0] & 0x0f;
	} else if (2 == sscanf (arg, "%i/%i", &param[0], &param[1])) {
		r->tx_mask[i] = param[1] & 0xff;
		r->tx_set[i] = param[0] & 0xff;
	} else if (1 == sscanf (arg, "%i", &param[0])) {
		r->tx_mask[i] = 0x00;
		r->tx_set[i] = param[0] & 0xff;
	} else {
		return false;
	}
	return true;
}

static bool parse_line_v1 (RuleSet* rs, const char* line)
{
	if (0 == strcmp (line, "forward-unmatched")) {
		rs->forward_unmatched = true;
		return true;
	}

	if (0 == strcmp (line, "match-all")) {
		rs->match_all = true;
		return true;
	}

	Rule r;
	clear_rule (&r);

	char *tmp, *fre, *prt;
	int i = 0;
	bool in_match = true;
	tmp = fre = strdup (line);
	for (prt = strtok (tmp, " "); prt; prt = strtok (NULL, " "), ++i) {
		bool rv;
		if (prt[0] == '#') {
			break;
		}
		if (0 == strcmp (prt, "|")) {
			if (i == 0 || !in_match) {
				i = -1;
				break;
			}
			in_match = false;
			r.len = i;
			i = -1; // continue bumps it
			continue;
		}
		if (i >= 3) {
			i = -1;
			break;
		}

		if (in_match) {
			switch (i) {
				case 0:
					rv = parse_status (&r, prt);
					break;
				case 1:
					r.match[i] = parse_note (prt); // TODO IFF note-status..
					if (r.match[i] < 128) {
						r.mask[i] = 0x7f;
						rv = true;
						break;
					}
					// no break - fall through
				default:
					rv = parse_match (&r, i, prt);
					break;
			}
		} else {
			switch (i) {
				case 1:
					r.tx_set[i] = parse_note (prt); // TODO IFF note-status..
					if (r.tx_set[i] < 128) {
						r.tx_mask[i] = 0x7f;
						rv = true;
						break;
					}
					// no break - fall through
				default:
					rv = parse_replacement (&r, i, prt);
					break;
			}
		}

		if (!rv) {
			i = -1;
			break;
		}
	}

	free (fre);
	if (i < 1 || i > 3 || in_match) {
		return false;
	}
	if (i != r.len) {
		// currently only same-length
		return false;
	}

	add_rule (rs, &r);
#ifndef NDEBUG
	printf ("Rule [%d]", r.len);
	for (uint32_t b = 0; b < r.len; ++b) {
		printf (" %02x/%02x", r.match[b], r.mask[b]);
	}
	printf (" |");
	for (uint32_t b = 0; b < r.len; ++b) {
		printf (" %02x/%02x", r.tx_set[b], r.tx_mask[b]);
	}
	printf ("\n");
#endif
	return true;
}

/** non-realtime function to read config,
 * called from state-restore or worker-thread
 */
static void
parse_config_file (MidiMap* self, const char* fn)
{
	assert (self->state == NULL);
	FILE *f;
	if (!fn) {
		lv2_log_error (&self->logger, "MidiMap.lv2: invalid config file handle");
	}
	if (!(f = fopen (fn, "r"))) {
		lv2_log_error (&self->logger, "MidiMap.lv2: cannot open config file '%s'", fn);
		return;
	}
	lv2_log_note (&self->logger, "MidiMap.lv2: parsing config file '%s'", fn);

	bool ok = true;

	self->state = calloc (1, sizeof (RuleSet));

	char line[MAX_CFG_LINE_LEN];
	unsigned int lineno = 0;
	unsigned int cfg_version = 0;
	while (fgets (line, MAX_CFG_LINE_LEN - 1, f) != NULL ) {
		++lineno;
		if (strlen (line) == MAX_CFG_LINE_LEN - 1) {
			lv2_log_error (&self->logger, "MidiMap.lv2: Too long config line %d", lineno);
			continue;
		}
		// strip trailing whitespace
		while (strlen (line) > 0 && (line[strlen (line) - 1] == '\n' || line[strlen (line) - 1] == '\r' || line[strlen (line) - 1] == ' ' || line[strlen (line) - 1] == '\t')) {
			line[strlen (line) - 1] = '\0';
		}
		// ignore comments and empty lines
		if (strlen (line) == 0 || line[0] == '#') {
			continue;
		}

		if (0 == strncmp (line, "midimap v", 9) && strlen (line) > 9) {
			cfg_version = atoi (&line[9]);
			continue;
		}

		switch (cfg_version) {
			case 1:
				if (!parse_line_v1 (self->state, line)) {
					lv2_log_error (&self->logger, "MidiMap.lv2: Parser error on line %d", lineno);
				}
				break;
			default:
				lv2_log_error (&self->logger, "MidiMap.lv2: invalid version '%d' on config line %d",
						cfg_version, lineno);
				break;
		}
	}

	fclose (f);
	if (ok) {
		/* remember config file - for state */
		free (self->cfg_file_path);
		self->cfg_file_path = strdup (fn);
	} else {
		lv2_log_error (&self->logger, "MidiMap.lv2: error parsing config file");
		free (self->state);
		self->state = NULL;
	}
}

/** apply parsed config (if any) in rt-thread,
 * return true if there's an old ruleset to be free()d
 */
static void
activate_config (MidiMap* self)
{
	if (self->state) {
		RuleSet* old = self->rules;
		self->rules = self->state;
		self->state = old;
	}
}

/**
 * add a midi message to the output port
 */
static void
forge_midimessage (MidiMap* self,
		uint32_t tme,
		const uint8_t* const buffer,
		uint32_t size)
{
	LV2_Atom midiatom;
	midiatom.type = self->uris.midi_MidiEvent;
	midiatom.size = size;

	if (0 == lv2_atom_forge_frame_time (&self->forge, tme)) return;
	if (0 == lv2_atom_forge_raw (&self->forge, &midiatom, sizeof (LV2_Atom))) return;
	if (0 == lv2_atom_forge_raw (&self->forge, buffer, size)) return;
	lv2_atom_forge_pad (&self->forge, sizeof (LV2_Atom) + size);
}

/* *****************************************************************************
 * the actual "process" function, called for every midi event
 */
static void
filter_midimessage (MidiMap* self,
		uint32_t tme,
		const uint8_t* const mmsg,
		uint32_t size)
{
	if (!self->rules  || size > 3) {
		/* just foward */
		forge_midimessage (self, tme, mmsg, size);
		return;
	}

	bool matched = false;

	// TODO use tree with masked status or a hash-table
	const unsigned int rc = self->rules->count;
	for (unsigned int i = 0; i < rc; ++i) {
		Rule *r = &self->rules->rule[i];
		if (   (r->len == size)
				&& (            (mmsg[0] & r->mask[0]) == r->match[0])
				&& (size < 2 || (mmsg[1] & r->mask[1]) == r->match[1])
				&& (size < 3 || (mmsg[2] & r->mask[2]) == r->match[2])
			 )
		{
			uint8_t msg[3];
			for (uint32_t b = 0; b < size; ++b) {
				msg[b] = (mmsg[b] & r->tx_mask[b]) | r->tx_set[b];
			}
			forge_midimessage (self, tme, msg, size);
			matched = true;
			if (!self->rules->match_all) {
				break;
			}
		}
	}

	if (!matched && self->rules->forward_unmatched) {
		forge_midimessage (self, tme, mmsg, size);
	}
}


/* *****************************************************************************
 * LV2 Plugin
 */

static LV2_Handle
instantiate (const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
	MidiMap* self = (MidiMap*)calloc (1, sizeof (MidiMap));

	int i;
	for (i=0; features[i]; ++i) {
		if (!strcmp (features[i]->URI, LV2_URID__map)) {
			self->map = (LV2_URID_Map*)features[i]->data;
    } else if (!strcmp (features[i]->URI, LV2_WORKER__schedule)) {
      self->schedule = (LV2_Worker_Schedule*)features[i]->data;
    } else if (!strcmp (features[i]->URI, LV2_LOG__log)) {
      self->log = (LV2_Log_Log*)features[i]->data;
		}
	}

	if (!self->map) {
		fprintf (stderr, "MidiMap.lv2 error: Host does not support urid:map\n");
		free (self);
		return NULL;
	}
	if (!self->schedule) {
		fprintf (stderr, "MidiMap.lv2 error: Host does not support worker:schedule\n");
		free (self);
		return NULL;
	}

	lv2_atom_forge_init (&self->forge, self->map);
	map_mem_uris (self->map, &self->uris);
  lv2_log_logger_init (&self->logger, self->map, self->log);
	return (LV2_Handle)self;
}

static void
connect_port (LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
	MidiMap* self = (MidiMap*)instance;

	switch (port) {
		case 0:
			self->midiin = (const LV2_Atom_Sequence*)data;
			break;
		case 1:
			self->midiout = (LV2_Atom_Sequence*)data;
			break;
		case 2:
			self->p_rulecount = (float*)data;
		default:
			break;
	}
}

static void
run (LV2_Handle instance, uint32_t n_samples)
{
	MidiMap* self = (MidiMap*)instance;
	if (!self->midiout || !self->midiin) {
		return;
	}

	/* prepare midiout port */
	const uint32_t capacity = self->midiout->atom.size;
	lv2_atom_forge_set_buffer (&self->forge, (uint8_t*)self->midiout, capacity);
	lv2_atom_forge_sequence_head (&self->forge, &self->frame, 0);

	/* inform the GUI */
	if (self->inform_ui) {
		self->inform_ui = false;
		inform_ui (self);
	}

	/* Process incoming midi events */
	LV2_Atom_Event* ev = lv2_atom_sequence_begin (&(self->midiin)->body);
	while (!lv2_atom_sequence_is_end (&(self->midiin)->body, (self->midiin)->atom.size, ev)) {
		if (ev->body.type == self->uris.midi_MidiEvent) {
			/* process midi event */
			filter_midimessage (self, ev->time.frames, (uint8_t*)(ev+1), ev->body.size);
		} else {
			/* schedule loading config file */
			const LV2_Atom_Object* obj = (LV2_Atom_Object*)&ev->body;
			if (obj->body.otype == self->uris.patch_Set) {
				self->schedule->schedule_work (self->schedule->handle, lv2_atom_total_size (&ev->body), &ev->body);
			}
		}
		ev = lv2_atom_sequence_next (ev);
	}

	/* close off atom sequence */
	//lv2_atom_forge_pop (&self->forge, &self->frame);
	/* report active rules */
	*self->p_rulecount = self->rules ? self->rules->count : 0;
}

static void
cleanup (LV2_Handle instance)
{
	MidiMap* self = (MidiMap*)instance;
	free (self->rules);
	free (self->state);
	free (instance);
}


/* *****************************************************************************
 * LV2 interfaces: state load/save and worker-thread file i/o
 */

static LV2_Worker_Status
work (LV2_Handle                  instance,
      LV2_Worker_Respond_Function respond,
      LV2_Worker_Respond_Handle   handle,
      uint32_t                    size,
      const void*                 data)
{
	MidiMap* self = (MidiMap*)instance;

	if (self->state) {
		assert (size == sizeof (int) && 0x5780 == *((int*)data));
		free (self->state);
		self->state = 0;
		return LV2_WORKER_SUCCESS;
	}

	const LV2_Atom_Object* obj = (const LV2_Atom_Object*) data;
	MidiMapURIs* uris = &self->uris;

	const LV2_Atom* file_path = parse_patch_msg (uris, obj);
	if (file_path) {
		const char *fn = (const char*)(file_path+1);
		parse_config_file (self, fn);
	}
	respond (handle, 1, "");
  return LV2_WORKER_SUCCESS;
}

static LV2_Worker_Status
work_response (LV2_Handle  instance,
               uint32_t    size,
               const void* data)
{
	MidiMap* self = (MidiMap*)instance;
	activate_config (self);
	if (self->state) {
		int d = 0x5780; // magic
		self->schedule->schedule_work (self->schedule->handle, sizeof (int), &d);
	}
	self->inform_ui = true;
	return LV2_WORKER_SUCCESS;
}

static LV2_State_Status
save (LV2_Handle                instance,
      LV2_State_Store_Function  store,
      LV2_State_Handle          handle,
      uint32_t                  flags,
      const LV2_Feature* const* features)
{
	MidiMap* self = (MidiMap*)instance;
  LV2_State_Map_Path* map_path = NULL;

  for (int i = 0; features[i]; ++i) {
    if (!strcmp (features[i]->URI, LV2_STATE__mapPath)) {
      map_path = (LV2_State_Map_Path*) features[i]->data;
    }
  }

  if (map_path && self->cfg_file_path) {
		char* apath = map_path->abstract_path (map_path->handle, self->cfg_file_path);
		store (handle, self->uris.mem_cfgfile,
				apath, strlen (apath) + 1,
				self->uris.atom_Path,
				LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);
	}
  return LV2_STATE_SUCCESS;
}

static LV2_State_Status
restore (LV2_Handle                  instance,
         LV2_State_Retrieve_Function retrieve,
         LV2_State_Handle            handle,
         uint32_t                    flags,
         const LV2_Feature* const*   features)
{
	MidiMap* self = (MidiMap*)instance;
	if (self->state) {
		fprintf (stderr, "MidiMap.lv2 error: state restore ignored\n");
		return LV2_STATE_SUCCESS; // or  LV2_STATE_ERR_UNKNOWN ??
	}

  size_t   size;
  uint32_t type;
  uint32_t valflags;
  const void* value;

	value = retrieve (handle, self->uris.mem_cfgfile, &size, &type, &valflags);

	if (value) {
		const char* fn = (const char*)value;
		parse_config_file (self, fn);
		activate_config (self);
		free (self->state);
		self->state = NULL;
		self->inform_ui = true;
	}
  return LV2_STATE_SUCCESS;
}

const void*
extension_data (const char* uri)
{
	static const LV2_Worker_Interface worker = { work, work_response, NULL };
	static const LV2_State_Interface  state  = { save, restore };
	if (!strcmp (uri, LV2_WORKER__interface)) {
		return &worker;
	}
	else if (!strcmp (uri, LV2_STATE__interface)) {
		return &state;
	}
	return NULL;
}

static const LV2_Descriptor descriptor = {
	MEM_URI,
	instantiate,
	connect_port,
	NULL,
	run,
	NULL,
	cleanup,
	extension_data
};

#undef LV2_SYMBOL_EXPORT
#ifdef _WIN32
#    define LV2_SYMBOL_EXPORT __declspec(dllexport)
#else
#    define LV2_SYMBOL_EXPORT  __attribute__ ((visibility ("default")))
#endif
LV2_SYMBOL_EXPORT
const LV2_Descriptor*
lv2_descriptor (uint32_t index)
{
	switch (index) {
	case 0:
		return &descriptor;
	default:
		return NULL;
	}
}
