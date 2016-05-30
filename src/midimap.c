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

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"
#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"
#include "lv2/lv2plug.in/ns/ext/midi/midi.h"

#define MEM_URI "http://gareus.org/oss/lv2/midimap"

typedef struct {
	LV2_URID atom_Blank;
	LV2_URID atom_Object;
	LV2_URID atom_Sequence;
	LV2_URID midi_MidiEvent;
} MidiMapURIs;

typedef struct {
	const LV2_Atom_Sequence* midiin;
	LV2_Atom_Sequence* midiout;

	/* atom-forge and URI mapping */
	LV2_URID_Map* map;
	MidiMapURIs uris;
	LV2_Atom_Forge forge;
	LV2_Atom_Forge_Frame frame;

} MidiMap;


static void
map_mem_uris (LV2_URID_Map* map, MidiMapURIs* uris)
{
	uris->atom_Blank         = map->map (map->handle, LV2_ATOM__Blank);
	uris->atom_Object        = map->map (map->handle, LV2_ATOM__Object);
	uris->midi_MidiEvent     = map->map (map->handle, LV2_MIDI__MidiEvent);
	uris->atom_Sequence      = map->map (map->handle, LV2_ATOM__Sequence);
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

/**
 * the actual "process" function, called for every midi event
 */
static void
filter_midimessage (MidiMap* self,
		uint32_t tme,
		const uint8_t* const buffer,
		uint32_t size)
{
	forge_midimessage (self, tme, buffer, size);
}


/*******************************************************************************
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
		}
	}

	if (!self->map) {
		fprintf (stderr, "MidiMap.lv2 error: Host does not support urid:map\n");
		free (self);
		return NULL;
	}

	lv2_atom_forge_init (&self->forge, self->map);
	map_mem_uris (self->map, &self->uris);

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

	/* Process incoming midi events */
	LV2_Atom_Event* ev = lv2_atom_sequence_begin (&(self->midiin)->body);
	while (!lv2_atom_sequence_is_end (&(self->midiin)->body, (self->midiin)->atom.size, ev)) {
		if (ev->body.type == self->uris.midi_MidiEvent) {
			filter_midimessage (self, ev->time.frames, (uint8_t*)(ev+1), ev->body.size);
		}
		ev = lv2_atom_sequence_next (ev);
	}

	/* close off atom sequence */
	lv2_atom_forge_pop (&self->forge, &self->frame);
}

static void
cleanup (LV2_Handle instance)
{
	free (instance);
}

const void*
extension_data (const char* uri)
{
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
