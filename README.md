midimap.lv2 - Midi Event Mapper
===============================

midimap.lv2 is a flexible MIDI event mapping plugin, using a rule-based
lookup-table, which is loaded from a config file.

Install
-------

Compiling midimap.lv2 requires the LV2 SDK, gnu-make, and a c-compiler.

```bash
  git clone git://github.com/x42/midimap.lv2.git
  cd midimap.lv2
  make
  sudo make install PREFIX=/usr
```

Note to packagers: The Makefile honors `PREFIX` and `DESTDIR` variables as well
as `CFLAGS`, `LDFLAGS` and `OPTIMIZATIONS` (additions to `CFLAGS`), also
see the first 10 lines of the Makefile.
