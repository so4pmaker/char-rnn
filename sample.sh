ls -t cv/* | head -1
ls -t cv/* | head -1 | xargs th sample.lua -gpuid -1  -primetext "/*
===========================================================================

Doom 3 GPL Source Code" | tail +7 
