#!/bin/sh

set -e

js="prod.js"
min="prod.min.js"

elm make src/Results.elm --optimize --output=$js $@

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $min

echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"


currentjs="current-games-prod.js"
currentmin="current-games-prod.min.js"

elm make src/CurrentGames.elm --optimize --output=$currentjs $@

uglifyjs $currentjs --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $currentmin

echo "Compiled size:$(cat $currentjs | wc -c) bytes  ($currentjs)"
echo "Minified size:$(cat $currentmin | wc -c) bytes  ($currentmin)"
echo "Gzipped size: $(cat $currentmin | gzip -c | wc -c) bytes"
