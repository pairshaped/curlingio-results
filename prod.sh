#!/bin/sh

set -e

js="prod.js"
min="prod.min.js"

elm make src/Results.elm --optimize --output=$js $@

uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $min

echo "Compiled size:$(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"


drawsjs="draws-prod.js"
drawsmin="draws-prod.min.js"

elm make src/CurrentDraws.elm --optimize --output=$drawsjs $@

uglifyjs $drawsjs --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $drawsmin

echo "Compiled size:$(cat $drawsjs | wc -c) bytes  ($drawsjs)"
echo "Minified size:$(cat $drawsmin | wc -c) bytes  ($drawsmin)"
echo "Gzipped size: $(cat $drawsmin | gzip -c | wc -c) bytes"
