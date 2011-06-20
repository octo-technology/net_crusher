#!/bin/bash

for f in *.erl; do
    elixir_file=$(echo $f | sed -e 's/^\(.\)\(.*\)\.erl/ex\U\1\L\2.exs/')
    ./generate_module.sh $f ../exbin/${elixir_file}
done
