#!/bin/bash

erlang_module=$1
elixir_module=$2

export_functions=$(cat $erlang_module  | \
sed -e '/^-export(\[/,/\])\./{
/-export(\[/d
/\])\./d
s/,//
p}
/.*/d')

module_name=$(cat $erlang_module | \
sed 's/-module(\([^)]\+\))\./\1/p
/.*/d')
# upcase first module character
Module_name=${module_name^?}

echo "module $Module_name" > $elixir_module

function gen_params() {
    nb_params=$1
    echo -n "("
    for i in `seq $nb_params`; do
        echo -n "arg${i}"
        if [ $i -ne $nb_params ]; then
            echo -n ", "
        fi
    done
    echo ")"
}

for func in $export_functions; do
    func_name=${func%%/*}
    nb_params=${func##*/}
    echo -n "  def $func_name" >> $elixir_module
    gen_params $nb_params >> $elixir_module
    echo -n "    Erlang.${module_name}.${func_name}" >> $elixir_module
    gen_params $nb_params >> $elixir_module
    echo "  end" >> $elixir_module
done

echo "end" >> $elixir_module
