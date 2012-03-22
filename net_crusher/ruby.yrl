Nonterminals

root
block
commands
command
map
map_content
map_elem
array
array_content
expr
args
function
function_call
crs
.

Terminals

if
then
else
do
end
begin
while
to_i
to_s
true
false
func_call_start

'symbol'
'integer'
'string'
'cr'
';'
'('
')'
','
'{'
'}'
'['
']'
'='
'<'
'>'
'.'
'+'
'-'
'!'
'!='
'=='
.

Rootsymbol root.

root -> crs root : '$2'.
root -> commands : {commands, '$1'}.
root -> expr crs : {expr, '$1'}.

crs -> cr.
crs -> ';'.
crs -> cr crs.

commands -> command commands : ['$1' | '$2'].
commands -> command : ['$1'].

block -> do crs commands end : { block, '$3' }.
block -> do commands end : { block, '$3' }.

function_call -> func_call_start ')': { element(3, '$1'), function_call, element(2, '$1'), []}.
function_call -> func_call_start args ')': { element(3, '$1'), function_call, element(2, '$1'), '$2'}.

function_call -> function : { element(1, '$1'), function_call, element(2, '$1'), []}.
function_call -> function args : { element(1, '$1'), function_call, element(2, '$1'), '$2' }.
function_call -> function args block : { element(1, '$1'), function_call, element(2, '$1'), '$2' ++ ['$3'] }.

function -> symbol : { element(3, '$1'), element(2, '$1') }.

args -> expr : [ '$1' ].
args -> expr ',' args : [ '$1' | '$3' ].

command -> function_call : '$1'.
command -> function_call crs : '$1'.

expr -> function_call : {function, '$1'}.
expr -> true : { bool, true }.
expr -> false : { bool, false }.
expr -> '(' expr ')' : '$2'.
expr -> string : process_string('$1').
expr -> expr '.' 'to_s' : '$1'.
expr -> expr '.' 'to_i' : '$1'.
expr -> integer : { integer, element(3, '$1')}.
expr -> expr '+' expr : { function, { integer, { element(2, '$2'), erlang_statment, arithmetic, int_add, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> expr '-' expr : { function, { integer, { element(2, '$2'), erlang_statment, arithmetic, int_sub, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> '!' expr : { function, { bool, { element(2, '$1'), erlang_statment, arithmetic, bool_not, [{bool, '$2'}]}}}.
expr -> expr '<' expr : { function, { bool, { element(2, '$2'), erlang_statment, arithmetic, bool_int_lesser, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> expr '>' expr : { function, { bool, {  element(2, '$2'), erlang_statment, arithmetic, bool_int_greater, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> expr '==' expr : { function, { bool, {  element(2, '$2'), erlang_statment, arithmetic, bool_str_equal, [{string, '$1'}, {string, '$3'}]}}}.
expr -> expr '!=' expr : { function, { bool, {  element(2, '$2'), erlang_statment, arithmetic, bool_str_different, [{string, '$1'}, {string, '$3'}]}}}.

expr -> array : {array, '$1'}.
expr -> map : {map, '$1'}.

array -> '[' array_content ']' : '$2'.
array -> '[' ']' : [].

array_content -> expr : '$1'.
array_content -> expr ',' array_content : ['$1' | '$3'].

map -> '{' map_content '}' : '$2'.
map -> '{' '}' : [].

map_content -> map_elem : ['$1'].
map_content -> map_elem ',' map_content : ['$1' | '$3'].

map_elem -> expr '=' '>' expr : {'$1', '$4'}.

command -> if expr then commands end crs : {void, { element(2, '$1'), erlang_statment, statment, expr_if, [{bool, '$2'}, {commands, {block, '$4'}}]}}.
command -> if expr then crs commands end crs : {void, { element(2, '$1'), erlang_statment, statment, expr_if, [{bool, '$2'}, {commands, {block, '$5'}}]}}.
command -> if expr crs commands end crs : {void, { element(2, '$1'), erlang_statment, statment, expr_if, [{bool, '$2'}, {commands, {block, '$4'}}]}}.

command -> if expr then crs commands else crs commands end crs : {void, { element(2, '$1'), erlang_statment, statment, expr_if_else, [{bool, '$2'}, {commands, {block, '$5'}}, {commands, {block, '$8'}}]}}.
command -> if expr crs commands else crs commands end crs : {void, { element(2, '$1'), erlang_statment, statment, expr_if_else, [{bool, '$2'}, {commands, {block, '$4'}}, {commands, {block, '$7'}}]}}.

command -> begin crs commands end while expr crs : { void, { element(2, '$1'), erlang_statment, statment, expr_do_while, [{evaluable_bool, '$6'}, {commands, {block, '$3'}}]}}.

command -> symbol '=' expr crs : { void, { element(3, '$1'), erlang_statment, vars, cmd_s, [{string, [{string, element(2, '$1')}]}, {string, '$3'}]}}.

Erlang code.

-export([scan/1, scan_file/1, parse_str/2, parse_file/1, post_process/1]).

process_string({string, LineNumber, Str}) ->
  try
    {string, extract_interpreted(Str, LineNumber)}
  catch
    _:E -> throw({interpreted_string_parse_error, LineNumber, Str, E})
  end.

extract_interpreted("", _) -> [];
extract_interpreted(Str, LineNumber) ->
  case re:run(Str, "#\{([^\}]+)\}") of
    nomatch -> [{string, Str}];
    {match, [{_, _}, {Start, Stop}]} ->
      R = parse_str(string:substr(Str, Start + 1, Stop), LineNumber),
      Result = case R of
        {ok, {expr, V}} -> V;
        {ok, {commands, [V]}} -> {function, V}
      end,
      Middle = {interpreted_string, Result},
      End = [ Middle | extract_interpreted(string:substr(Str, Start + Stop + 2), LineNumber)],
      case Start of
        2 -> End;
        _ -> [{string, string:substr(Str, 1, Start - 2)} | End]
      end
  end.

scan(Str) -> lexer:lex(Str).

scan_file(FileName) ->
  {ok, File} = file:read_file(FileName),
  scan(binary_to_list(File)).

post_process({ok, L, K}) -> {ok, post_process(L), K};
post_process([{func_call_start, LineNumber, "if"} | T]) -> [{'if', LineNumber}, {'(', LineNumber} | post_process(T)];
post_process([{func_call_start, LineNumber, Value} | T]) -> [{'func_call_start', Value, LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "if"} | T]) -> [{'if', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "then"} | T]) -> [{'then', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "else"} | T]) -> [{'else', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "do"} | T]) -> [{'do', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "end"} | T]) -> [{'end', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "begin"} | T]) -> [{'begin', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "while"} | T]) -> [{'while', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "to_i"} | T]) -> [{'to_i', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "to_s"} | T]) -> [{'to_s', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "true"} | T]) -> [{'true', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, "false"} | T]) -> [{'false', LineNumber} | post_process(T)];
post_process([{atom, LineNumber, Value} | T]) -> [{symbol, Value, LineNumber} | post_process(T)];
post_process([{Type, LineNumber, Value} | T]) -> [{Type, LineNumber, Value} | post_process(T)];
post_process([{Token, LineNumber} | T]) -> [{Token, LineNumber} | post_process(T)];
post_process([]) -> [].

parse_str(Str, LineNumber) ->
  {ok, List, _} = post_process(lexer:lex(Str, LineNumber)),
  parse(List).

parse_file(FileName) ->
  {ok, List, _} = post_process(scan_file(FileName)),
  case parse(List) of
    {ok, Content} -> {ok, Content};
    X -> throw(X)
  end.
