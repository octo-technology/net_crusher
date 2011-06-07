Nonterminals

root
commands
command
map
map_content
map_elem
array
array_content
expr
.

Terminals

##TOKENS##

if
then
do
end
begin
while
to_i
to_s
true
false
call

'integer'
','
'{'
'}'
'['
']'
'('
')'
'='
'<'
'>'
'.'
'+'
'-'
'string'
'!'
.

Rootsymbol root.

root -> commands : {commands, '$1'}.
root -> expr : {expr, '$1'}.

commands -> command commands : ['$1' | '$2'].
commands -> command : ['$1'].

expr -> true : { bool, true }.
expr -> false : { bool, false }.
expr -> '(' expr ')' : '$2'.
expr -> string : process_string('$1').
expr -> expr '.' 'to_s' : '$1'.
expr -> expr '.' 'to_i' : '$1'.
expr -> integer : { integer, element(3, '$1')}.
expr -> expr '+' expr : { function, { integer, { element(2, '$2'), arithmetic, int_add, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> expr '-' expr : { function, { integer, { element(2, '$2'), arithmetic, int_sub, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> '!' expr : { function, { bool, { element(2, '$1'), arithmetic, bool_not, [{bool, '$2'}]}}}.

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

command -> if expr then commands end : { element(2, '$1'), statment, expr_if, [{bool, '$2'}, {commands, '$4'}]}.
command -> begin commands end while expr : { element(2, '$1'), statment, expr_do_while, [{evaluable_bool, '$5'}, {commands, '$2'}]}.
expr -> expr '<' expr : { function, { bool, { element(2, '$2'), arithmetic, bool_int_lesser, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> expr '>' expr : { function, { bool, {  element(2, '$2'), arithmetic, bool_int_greater, [{integer, '$1'}, {integer, '$3'}]}}}.
expr -> expr '=' '=' expr : { function, { bool, {  element(2, '$2'), arithmetic, bool_str_equal, [{string, '$1'}, {string, '$4'}]}}}.
expr -> expr '!' '=' expr : { function, { bool, {  element(2, '$2'), arithmetic, bool_str_different, [{string, '$1'}, {string, '$4'}]}}}.

expr -> call '(' expr ',' expr ')' : { function, { string, {  element(2, '$1'), statment, make_call, [{string, '$3'}, {string, '$5'}]}}}.

##RULES##

Erlang code.

-export([scan/1, scan_file/1, parse_str/1, parse_file/1]).

process_string({string, LineNumber, Str}) ->
  try
    {string, extract_interpreted(Str)}
  catch
    _:E -> throw({interpreted_string_parse_error, LineNumber, Str, E})
  end.

extract_interpreted("") -> [];
extract_interpreted(Str) ->
  case re:run(Str, "#\{([^\}]+)\}") of
    nomatch -> [{string, Str}];
    {match, [{_, _}, {Start, Stop}]} ->
      {ok, {expr, Result}} = parse_str(string:substr(Str, Start + 1, Stop)),
      Middle = {interpreted_string, Result},
      End = [ Middle | extract_interpreted(string:substr(Str, Start + Stop + 2))],
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
post_process([{atom, LineNumber, Value} | T]) -> [{list_to_atom(Value), LineNumber} | post_process(T)];
post_process([{Type, LineNumber, Value} | T]) -> [{Type, LineNumber, Value} | post_process(T)];
post_process([{Token, LineNumber} | T]) -> [{Token, LineNumber} | post_process(T)];
post_process([]) -> [].

parse_str(Str) ->
  {ok, List, _} = post_process(scan(Str)),
  parse(List).

parse_file(FileName) ->
  {ok, List, _} = post_process(scan_file(FileName)),
  case parse(List) of
    {ok, Content} -> {ok, Content};
    X -> throw(X)
  end.
