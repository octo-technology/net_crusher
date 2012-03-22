%% NetCrusher.
%% Copyright (C) 2011 Bertrand Paquet, David Rousselie All Rights Reserved

%% NetCrusher is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2.1 of the License, or (at your option) any later version.

%% NetCrusher is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.

%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
-module(lexer).

-export([lex/1,
         lex/2
        ]).

lex(Str) ->
  lex(Str, 1).

lex(Str, LineNumber) ->
  L = parse(Str ++ " \n", LineNumber),
  {Tokens, [Size]} = lists:split(length(L) - 1, L),
  {ok, Tokens, Size - 1}.

parse(Str, LineNumber) ->
  case string:strip(Str, left) of
    "" -> [LineNumber];
    [$\n | Tail] -> [{cr, LineNumber} | parse(Tail, LineNumber + 1)];
    [$\r | Tail] -> parse(Tail, LineNumber);
    [$!, $= | Tail] -> [{'!=', LineNumber} | parse(Tail, LineNumber)];
    [$=, $= | Tail] -> [{'==', LineNumber} | parse(Tail, LineNumber)];
    [$=, $> | Tail] -> [{'=>', LineNumber} | parse(Tail, LineNumber)];
    [$. | Tail] -> [{'.', LineNumber} | parse(Tail, LineNumber)];
    [$, | Tail] -> [{',', LineNumber} | parse(Tail, LineNumber)];
    [$+ | Tail] -> [{'+', LineNumber} | parse(Tail, LineNumber)];
    [$- | Tail] -> [{'-', LineNumber} | parse(Tail, LineNumber)];
    [$* | Tail] -> [{'*', LineNumber} | parse(Tail, LineNumber)];
    [$/ | Tail] -> [{'/', LineNumber} | parse(Tail, LineNumber)];
    [$( | Tail] -> [{'(', LineNumber} | parse(Tail, LineNumber)];
    [$) | Tail] -> [{')', LineNumber} | parse(Tail, LineNumber)];
    [$= | Tail] -> [{'=', LineNumber} | parse(Tail, LineNumber)];
    [$! | Tail] -> [{'!', LineNumber} | parse(Tail, LineNumber)];
    [$< | Tail] -> [{'<', LineNumber} | parse(Tail, LineNumber)];
    [$> | Tail] -> [{'>', LineNumber} | parse(Tail, LineNumber)];
    [${ | Tail] -> [{'{', LineNumber} | parse(Tail, LineNumber)];
    [$} | Tail] -> [{'}', LineNumber} | parse(Tail, LineNumber)];
    [$[ | Tail] -> [{'[', LineNumber} | parse(Tail, LineNumber)];
    [$] | Tail] -> [{']', LineNumber} | parse(Tail, LineNumber)];
    [$# | Tail] ->
       {_, After} = find_delimiter_and_unescape($\n, Tail),
      [{cr, LineNumber} | parse(After, LineNumber + 1)];
    [$" | Tail] ->
      {ParsedString, After} = find_delimiter_and_unescape($", Tail),
      [{string, LineNumber, ParsedString} | parse(After, LineNumber)];
    [$' | Tail] ->
      {ParsedString, After} = find_delimiter_and_unescape($', Tail),
      [{string, LineNumber, ParsedString} | parse(After, LineNumber)];
    Current ->
      {Token, Tail} = parse_with_regex(Current ++ " ", LineNumber, [
        {"^([0-9]+)[^0-9\\.]", fun(S, L) -> {integer, L, list_to_integer(S)} end},
        {"^([0-9\\.]+)[^0-9\\.]", fun(S, L) -> {float, L, list_to_float(S)} end},
        {"^([a-zA-Z0-9_]+\\s*\\()", fun(S, L) -> {func_call_start, L, string:strip(string:substr(S, 1, length(S) - 1), right)} end},
        {"^([a-zA-Z0-9_]+)[^a-zA-Z0-9_]", fun(S, L) -> {atom, L, S} end}
        ]),
      [Token | parse(Tail, LineNumber)]
  end.

parse_with_regex(Str, LineNumber, []) -> throw({lexer_error, unable_to_parse, Str, LineNumber});
parse_with_regex(Str, LineNumber, [{Regex, Callback} | Tail]) ->
  case re:run(Str, Regex) of
    {match, [{_, _}, {A1, A2}]} -> {Callback(string:substr(Str, A1 + 1, A2), LineNumber), string:substr(Str, A2 + 1)};
    nomatch -> parse_with_regex(Str, LineNumber, Tail)
  end.

find_delimiter_and_unescape(Delim, Str) -> find_delimiter_and_unescape("", Delim, Str).

find_delimiter_and_unescape(Before, Delim, [$\\, $" | Tail]) -> find_delimiter_and_unescape(Before ++ [$"], Delim, Tail);
find_delimiter_and_unescape(Before, Delim, [$\\, $' | Tail]) -> find_delimiter_and_unescape(Before ++ [$'], Delim, Tail);
find_delimiter_and_unescape(Before, Delim, [$\\, $\\ | Tail]) -> find_delimiter_and_unescape(Before ++ [$\\], Delim, Tail);
find_delimiter_and_unescape(Before, Delim, [$\\, $n | Tail]) -> find_delimiter_and_unescape(Before ++ [$\n], Delim, Tail);
find_delimiter_and_unescape(_, _, [$\\, X | _]) -> throw({lexer_error, unkown_escaped_character, X});
find_delimiter_and_unescape(Before, Delim, [Delim | After]) -> {Before, After};
find_delimiter_and_unescape(Before, Delim, [X | Tail]) -> find_delimiter_and_unescape(Before ++ [X], Delim, Tail);
find_delimiter_and_unescape(Before, Delim, []) -> throw({lexer_error, unterminated_string, Delim, Before}).
