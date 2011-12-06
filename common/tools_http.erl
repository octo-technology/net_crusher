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
-module(tools_http).

-export([
  reopen_socket/1,
  open_socket/3,
  close_socket/1,
  close_socket/2,
  send_socket/2,
  send_socket_binary/2,

  create_http_get/2,
  create_http_post/3,

  read_http_data_binary/1,
  read_http_data_decode/1,

  post_http/4,
  get_http/3,

  extract_header/2,
  extract_headers/2,
  parse_url/1,
  make_url/3,

  start_simple_http_monitor/0,
  start_void_http_monitor/0,
  replace_http_monitor/1
  ]).

socket_table_name() ->
  list_to_atom("socket" ++ pid_to_list(self())).

create_socket_table() ->
  case ets:info(socket_table_name()) of
    undefined -> ets:new(socket_table_name(), [named_table, public]);
    _ -> noop
  end.

store_socket(Key, Socket) ->
  ets:insert(socket_table_name(), {Key, Socket}).

delete_socket(Socket) ->
  ets:match_delete(socket_table_name(), {'_', Socket}).

get_socket(Key) ->
  case ets:lookup(socket_table_name(), Key) of
      [] -> undefined;
      [{Key, Socket} | _] -> Socket
  end.

reopen_socket(Socket) ->
  [[{Protocol, ServerAddr, ServerPort}]] = ets:match(socket_table_name(), {'$1', Socket}),
  close_socket(Socket),
  open_socket(Protocol, ServerAddr, ServerPort).

open_socket(Protocol, ServerAddr, ServerPort) ->
  create_socket_table(),
  case get_socket({Protocol, ServerAddr, ServerPort}) of
    undefined -> Opts = [binary, {packet, 0}, {reuseaddr, true}, {keepalive, true}, {active, false}],
                 %% Opts = [binary, {packet, 0}, {ip, ClientIP}, {reuseaddr, false}, {active, false}],
                 {ok, Socket} = gen_tcp:connect(ServerAddr, ServerPort, Opts, infinity),
                 whereis(http_monitoring) ! {socket_connected},
                 OutputSocket = case Protocol of
                                    http -> Socket;
                                    https ->
                                        {ok, SocketSSL} = ssl:connect(Socket, []),
                                        SocketSSL
                                end,
                 store_socket({Protocol, ServerAddr, ServerPort}, OutputSocket),
                 %% io:format("Create socket to ~s ~w\n", [ServerAddr, ServerPort]),
                 OutputSocket;
    S -> S
  end.

close_socket(Sock, Headers) ->
  case extract_header(Headers, 'Connection') of
   {ok, "keep-alive"} -> noop;
   _ -> close_socket(Sock)
  end.

close_socket(Sock) ->
  %io:format("Closing ~p\n", [Sock]),
  whereis(http_monitoring) ! {socket_closed},
  delete_socket(Sock),
  close(Sock).

close(Sock) when is_tuple(Sock) ->
  ssl:shutdown(Sock, read_write),
  ssl:close(Sock);
close(Sock) ->
  gen_tcp:shutdown(Sock, read_write),
  gen_tcp:close(Sock).

send_socket(Sock, Data) ->
  send_socket_binary(Sock, list_to_binary(Data)).

send(Sock, Data) when is_tuple(Sock) -> ssl:send(Sock, Data);
send(Sock, Data) -> gen_tcp:send(Sock, Data).

send_socket_binary(Sock, Data) ->
  %io:format("Send Http Request ~p\n<<\n~s\n>>\n", [Sock, Data]),
  case send(Sock, [Data]) of
    ok ->
      whereis(http_monitoring) ! {request_sent},
      {ok, Sock};
    {error, closed} ->
      closed;
    {error, enotconn} ->
      NewSocket = reopen_socket(Sock),
      send_socket_binary(NewSocket, Data);
    Other ->
      io:format("Other:~w\n",[Other]),
      other
  end.

dump_headers([{Name, Value} | Tail]) ->
  io_lib:format("~s: ~s\r\n", [Name, Value]) ++ dump_headers(Tail);
dump_headers([]) -> "".

add_default_headers(Headers) ->
  [{"Accept", "*/*"}, {"User-Agent", "Erlang"} | Headers].
  
create_http_post(Path, Headers, PostData) ->
  HeadersStr = dump_headers(add_default_headers([{"Content-Length", integer_to_list(string:len(PostData))} | Headers])),
  io_lib:format("POST ~s HTTP/1.1\r\n~s\r\n~s", [Path, HeadersStr, PostData]).

create_http_get(Path, Headers) ->
  HeadersStr = dump_headers(add_default_headers(Headers)),
  io_lib:format("GET ~s HTTP/1.1\r\n~s\r\n", [Path, HeadersStr]).

recv(Sock, Data) when is_tuple(Sock) -> ssl:recv(Sock, Data);
recv(Sock, Data) -> gen_tcp:recv(Sock, Data).

read_http_data_binary(Sock) ->
  case recv(Sock, 0) of
    {ok, B} ->
      {ok, B};
    {error, closed} ->
      throw({socket_closed, Sock});
    Other ->
      throw({socket_error, Sock, Other})
  end.

read_http_data_decode(Sock) ->
  read_http_data_decode(Sock, <<"">>).

read_http_data_decode(Sock, Buffer) ->
  case erlang:decode_packet(http, Buffer, []) of
    {ok, {http_response, _, Code, Msg}, Rest} ->
      case decode_headers(Sock, Rest) of
        {ok, Headers, Body} ->
          {ok, Sock, Code, Msg, Headers, decode_body(Sock, Headers, Body)};
        _ ->
          other
      end;
    {more, _} ->
      {ok, Buffer2} = read_http_data_binary(Sock),
      read_http_data_decode(Sock, <<Buffer/binary, Buffer2/binary>>)
  end.

extract_header(Headers, Name) ->
  case extract_headers(Headers, Name) of
    [] -> not_found;
    [A | _] -> {ok, A}
  end.

extract_headers([{Name, V} | Tail], Name) -> [V | extract_headers(Tail, Name)];
extract_headers([{_, _} | Tail], Name) -> extract_headers(Tail, Name);
extract_headers([], _) -> [].

decode_body(Sock, Headers, Body) ->
  case extract_header(Headers, 'Content-Length') of
    not_found -> case extract_header(Headers, 'Transfer-Encoding') of
        not_found -> Body;
        {ok, "chunked"} -> read_chunk(Sock, Body);
        {ok, V} -> throw({http_error, wrong_chunked_value, V})
      end;
    {ok, K} ->
      Len = list_to_integer(K),
      read_body(Sock, Len, Body)
  end.

read_chunk(Sock, Body) when byte_size(Body) == 0 ->
  {ok, Buffer} = read_http_data_binary(Sock),
  read_chunk(Sock, Buffer);
read_chunk(Sock, Body) ->
  [SizeBinary, Left] = binary:split(Body, <<"\r\n">>),
  Size = tools:dehex(binary_to_list(SizeBinary)),
  %io:fwrite("Read chunk ~w\n", [Size]),
  {ChunkWithCRLF, NewBody} = read_chunk(Sock, Left, Size + 2),
  K = byte_size(ChunkWithCRLF),
  {Chunk, <<"\r\n">>} = {binary:part(ChunkWithCRLF, 0, K - 2), binary:part(ChunkWithCRLF, K - 2, 2)},
  case Size of
    0 ->
      %io:fwrite("End ~p\n", [NewBody]),
      Chunk;
    _ ->
      NextChunk = read_chunk(Sock, NewBody),
      <<Chunk/binary, NextChunk/binary>>
  end.

read_chunk(Sock, Body, Size) when byte_size(Body) < Size ->
  {ok, Buffer} = read_http_data_binary(Sock),
  read_chunk(Sock, <<Body/binary, Buffer/binary>>, Size);
read_chunk(_, Body, Size) ->
  {binary:part(Body, 0, Size),binary:part(Body, Size, byte_size(Body) - Size)}.

read_body(_, Len, Body) when byte_size(Body) == Len -> Body;
read_body(Sock, Len, Body) ->
  {ok, Buffer} = read_http_data_binary(Sock),
  read_body(Sock, Len, <<Body/binary, Buffer/binary>>).

decode_headers(Sock, Buffer) ->
  decode_headers(Sock, Buffer,[]).
decode_headers(Sock, Buffer, Acc) ->
  case erlang:decode_packet(httph, Buffer, []) of
    {ok, {http_header, _N, Name, _, V}, Rest} ->
      decode_headers(Sock, Rest, [{Name, V} | Acc]);
    {ok, http_eoh, Rest} ->
      {ok, Acc, Rest};
    {more, _} ->
      {ok, Buffer2} = read_http_data_binary(Sock),
      decode_headers(Sock, <<Buffer/binary, Buffer2/binary>>, Acc)
  end.

post_http(Sock, Path, Headers, PostData) ->
  case send_socket(Sock, create_http_post(Path, Headers, PostData)) of
    {ok, Socket} ->
      read_http_data_decode(Socket);
    Other ->
      Other
  end.

get_http(Sock, Path, Headers) ->
  case send_socket(Sock, create_http_get(Path, Headers)) of
    {ok, Socket} ->
      read_http_data_decode(Socket);
    Other -> Other
  end.

parse_url(Url) ->
  case re:run(Url, "^(http.?)://([^:]+):(\\d+)(/.*)$") of
    {match,[{_,_},{A1,A2},{B1,B2},{C1, C2},{D1,D2}]} -> {ok, list_to_atom(string:substr(Url, A1 + 1, A2)), string:substr(Url, B1 + 1, B2), list_to_integer(string:substr(Url, C1 + 1, C2)), string:substr(Url, D1 + 1, D2)};
    nomatch ->
      case re:run(Url, "^(http.?)://([^/]+)(/.*)$") of
        {match,[{_,_},{A1,A2},{B1,B2},{C1,C2}]} ->
          Protocol = list_to_atom(string:substr(Url, A1 + 1, A2)),
          Port = case Protocol of
            http -> 80;
            https -> 443
          end,
          {ok, Protocol, string:substr(Url, B1 + 1, B2), Port, string:substr(Url, C1 + 1, C2)};
        nomatch -> {wrong_url, Url}
      end
  end.

make_url(Protocol, ServerAddr, ServerPort) ->
  {Start, Port} = case Protocol of
    http -> {"http://",
    case ServerPort of
      80 -> "";
      _ -> ":" ++ integer_to_list(ServerPort)
    end};
    https -> {"https://",
    case ServerPort of
      443 -> "";
      _ -> ":" ++ integer_to_list(ServerPort)
    end}
  end,
  Start ++ ServerAddr ++ Port.

loop(F) ->
  F(),
  loop(F).

replace_http_monitor(F) ->
  case whereis(http_monitoring) of
    undefined -> noop;
    Pid ->
      exit(Pid, 0),
      tools:stop_process(http_monitoring)
  end,
  %% io:fwrite("Starting http monitor~n"),
  register(http_monitoring, spawn(fun() -> loop(fun() -> receive Msg -> F(Msg) end end) end)).

start_simple_http_monitor() ->
  replace_http_monitor(fun(Msg) -> io:format("[Http monitor] ~p\n", [Msg]) end).

start_void_http_monitor() ->
  replace_http_monitor(fun(_) -> noop end).
