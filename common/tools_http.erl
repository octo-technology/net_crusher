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
  open_socket/3,
  close_socket/1,
  send_socket/2,
  send_socket_binary/2,

  create_http_get/2,
  create_http_post/3,
  create_http_put/3,

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

open_socket(Protcol, ServerAddr, ServerPort) ->
  Opts = [binary, {packet, 0}, {reuseaddr, true}, {keepalive, true}, {active, false}],
  %Opts = [binary, {packet, 0}, {ip, ClientIP}, {reuseaddr, false}, {active, false}],
  {ok, Socket} = gen_tcp:connect(ServerAddr, ServerPort, Opts, infinity),
  whereis(http_monitoring) ! {socket_connected},
  OutputSocket = case Protcol of
    http -> Socket;
    https ->
    {ok, SocketSSL} = ssl:connect(Socket, []),
    SocketSSL
  end,
  %io:format("Create socket to ~s ~w\n", [ServerAddr, ServerPort]),
  OutputSocket.

close_socket(Sock) ->
  %io:format("Closing ~p\n", [Sock]),
  whereis(http_monitoring) ! {socket_closed},
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
      ok;
    {error, closed} ->
      closed;
    Other ->
      io:format("Other:~w\n",[Other]),
      other
  end.

create_http_post(Path, Headers, PostData) ->
  io_lib:format("POST ~s HTTP/1.1\r\nContent-Length: ~w\r\n~sAccept: */*\r\nUser-Agent: erlang\r\n\r\n~s", [Path, string:len(PostData), Headers, PostData]).

create_http_get(Path, Headers) ->
  io_lib:format("GET ~s HTTP/1.1\r\n~sAccept: */*\r\nUser-Agent: erlang\r\n\r\n", [Path, Headers]).

create_http_put(Path, Headers, PostData) ->
  io_lib:format("PUT ~s HTTP/1.1\r\nContent-Length: ~w\r\n~sAccept: */*\r\nUser-Agent: erlang\r\n\r\n~s", [Path, string:len(PostData), Headers, PostData]).

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
          {ok, Code, Msg, Headers, decode_body(Sock, Headers, Body)};
        _ ->
          other
      end;
    {more, _} ->
      {ok, Buffer2} = case catch read_http_data_binary(Sock) of
        {socket_closed, _} -> io:format("Socket_closed: ~p", [Buffer]), throw({socket_closed, Sock});
                         X -> X
      end,
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
    ok ->
      read_http_data_decode(Sock);
    Other ->
      Other
  end.

get_http(Sock, Path, Headers) ->
  case send_socket(Sock, create_http_get(Path, Headers)) of
    ok ->
      read_http_data_decode(Sock);
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
