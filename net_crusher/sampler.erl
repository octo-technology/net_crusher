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
-module(sampler).

-export([start/2,
         stop/0,
         cmd_start_sampler/2,
         cmd_stop_sampler/0,
         int_get_new_sample/0,
         sampler/4]).

start(NbSamples, SampleRepartition) ->
  case lists:member(process_sampler, global:registered_names()) of
    true -> logger:cmd_log(2, "Sampler already started");
    _ ->
      logger:cmd_logf(1, "Starting sampler with ~p samples and a repartition ~p",
                      [NbSamples, SampleRepartition]),
      global:register_name(process_sampler,
                           runtime:spawn_with_monitor(node(), sampler, sampler,
                                                      [NbSamples, SampleRepartition, 1, 1]))
  end.

stop() ->
  tools:stop_process(process_sampler).

cmd_start_sampler(StrNbSamples, StrRepartition) ->
  start(list_to_integer(StrNbSamples),
        lists:map(fun(Str) -> list_to_integer(Str) end,
                  re:split(StrRepartition, ",", [{return, list}]))).

cmd_stop_sampler() -> stop().

int_get_new_sample() ->
  tools:sync_msg(global:whereis_name(process_sampler), sample_value, get_sample, {}).

sampler(NbSamples, SampleRepartition, CurrentSample, SampleRepartitionIndex) ->
  receive
    halt -> noop;
    {get_sample, Target, {}} ->
      sampler_(NbSamples, SampleRepartition, CurrentSample,
               SampleRepartitionIndex, Target)
  end.

sampler_(NbSamples, SampleRepartition, CurrentSample,
         SampleRepartitionIndex, Target) ->
  case SampleRepartitionIndex > length(SampleRepartition) of
    true -> sampler_(NbSamples, SampleRepartition, 1, 1, Target);
    _ ->
      CurrentSamplePercent = CurrentSample / NbSamples * 100,
      case lists:nth(SampleRepartitionIndex, SampleRepartition) of
        Percent when Percent < CurrentSamplePercent ->
          sampler_(NbSamples, SampleRepartition, 1,
                  SampleRepartitionIndex + 1, Target);
        _ ->
          Target ! {sample_value, SampleRepartitionIndex - 1},
          sampler(NbSamples, SampleRepartition, CurrentSample + 1,
                  SampleRepartitionIndex)
      end
  end.
