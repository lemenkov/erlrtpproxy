%%%----------------------------------------------------------------------
%%% Copyright (c) 2011 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the names of its contributors may be used to endorse or promote
%%% products derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(gen_rtp_socket).
-author('lemenkov@gmail.com').

-behaviour(gen_server).
-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("../include/common.hrl").

% Milliseconds
-define(RTP_TIME_TO_LIVE, 150000).
% Milliseconds
-define(INTERIM_UPDATE, 30000).

-record(state, {
		parent,
		fd,
		transport,
		proto,
		ipf,
		portf,
		ipt,
		portt,
		started,
		lastseen,
		alive,
		weak,
		symmetric,
		tref,
		neighbour = null
	}
).

start(Args) ->
	gen_server:start(?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ([Parent, Transport, Proto, Parameters]) ->
	% FIXME allow explicitly set address (IP and Port) for both RTP and RTCP sockets
	SockParams = proplists:get_value(sockparams, Parameters, []),
	{ok, Fd} = gen_udp:open(0, SockParams),
	{ok, TRef} = timer:send_interval(?INTERIM_UPDATE, interim_update),
	Weak = proplists:get_value(weak, Parameters, false),
	Symmetric = proplists:get_value(symmetric, Parameters, true),

	% Get probable IP and port
	{Ip, Port} = ensure_addr(
		proplists:get_value(ip, Parameters, null),
		proplists:get_value(port, Parameters, null),
		proplists:get_value(external, Parameters, true)
	),

	{ok, #state{
			parent = Parent,
			fd = Fd,
			transport = Transport,
			proto = Proto,
			ipf = null,
			portf = null,
			ipt = Ip,
			portt = Port,
			started = false,
			lastseen = null,
			alive = false,
			weak = Weak,
			symmetric = Symmetric,
			tref = TRef
		}
	};
init ([Parent, Fd, Transport, Proto, Parameters]) ->
	{ok, TRef} = timer:send_interval(?INTERIM_UPDATE, interim_update),
	Weak = proplists:get_value(weak, Parameters, false),
	Symmetric = proplists:get_value(symmetric, Parameters, true),

	% Get probable IP and port
	{Ip, Port} = ensure_addr(
		proplists:get_value(ip, Parameters, null),
		proplists:get_value(port, Parameters, null),
		proplists:get_value(external, Parameters, true)
	),

	{ok, #state{
			parent = Parent,
			fd = Fd,
			transport = Transport,
			proto = Proto,
			ipf = null,
			portf = null,
			ipt = Ip,
			portt = Port,
			started = false,
			lastseen = null,
			alive = false,
			weak = Weak,
			symmetric = Symmetric,
			tref = TRef
		}
	}.

handle_call({addr, Ip, Port}, {Pid, _}, #state{symmetric = true, neighbour = Pid} = State) ->
	{reply, ok, State#state{ipt = Ip, portt = Port}};
handle_call({addr, Ip, Port}, {Pid, _}, #state{symmetric = false, neighbour = Pid} = State) ->
	{reply, ok, State};

handle_call({neighbour, Pid}, _From, State) when is_pid(Pid) ->
	{reply, ok, State#state{neighbour = Pid}}.

handle_cast({Proto, _Pkts}, #state{proto = Proto, ipt = null, portt = null} = State) ->
	{noreply, State};
handle_cast({Proto, Pkts}, #state{fd = Fd, proto = Proto, ipt = Ip, portt = Port} = State) ->
	Msg = Proto:encode(Pkts),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};

handle_cast({update, Parameters}, State) ->
	Weak = proplists:get_value(weak, Parameters, false),
	Symmetric = proplists:get_value(symmetric, Parameters, true),

	% Get probable IP and port
	{Ip, Port} = ensure_addr(
		proplists:get_value(ip, Parameters, null),
		proplists:get_value(port, Parameters, null),
		proplists:get_value(external, Parameters, true)
	),

	case State#state.started of
		true ->
			{noreply, State#state{
					weak = Weak,
					symmetric = Symmetric
				}
			};
		_ ->
			{noreply, State#state{
					ipt = Ip,
					portt = Port,
					weak = Weak,
					symmetric = Symmetric
				}
			}
	end;

handle_cast(stop, #state{tref = TRef} = State) ->
	% Run 30-second Timer instead
	% We shouldn't stop right here. Instead we need to capture all
	% remaining packets if any.
	timer:cancel(TRef),
	{ok, TRef2} = timer:send_interval(?INTERIM_UPDATE, stop),
	{noreply, State#state{tref = TRef2}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{parent = Parent, fd = Fd, transport = Transport, tref = TRef}) ->
	timer:cancel(TRef),
	Type = case Transport of
		udp -> gen_udp;
		tcp -> gen_tcp;
		sctp -> gen_sctp
	end,
	Type:close(Fd),
	gen_server:cast(Parent, {stop, self(), Reason}),
	ok.

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, proto = Proto, started = true, weak = true, symmetric = Symmetric, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = Proto:decode(Msg),
		process_data(Proto, Pkts, Parent, Neighbour),
		{noreply, State#state{ipt = Ip, portt = Port, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, proto = Proto, started = true, weak = false, ipf = Ip, portf = Port, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = Proto:decode(Msg),
		process_data(Proto, Pkts, Parent, Neighbour),
		{noreply, State#state{lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, _Ip, _Port, _Msg}, #state{fd = Fd, started = true, weak = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	?ERR("Disallow data from strange source", []),
	{noreply, State};

handle_info({udp, Fd, Ip, Port, Msg}, #state{parent = Parent, proto = Proto, started = false, neighbour = Neighbour} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = Proto:decode(Msg),
		gen_server:cast(Parent, {start, self()}),
		gen_server:call(Neighbour, {addr, Ip, Port}),
		process_data(Proto, Pkts, Parent, Neighbour),

		{noreply, State#state{started = true, ipf = Ip, portf = Port, lastseen = now(), alive = true}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info(ping, State) ->
	{noreply, State};

handle_info(stop, State) ->
	% Final stop
	{stop, stop, State};

handle_info(interim_update, #state{started = false} = State) ->
	% Discard - we didn't start yet
	{noreply, State};
handle_info(interim_update, #state{parent = Parent, alive = true} = State) ->
	gen_server:cast(Parent, {interim_update, self()}),
	{noreply, State#state{alive = false}};
handle_info(interim_update, #state{alive = false} = State) ->
	{stop, timeout, State}.

%%
%% Private functions
%%

ensure_addr (null, _, _) ->
	{null, null};
ensure_addr (GuessIp, GuessPort, IsInternal) ->
	% Get probable IP and port
	% FIXME consider IsInternal param
	{Ip, Port} = case rtpproxy_utils:is_rfc1918(GuessIp) of
		true -> {null, null};
		_ -> {GuessIp, GuessPort}
	end.

process_data(rtp, Pkts, Parent, Neighbour) ->
	gen_server:cast(Neighbour, {rtp, Pkts});
process_data(rtcp, Pkts, Parent, Neighbour) ->
	gen_server:cast(Parent, {rtcp, Pkts, self(), Neighbour}).
