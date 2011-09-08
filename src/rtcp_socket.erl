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

-module(rtcp_socket).
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
-include_lib("rtplib/include/rtcp.hrl").

-record(state, {
		parent,
		fd,
		transport,
		ipf,
		portf,
		ipt,
		portt,
		started,
		weak,
		symmetric
	}
).

start(Args) ->
	gen_server:start(?MODULE, Args, []).
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init ([Parent, Fd, Transport, Parameters]) ->
	process_flag(trap_exit, true),

	Weak = proplists:get_value(weak, Parameters, false),
	Symmetric = proplists:get_value(symmetric, Parameters, true),

	% Get probable IP and port
	{Ip, Port} = proplists:get_value(rtcp, Parameters, {null, null}),

	{ok, #state{
			parent = Parent,
			fd = Fd,
			transport = Transport,
			ipf = null,
			portf = null,
			ipt = Ip,
			portt = Port,
			started = false,
			weak = Weak,
			symmetric = Symmetric
		}
	}.

handle_call(_Call, _From, State) ->
	{stop, bad_call, State}.

handle_cast({rtcp, _Pkts}, #state{ipt = null, portt = null} = State) ->
	{noreply, State};
handle_cast({rtcp, Pkts}, #state{fd = Fd, ipt = Ip, portt = Port} = State) ->
	Msg = rtcp:encode(Pkts),
	% FIXME use Transport
	gen_udp:send(Fd, Ip, Port, Msg),
	{noreply, State};

handle_cast({update, Parameters}, State) ->
	Weak = proplists:get_value(weak, Parameters, false),
	Symmetric = proplists:get_value(symmetric, Parameters, true),

	% Get probable IP and port
	{Ip, Port} = proplists:get_value(rtcp, Parameters, {null, null}),

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

handle_cast(stop, State) ->
	% Final stop
	{stop, stop, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{parent = Parent, fd = Fd, transport = Transport}) ->
	% We must send RTCP bye here
	Type = case Transport of
		udp -> gen_udp;
		tcp -> gen_tcp;
		sctp -> gen_sctp
	end,
	Type:close(Fd),
	ok.

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, started = true, weak = true, symmetric = Symmetric} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtcp:decode(Msg),
		gen_server:cast(Parent, {rtcp, Pkts, self()}),
		{noreply, State#state{ipt = Ip, portt = Port}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;

handle_info({udp, Fd, Ip, Port, Msg}, #state{fd = Fd, parent = Parent, started = true, weak = false, ipf = Ip, portf = Port} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtcp:decode(Msg),
		gen_server:cast(Parent, {rtcp, Pkts, self()}),
		{noreply, State}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;
handle_info({udp, Fd, _Ip, _Port, _Msg}, #state{fd = Fd, started = true, weak = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	?ERR("Disallow data from strange source", []),
	{noreply, State};

handle_info({udp, Fd, Ip, Port, Msg}, #state{parent = Parent, started = false} = State) ->
	inet:setopts(Fd, [{active, once}]),
	try
		{ok, Pkts} = rtcp:decode(Msg),
		gen_server:cast(Parent, {rtcp, Pkts, self()}),
		{noreply, State#state{started = true, ipf = Ip, portf = Port}}
	catch
		_:_ -> rtp_utils:dump_packet(node(), self(), Msg),
		{noreply, State}
	end;


handle_info(_Info, State) ->
	{noreply, State}.
