%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ser_proto_test).

%%
%% Tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("../src/common.hrl").

parse_cmd_v_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_V,
			cookie="24390_0",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234}
		}, ser_proto:parse("24390_0 V", {127,0,0,1}, 1234)).

parse_cmd_vf_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_VF,
			cookie="24393_1",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			params="20050322"
		}, ser_proto:parse("24393_1 VF 20050322", {127,0,0,1}, 1234)).

parse_cmd_vf_unknown_version_test() ->
	?assertThrow(
		{error_syntax,"Unknown version: 20070101"},
		ser_proto:parse("24393_1 VF 20070101", {127,0,0,1}, 1234)).

parse_cmd_u_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="24393_4",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50cd69210b8c36b-0ecf0120",addr={{192,0,43,10}, 27686}, rtcpaddr={{192,0,43,10}, 27687}},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true}
			]
		}, ser_proto:parse("24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.0.43.10 27686 0003e30cc50cd69210b8c36b-0ecf0120;1", {127,0,0,1}, 1234)).

parse_cmd_u_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,9}, 16432},rtcpaddr={{192,0,43,9}, 16433}},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true}
			]
		}, ser_proto:parse("438_41061 Uc8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.0.43.9 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_3_1_transcode_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Uc8,0,2,4,18,96,97,98,100,101t4 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_3_2_transcode_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,5}, 16432},rtcpaddr={{192,0,43,5}, 16433}},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4c8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.0.43.5 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

%parse_cmd_u_3_3_transcode_incompatible_test() ->
%	?assertThrow(
%		{error_syntax,"Requested transcoding to incompatible codec"},
%		ser_proto:parse("438_41061 Ut5c8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).
parse_cmd_u_3_3_transcode_incompatible_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1"},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true}
			]
		}, ser_proto:parse("438_41061 Ut5c8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

%parse_cmd_u_3_4_transcode_no_codecs_test() ->
%	?assertThrow(
%		{error_syntax,"Requested transcoding but no codecs are available"},
%		ser_proto:parse("438_41061 Ut1 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).
parse_cmd_u_3_4_transcode_no_codecs_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1"},
			params=[
				{direction, {external, external}},
				{symmetric,true}
			]
		}, ser_proto:parse("438_41061 Ut1 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_4_zeroes_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="5958_7",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="ffe0100519df4bc1bbd2f8e18309ca8a",
			mediaid=2,
			from=#party{tag="186f101b0e04481ea045517edb93b62d",addr={{192,0,43,7}, 19268},rtcpaddr={{192,0,43,7}, 19269}},
			params=[
				{codecs,[
						{'H261',90000,0},
						{'H263',90000,0}
					]
				},
				{direction, {external, external}},
				{repacketize,30},
				{symmetric,true}
			]
		}, ser_proto:parse("5958_7 UZ30" ++ [0,0,0] ++ "c34,31 ffe0100519df4bc1bbd2f8e18309ca8a 192.0.43.7 19268 186f101b0e04481ea045517edb93b62d;2", {127,0,0,1}, 1234)).

parse_cmd_u_5_proto_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,4}, 16432},rtcpaddr={{192,0,43,4}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1c8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.0.43.4 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_6_0_internal_to_internal_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,168,5,3}, 16432},rtcpaddr={{192,168,5,3}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {internal, internal}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1iic8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_6_1_internal_to_external_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,168,5,3}, 16432},rtcpaddr={{192,168,5,3}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {internal, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1iec8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_6_2_external_to_internal_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, internal}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1eic8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_6_3_external_to_external_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1eec8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_6_4_internal_to_internal_single_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,168,5,3}, 16432},rtcpaddr={{192,168,5,3}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {internal, internal}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1ic8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_6_5_external_to_external_single_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}, proto=tcp},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1ec8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_7_both_symmetric_and_asymmetric_test() ->
	?assertThrow(
		{error_syntax,"Both symmetric and asymmetric modifiers are defined"},
		ser_proto:parse("438_41061 Ut4p1asc8,0,2,4,18,96,97,98,100,101 e12ea248-94a5e885@192.168.5.3 192.168.5.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_8_acc_start_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}, proto=tcp},
			params=[
				{acc, start},
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1c8,0,2,4,18,96,97,98,100,101v0 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_9_acc_interim_update_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}, proto=tcp},
			params=[
				{acc, interim_update},
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1c8,0,2,4,18,96,97,98,100,101v0v1 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_10_acc_stop_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",addr={{192,0,43,3}, 16432},rtcpaddr={{192,0,43,3}, 16433}, proto=tcp},
			params=[
				{acc, stop},
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1c8,0,2,4,18,96,97,98,100,101v0v1v2 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_11_wrong_ipv4_test() ->
	?assertThrow(
		{error_syntax,"Wrong IP"},
		ser_proto:parse("24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 892.168.0.100 27686 0003e30cc50cd69210b8c36b-0ecf0120;1", {127,0,0,1}, 1234)).

parse_cmd_u_12_wrong_port_test() ->
	?assertThrow(
		{error_syntax,"Wrong port"},
		ser_proto:parse("24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.168.0.100 627686 0003e30cc50cd69210b8c36b-0ecf0120;1", {127,0,0,1}, 1234)).

parse_cmd_u_13_wrong_mediaid_test() ->
	?assertThrow(
		{error_syntax,"Wrong MediaID"},
		ser_proto:parse("24393_4 Uc0,8,18,101 0003e30c-c50c00f7-123e8bd9-542f2edf@192.168.0.100 192.168.0.100 27686 0003e30cc50cd69210b8c36b-0ecf0120;foo", {127,0,0,1}, 1234)).

parse_cmd_u_14_discard_rfc1918_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1",proto=tcp},
			params=[
				{acc, stop},
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1c8,0,2,4,18,96,97,98,100,101v0v1v2 e12ea248-94a5e885@192.168.5.3 192.168.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).

parse_cmd_u_15_discard_non_rfc1918_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="438_41061",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="e12ea248-94a5e885@192.168.5.3",
			mediaid=1,
			from=#party{tag="6b0a8f6cfc543db1o1", proto=tcp},
			params=[
				{acc, stop},
				{codecs,[
						{'PCMU',8000,1},
						2,
						{'G723',8000,1},
						{'PCMA',8000,1},
						{'G729',8000,1},
						96,
						97,
						98,
						100,
						101
					]
				},
				{direction, {internal, external}},
				{symmetric,true},
				{transcode,{'G723',8000,1}}
			]
		}, ser_proto:parse("438_41061 Ut4p1iec8,0,2,4,18,96,97,98,100,101v0v1v2 e12ea248-94a5e885@192.168.5.3 192.0.43.3 16432 6b0a8f6cfc543db1o1;1", {127,0,0,1}, 1234)).


parse_cmd_l_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="413_40797",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="452ca314-3bbcf0ea@192.168.0.2",
			mediaid=1,
			from=#party{tag="8d11d16a3b56fcd588d72b3d359cc4e1",addr={{192,0,43,4}, 17050},rtcpaddr={{192,0,43,4}, 17051}},
			to=#party{tag="e4920d0cb29cf52o0"},
			params=[
				{codecs,[
						{'PCMU',8000,1},
						100,
						101
					]
				},
				{direction, {external, external}},
				{symmetric,true}
			]
		}, ser_proto:parse("413_40797 Lc0,101,100 452ca314-3bbcf0ea@192.168.0.2 192.0.43.4 17050 e4920d0cb29cf52o0;1 8d11d16a3b56fcd588d72b3d359cc4e1;1", {127,0,0,1}, 1234)).

parse_cmd_l_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_U,
			cookie="418_41111",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="a68e961-5f6a75e5-356cafd9-3562@192.168.100.6",
			mediaid=1,
			from=#party{tag="60753eabbd87fe6f34068e9d80a9fc1c",addr={{192,168,100,4}, 18756},rtcpaddr={{192,168,100,4}, 18757}},
			to=#party{tag="1372466422"},
			params=[
				{codecs,[
						{'PCMA',8000,1},
						100,
						101
					]
				},
				{direction, {internal, internal}},
				{symmetric,true}
			]
		}, ser_proto:parse("418_41111 LIc8,101,100 a68e961-5f6a75e5-356cafd9-3562@192.168.100.6 192.168.100.4 18756 1372466422;1 60753eabbd87fe6f34068e9d80a9fc1c;1", {127,0,0,1}, 1234)).

parse_cmd_d_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_D,
			cookie="441_40922",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="2498331773@192.168.1.37",
			mediaid=0,
			from=#party{tag="8edccef4eb1a16b8cef7192b77b7951a"}
		}, ser_proto:parse("441_40922 D 2498331773@192.168.1.37 8edccef4eb1a16b8cef7192b77b7951a", {127,0,0,1}, 1234)).

parse_cmd_d_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_D,
			cookie="437_40882",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="7adc6214-268583a6-1b74a438-3548@192.168.100.6",
			mediaid=0,
			from=#party{tag="1372466422"},
			to=#party{tag="9c56ba15bd794082ce6b166dba6c9c2"}
		}, ser_proto:parse("437_40882 D 7adc6214-268583a6-1b74a438-3548@192.168.100.6 1372466422 9c56ba15bd794082ce6b166dba6c9c2", {127,0,0,1}, 1234)).


parse_cmd_r_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_C,
			cookie="393_6",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e348-e21901f6-29cc58a1-379f3ffd@192.168.0.1",
			mediaid=0,
			from=#party{tag="0003e348e219767510f1e38f-47c56231"},
			to=null,
			params=[
				{filename, default}
			]
		}, ser_proto:parse("393_6 R 0003e348-e21901f6-29cc58a1-379f3ffd@192.168.0.1 0003e348e219767510f1e38f-47c56231", {127,0,0,1}, 1234)).

parse_cmd_r_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_C,
			cookie="32711_5",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c016a-35dc4387-58a65654@192.168.0.100",
			mediaid=0,
			from=#party{tag="eb1f1ca7e74cf0fc8a81ea331486452a"},
			to=#party{tag="0003e30cc50ccbed0342cc8d-0bddf550"},
			params=[
				{filename, default}
			]
		}, ser_proto:parse("32711_5 R 0003e30c-c50c016a-35dc4387-58a65654@192.168.0.100 eb1f1ca7e74cf0fc8a81ea331486452a 0003e30cc50ccbed0342cc8d-0bddf550", {127,0,0,1}, 1234)).

parse_cmd_p_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_P,
			cookie="2154_5",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50ccc9f743d4fa6-38d0bd14"},
			to=null,
			params=[
				{codecs,"session"},
				{filename,"/var/run/tmp/hello_uac.wav"},
				{playcount, 20}
			]
		}, ser_proto:parse("2154_5 P20 0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100 /var/run/tmp/hello_uac.wav session 0003e30cc50ccc9f743d4fa6-38d0bd14;1", {127,0,0,1}, 1234)).

parse_cmd_p_2_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_P,
			cookie="1389_5",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c016d-46bbcf2e-6369eecf@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50ccc5416857d59-357336dc"},
			to=#party{tag="28d49e51a95d5a31d09b31ccc63c5f4b"},
			params=[
				{codecs,"session"},
				{filename,"/var/tmp/rtpproxy_test/media/01.wav"},
				{playcount, 10}
			]
		}, ser_proto:parse("1389_5 P10 0003e30c-c50c016d-46bbcf2e-6369eecf@192.168.0.100 /var/tmp/rtpproxy_test/media/01.wav session 0003e30cc50ccc5416857d59-357336dc;1 28d49e51a95d5a31d09b31ccc63c5f4b;1", {127,0,0,1}, 1234)).

parse_cmd_p_3_wrong_playcount_test() ->
	?assertThrow(
		{error_syntax,"Wrong PlayCount"},
		ser_proto:parse("1389_5 Phello 0003e30c-c50c016d-46bbcf2e-6369eecf@192.168.0.100 /var/tmp/rtpproxy_test/media/01.wav session 0003e30cc50ccc5416857d59-357336dc;1 28d49e51a95d5a31d09b31ccc63c5f4b;1", {127,0,0,1}, 1234)).

parse_cmd_s_1_test() ->
	?assertEqual(
		#cmd{
			type=?CMD_S,
			cookie="2154_6",
			origin=#origin{type=ser,pid=self(),ip={127,0,0,1},port=1234},
			callid="0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100",
			mediaid=1,
			from=#party{tag="0003e30cc50ccc9f743d4fa6-38d0bd14"}
		}, ser_proto:parse("2154_6 S 0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100 0003e30cc50ccc9f743d4fa6-38d0bd14;1", {127,0,0,1}, 1234)).

parse_cmd_unknown_test() ->
	?assertThrow(
		{error_syntax,"Unknown command"},
		ser_proto:parse("2154_6 Z 0003e30c-c50c0171-35b90751-013a3ef6@192.168.0.100 0003e30cc50ccc9f743d4fa6-38d0bd14;1", {127,0,0,1}, 1234)).

encode_ok_test() ->
	?assertEqual("438_41067 0\n", ser_proto:encode(#cmd{cookie="438_41067"}, ok)).

encode_ip_test() ->
	?assertEqual("8411_41413 41212 192.168.100.4\n", ser_proto:encode(#cmd{cookie="8411_41413"}, {{192,168,100,4},41212})).

encode_version_basic_test() ->
	?assertEqual("32031_1 20040107\n", ser_proto:encode(#cmd{cookie="32031_1"}, {version, "20040107"})).

encode_version_supported_test() ->
	?assertEqual("32031_3 1\n", ser_proto:encode(#cmd{cookie="32031_3"}, {supported, "20081224"})).

encode_error_syntax_test() ->
	?assertEqual("32098_3 E1\n", ser_proto:encode("32098_3 hello there - some invalid command string", {error, syntax})).

encode_error_software_test() ->
	?assertEqual("24393_4 E7\n", ser_proto:encode(#cmd{cookie="24393_4"}, {error, software})).

encode_error_notfound_test() ->
	?assertEqual("24393_4 E8\n", ser_proto:encode(#cmd{cookie="24393_4"}, {error, notfound})).

is_rfc1918_test_() ->
	[
		{"test for localhost",
			fun() -> ?assertEqual(true, ser_proto:is_rfc1918({127,1,2,3})) end
		},
		{"test for 10.x.x.x subnet",
			fun() -> ?assertEqual(true, ser_proto:is_rfc1918({10,0,127,3})) end
		},
		{"test for 172.x.x.x subnet",
			fun() -> ?assertEqual(true, ser_proto:is_rfc1918({172,16,127,3})) end
		},
		{"test for 192.168.x.x subnet",
			fun() -> ?assertEqual(true, ser_proto:is_rfc1918({192,168,127,3})) end
		},
		{"test #1 for non-RFC1918 subnet",
			fun() -> ?assertEqual(false, ser_proto:is_rfc1918({172,168,127,3})) end
		},
		{"test #2 for non-RFC1918 subnet",
			fun() -> ?assertEqual(false, ser_proto:is_rfc1918({192,169,127,3})) end
		},
		{"test for non-IPv4 subnet",
			fun() -> ?assertThrow({error, "Not a valid IPv4 address"}, ser_proto:is_rfc1918("::1")) end
		}
	].

-endif.

