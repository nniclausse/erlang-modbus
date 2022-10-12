%%% -*- erlang-indent-level: 8;indent-tabs-mode: nil -*-
%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus-tcp devices on an ethernet network

-module(modbus_device).
-author('Caleb Tennis <caleb.tennis@gmail.com>').

%% Public API
-export([
	connect/3,
	disconnect/1,
	read_coils/3,
	read_inputs/3,
	read_ireg/3,
	read_hreg/3,
	read_memory/2,
	read_memory/3,
        write_coil/3,
	write_hreg/3,
	write_hregs/3
]).

%% Internal API (gen_server)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("modbus.hrl").
-behavior(gen_server).

-define(TIMEOUT, 3000).
-define(RETRIES, 2).

%% @doc Function to connect with the modbus device.
%% @end
-spec connect(Host::string(), Port::integer(), DeviceAddr::integer()) -> {ok, pid()} | {error, term()}.
connect(Host, Port, DeviceAddr) ->
	gen_server:start(modbus_device, [Host, Port, DeviceAddr],[{timeout, ?TIMEOUT}]).

%% @doc Function to disconnect the modbus device.
%% @end
-spec disconnect(Pid::pid()) -> ok.
disconnect(Pid) ->
	gen_server:cast(Pid, stop).

%% @doc Function to request coils from the modbus device.
%% @end
-spec read_coils(Pid::pid(), Start::integer(), Offset::integer()) -> integer() | [integer()].
read_coils(Pid, Start, Offset) ->
	gen_server:call(Pid, {read_coils, Start, Offset}).

%% @doc Function to request inputs from the modbus device.
%% @end
-spec read_inputs(Pid::pid(), Start::integer(), Offset::integer()) -> integer() | [integer()].
read_inputs(Pid, Start, Offset) ->
	gen_server:call(Pid, {read_inputs, Start, Offset}).

%% @doc Function to request holding registers from the modbus device.
%% @end
-spec read_hreg(Pid::pid(), Start::integer(), Offset::integer()) -> integer() | [integer()].
read_hreg(Pid, Start, Offset) ->
	gen_server:call(Pid, {read_hreg, Start, Offset}).

%% @doc Function to request input registers from the modbus device.
%% @end
-spec read_ireg(Pid::pid(), Start::integer(), Offset::integer()) -> integer() | [integer()].
read_ireg(Pid, Start, Offset) ->
	gen_server:call(Pid, {read_ireg, Start, Offset}).

-spec write_coil(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_coil(Pid, Start, Value) ->
        gen_server:call(Pid, {write_coil, Start, Value}).

%% @doc Function to write data on holding register from the modbus device.
%% @end
-spec write_hreg(Pid::pid(), Start::integer(), Value::integer()) -> term().
write_hreg(Pid, Start, Value) ->
	gen_server:call(Pid, {write_hreg, Start, Value }).

-spec write_hregs(Pid::pid(), Start::integer(), Values::[integer()]) -> term().
write_hregs(Pid, Start, Values) ->
	gen_server:call(Pid, {write_hregs, Start, Values}).

%% @doc Function to request a memory position from the modbus device.
%% @end
-spec read_memory(Pid::pid(), string(), Offset::integer()) -> number() | [number()].
read_memory(Pid, "%MD" ++ PosNum, Offset) ->
	[Line, Word] = string:tokens(PosNum, "."),
	Reg = erlang:list_to_integer(Line) * 32768 + erlang:list_to_integer(Word) *2,
	Result = gen_server:call(Pid, {read_hreg, Reg, Offset *2}),
	words_to_float(Result);

read_memory(Pid, "%MW" ++ PosNum, Offset) ->
	[Line, Word] = string:tokens(PosNum, "."),
	Reg = erlang:list_to_integer(Line) * 32768 + erlang:list_to_integer(Word),
	gen_server:call(Pid, {read_hreg, Reg, Offset});

read_memory(Pid, "%MB0." ++ PosNum, Offset) ->
	Reg = erlang:list_to_integer(PosNum),
	gen_server:call(Pid, {read_raw, Reg *8, Offset *8});
	
read_memory(Pid, "%MX0." ++ PosNum, Offset) ->
	[Word, Bit] = string:tokens(PosNum, "."),
	Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
	gen_server:call(Pid, {read_coils, Reg, Offset}).

%% @doc Function to request a list of memory positions from the modbus device.
%% @end
-spec read_memory(Pid::pid(), list()) -> [{string(), number()}].
read_memory(Pid, "%M" ++ _ = MemPosition) ->
	read_memory(Pid, MemPosition, 1);

read_memory(Pid, List) ->
	NewList  = lists:foldl( fun(Elem, Acc) ->
		case Elem of
			"%MD0." ++ PosNum ->
				Reg = erlang:list_to_integer(PosNum) *32,
				[{Reg, Elem} | Acc];
			"%MW0." ++ PosNum ->
				Reg = erlang:list_to_integer(PosNum) *16,
				[{Reg, Elem} | Acc];
			"%MB0." ++ PosNum ->
				Reg = erlang:list_to_integer(PosNum) *8,
				[{Reg, Elem} | Acc];
			"%MX0." ++ PosNum ->
				[Word, Bit] = string:tokens(PosNum, "."),
				Reg = erlang:list_to_integer(Word) * 8 + erlang:list_to_integer(Bit),
				[{Reg, Elem} | Acc]
		end
	end, [], List),

	ReqList = lists:foldl( fun(Elem, [{RegAcc, OffsetAcc, MemAcc} |Acc]) ->
		NewRegAcc = RegAcc + OffsetAcc,
		case Elem of
			{NewRegAcc, "%MD0." ++ _ = MemPosition} ->
				[{RegAcc, OffsetAcc +32, [MemPosition |MemAcc]} | Acc];
			{NewRegAcc, "%MW0." ++ _ = MemPosition} ->
				[{RegAcc, OffsetAcc +16, [MemPosition |MemAcc]} | Acc];
			{NewRegAcc, "%MB0." ++ _ = MemPosition} ->
				[{RegAcc, OffsetAcc +8, [MemPosition |MemAcc]} | Acc];
			{NewRegAcc, "%MX0." ++ _ = MemPosition} ->
				case MemAcc of
					[["%MX0." ++_ | _] = MxAcc |MemAccTail] when length(MxAcc) < 8 ->
						[{RegAcc, OffsetAcc +1, [[MemPosition |MxAcc] |MemAccTail]} | Acc];
					_ ->
						[{RegAcc, OffsetAcc +1, [[MemPosition] |MemAcc]} | Acc]
				end;
			{Reg, "%MD0." ++ _ = MemPosition} ->
				[{Reg, 32, [MemPosition]}, {RegAcc, OffsetAcc, MemAcc} | Acc];
			{Reg, "%MW0." ++ _ = MemPosition} ->
				[{Reg, 16, [MemPosition]}, {RegAcc, OffsetAcc, MemAcc} | Acc];
			{Reg, "%MB0." ++ _ = MemPosition} ->
				[{Reg, 8, [MemPosition]}, {RegAcc, OffsetAcc, MemAcc} | Acc];
			{Reg, "%MX0." ++ _ = MemPosition} ->
				[{Reg, 1, [[MemPosition]]}, {RegAcc, OffsetAcc, MemAcc} | Acc]
		end
	end, [{0, 0, []}], lists:usort(NewList)),

	lists:foldl( fun(Elem, Acc) ->
		case Elem of 
			{0, 0, []} ->
				Acc;
			{Reg, Offset, MemList} ->
				Data = gen_server:call(Pid, {read_raw, Reg, Offset}),
				BinaryData = erlang:list_to_binary(Data),
				{ResultList, _} = lists:foldl( fun(Mem, {MemAcc, DataAcc}) ->
					case Mem of
						"%MD0." ++ _ ->
							<<Result:32/float, DataTail/binary>> = DataAcc,
							{[{Mem, Result} |MemAcc], DataTail};
						"%MW0." ++ _ ->
							<<Result:16/integer, DataTail/binary>> = DataAcc,
							{[{Mem, Result} |MemAcc], DataTail};
						"%MB0." ++ _ ->
							<<Result:8/integer, DataTail/binary>> = DataAcc,
							{[{Mem, Result} |MemAcc], DataTail};
						["%MX0." ++ _ |_] = MxList ->
							<<Result:8/integer, DataTail/binary>> = DataAcc,
							MxResultList = lists:reverse(erlang:integer_to_list(Result, 2)),
							{FinalMxList, _} = lists:foldl( fun(MxElem, {MxAcc, MxResultAcc}) ->
								case MxResultAcc of
									[MxH | MxT] ->
										{[{MxElem, erlang:list_to_integer([MxH])} |MxAcc], MxT};
									[] ->
										{[{MxElem, 0} |MxAcc], []}
								end
							end, {[], MxResultList}, lists:reverse(MxList)),
							{MemAcc ++ FinalMxList, DataTail}
					end
				end, {Acc, BinaryData}, lists:reverse(MemList)),
				ResultList
		end
	end, [], ReqList).


%% Internal API (gen_server)
%% ===== ===== ===== ===== ===== ===== ===== ===== =====

init([Host, Port, DeviceAddr]) ->
	Retval = gen_tcp:connect(Host, Port, [{active,false}, {packet, 0}]),

	case Retval of
		{ok, Sock} ->
			State = #tcp_request{sock = Sock, address = DeviceAddr, host=Host, port=Port},
			{ok, State};
		{error,ErrorType} ->
			{stop, {error, ErrorType}}
	end.

handle_call({read_coils, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_COILS,
		start = Start,
		data = Offset
	},
	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),

	FinalData = case lists:split(Offset, bytes_to_bits(Data)) of
		{[Result], _} -> Result;
		{Result, _} -> Result
	end,

	{reply, FinalData, NewState2};

handle_call({read_inputs, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_INPUTS,
		start = Start,
		data = Offset
	},
	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),

	FinalData = case lists:split(Offset, bytes_to_bits(Data)) of
		{[Result], _} -> Result;
		{Result, _} -> Result
	end,

	{reply, FinalData, NewState2};


handle_call({read_hreg, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_HREGS,
		start = Start,
		data = Offset
	},
	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),

	FinalData = bytes_to_words(Data),

	{reply, FinalData, NewState2};

handle_call({read_ireg,Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_IREGS,
		start = Start,
		data = Offset
	},
	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),

	FinalData = bytes_to_words(Data),

	{reply, FinalData, NewState2};

handle_call({read_raw, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_COILS,
		start = Start,
		data = Offset
	},
	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),

	{reply, Data, NewState2};

handle_call({write_coil, Start, Value}, _From, State) ->
        NewState = State#tcp_request{
                     tid = State#tcp_request.tid +1,
                     function = ?FC_WRITE_COIL,
                     start = Start, data = Value
                    },
        {ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),
	{reply, Data, NewState2};

handle_call({write_hreg, Start, OrigData}, _From, State) when is_integer(OrigData) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_WRITE_HREG,
		start = Start,
		data = OrigData
	},
	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),
	{reply, bytes_to_words(Data), NewState2};

handle_call({write_hregs, Start, OrigData}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_WRITE_HREGS,
		start = Start,
		data = OrigData
	},

	{ok, Data, NewState2} = send_and_receive_with_retry(NewState, ?RETRIES),

	FinalData = bytes_to_words(Data),

	{reply, FinalData, NewState2}.


handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_From, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_, State) ->
	gen_tcp:close(State#tcp_request.sock),
	ok.

code_change(_OldVsn, State, _Extra) -> 
	 {ok, State}.


%%% %%% -------------------------------------------------------------------
%% Util
%%% %%% -------------------------------------------------------------------

send_and_receive_with_retry(State, 0) ->
    {error, too_many_retries, State};
send_and_receive_with_retry(State=#tcp_request{address=DeviceAddr, host=Host, port=Port}, Retry) ->
    try send_and_receive(State) of
        {ok, Data } -> {ok, Data, State}
    catch
            error:{badmatch, {error, closed}} ->
                    {ok, NewState} = init([Host, Port, DeviceAddr]),
                    send_and_receive_with_retry(NewState, Retry-1);
            Error:Reason ->
                    {Error,Reason,State}
    end.

send_and_receive(State) ->

	ok = modbus:send_request_message(State),
	ok = modbus:get_response_header(State),
	{ok, _Data} = modbus:get_response_data(State).

%% @private
%% @doc Function to convert bytes to bits.
%% @end
-spec bytes_to_bits(Bytes::list()) -> float().
bytes_to_bits(Bytes) when is_integer(Bytes) ->
	Bits = erlang:integer_to_list(Bytes, 2),
	List = lists:foldl( fun(Elem, Acc) ->
		[erlang:list_to_integer([Elem]) |Acc]
	end, [], Bits),
	List ++ lists:duplicate(8 - length(List), 0);

bytes_to_bits(Bytes) ->
	bytes_to_bits(Bytes, []).

%% @hidden
bytes_to_bits([], Acc) ->
	Acc;
bytes_to_bits([Byte | MoreBytes], Acc) ->
	bytes_to_bits(MoreBytes, Acc ++ bytes_to_bits(Byte)).

%% @private
%% @doc Function to convert bytes to words.
%% @end
-spec bytes_to_words(Bytes::list()) -> float().
bytes_to_words(Bytes) ->
	bytes_to_words(Bytes,[]).

%% @hidden
bytes_to_words([],[Acc])->
	Acc;  
bytes_to_words([],Acc)->
	Acc;  
bytes_to_words([Byte1, Byte2 | Tail], Acc) ->
	<<Value:16/integer>> = <<Byte1:8, Byte2:8>>,
	bytes_to_words(Tail,Acc ++ [Value]).

%% @private
%% @doc Function to convert words to a float number.
%% @end
-spec words_to_float(List::list()) -> float().
words_to_float(List) ->
	words_to_float(List, []).

%% @hidden
words_to_float([], [Acc]) ->
	Acc;
words_to_float([], Acc) ->
	Acc;
words_to_float([H1, H2 | Tail], Acc) ->
	<<Value:32/float>> = <<H1:16, H2:16>>,
	words_to_float(Tail, Acc ++ [Value]).

