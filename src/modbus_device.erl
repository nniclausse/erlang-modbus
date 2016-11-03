%% @author Caleb Tennis <caleb.tennis@gmail.com>
%% @copyright 2010 Data Cave, Inc.  www.thedatacave.com
%% @version 0.9
%% @doc A way to interact with modbus devices on an ethernet network

-module(modbus_device).
-author('Caleb Tennis <caleb.tennis@gmail.com>').

%% Public API
-export([
	connect/3,
	disconnect/1,
	read_coils/3,
	read_inputs/3,
	read_ireg/3,
	read_hreg/3
	%read_mem/3
]).

%% Internal API (gen_server)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("modbus.hrl").
-behavior(gen_server).

%% @spec connect(Host:: string(), Port :: integer (), DeviceAddress :: integer () ) -> ok | pid()
%% @doc Connect to the remote TCP device
connect(Host, Port, DeviceAddr) ->
	{ok, Pid} = gen_server:start_link(modbus_device, [Host, Port, DeviceAddr],[]),
	Pid.

disconnect(Name) -> gen_server:call(Name, stop).

%% @doc Read from a coil.
read_coils(Device, Start, Offset) ->
	gen_server:call(Device, {read_coils, Start, Offset}).

%% @doc Read from a bit.
read_inputs(Device, Start, Offset) ->
	gen_server:call(Device, {read_inputs, Start, Offset}).

%% @doc Read from a holding register.
read_hreg(Device, Start, Offset) ->
	gen_server:call(Device, {read_hreg, Start, Offset}).

read_ireg(Device, Start, Offset) ->
	gen_server:call(Device, {read_ireg, Start, Offset}).

write_hreg(Device, Start, Value) ->
	gen_server:call(Device, {write_hreg, Start, Value }).


init([Host, Port, DeviceAddr]) ->
	Retval = gen_tcp:connect(Host, Port, [{active,false}, {packet, 0}]),

	case Retval of
		{ok, Sock} ->
			State = #tcp_request{sock = Sock, address = DeviceAddr},
			{ok, State, 5000};
		{error,ErrorType} ->
			{stop,{error,ErrorType}}
	end.

handle_call({read_coils, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_COILS,
		start = Start,
		data = Offset
	},
	{ok, Data} = send_and_receive(NewState),

	FinalData = case lists:split(Offset, bytes_to_bits(Data)) of
		{[Result], _} -> Result;
		{Result, _} -> Result
	end,

	{reply, FinalData, NewState, 5000};

handle_call({read_inputs, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_INPUTS,
		start = Start,
		data = Offset
	},
	{ok, Data} = send_and_receive(NewState),

	FinalData = case lists:split(Offset, bytes_to_bits(Data)) of
		{[Result], _} -> Result;
		{Result, _} -> Result
	end,

	{reply, FinalData, NewState, 5000};


handle_call({read_hreg, Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_HREGS,
		start = Start,
		data = Offset
	},
	{ok, Data} = send_and_receive(NewState),

	FinalData = bytes_to_words(Data),

	{reply, FinalData, NewState, 5000};

handle_call({read_ireg,Start, Offset}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_READ_IREGS,
		start = Start,
		data = Offset
	},
	{ok, Data} = send_and_receive(NewState),

	FinalData = bytes_to_words(Data),

	{reply, FinalData, NewState, 5000};

handle_call({write_hreg, Start, OrigData}, From, State) when is_integer(OrigData) ->
	handle_call({write_hreg, Start, [OrigData]}, From, State);

handle_call({write_hreg, Start, OrigData}, _From, State) ->
	NewState = State#tcp_request{
		tid = State#tcp_request.tid +1,
		function = ?FC_WRITE_HREGS,
		start = Start,
		data = OrigData
	},

	{ok, [_Address,_FunctionCode|Data]} = send_and_receive(NewState),

	[FinalData] = bytes_to_words(Data),

	{reply, FinalData, NewState, 5000};

handle_call(stop,_From,State) ->
	gen_tcp:close(State#tcp_request.sock),
	{stop, normal, stopped, State}.

handle_cast(_From,State) -> {noreply, State}.

% If we timeout, do a stop
handle_info(timeout,State) ->
	handle_call(stop,whocares,State),
	{stop, normal, State}.

terminate(_Reason,State) -> 
	handle_call(stop,whocares,State).

code_change(_OldVsn, State, _Extra) -> { ok, State }.

send_and_receive(State) ->

	ok = modbus:send_request_message(State),
	ok = modbus:get_response_header(State),
	{ok, _Data} = modbus:get_response_data(State).


% Take a list of modbus bytes, and convert it to a list of bits.
bytes_to_bits(Bytes) when is_integer(Bytes) ->
	Bits = erlang:integer_to_list(Bytes, 2),
	List = lists:foldl( fun(Elem, Acc) ->
						 Acc ++ [erlang:list_to_integer([Elem])]
				 end, [], Bits),
	List ++ lists:duplicate(8 - length(List), 0);

bytes_to_bits(Bytes) ->
	bytes_to_bits(Bytes, []).

bytes_to_bits([], Acc) ->
	Acc;
bytes_to_bits([Byte | MoreBytes], Acc) ->
	bytes_to_bits(MoreBytes, Acc ++ bytes_to_bits(Byte)).


% Take a list of modbus bytes, and convert it to a list of words.
bytes_to_words(Bytes) ->
	bytes_to_words(Bytes,[]).

bytes_to_words([],[Acc])->
	Acc;  
bytes_to_words([],Acc)->
	Acc;  
bytes_to_words([Byte1, Byte2 | Tail], Acc) ->
	<<Value:16/integer>> = <<Byte1:8, Byte2:8>>,
	bytes_to_words(Tail,Acc ++ [Value]).

