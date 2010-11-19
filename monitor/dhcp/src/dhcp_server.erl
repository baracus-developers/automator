%%%-------------------------------------------------------------------
%%% Based on dhcp_server.erl, by Ruslan Babayev <ruslan@babayev.com>
%%%-------------------------------------------------------------------
-module(dhcp_server).
-include_lib("epcap/include/epcap_net.hrl").
-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("dhcp.hrl").

-define(SERVER, ?MODULE).
-define(DHCP_SERVER_PORT, 67).
-define(DHCP_CLIENT_PORT, 68).
-define(ETHER_BROADCAST, {255,255,255,255,255,255}). 
-define(IPV4HDRLEN, 20).
-define(UDPHDRLEN, 8).

-record(state, {rxsocket, txsocket, mac, ifindex,
		server_id, next_server, bootfile}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ServerId, NextServer, BootFile) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [ServerId, NextServer, BootFile], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([ServerId, NextServer, BootFile]) ->
    Intf = compute_intf(ServerId),
    Port = ?DHCP_SERVER_PORT,

    {ok, RxSockFD} = procket:listen(Port, [{protocol, udp}, {type, dgram},
                                         {interface, Intf}]),
    {ok, TxSocket} = packet:socket(),
    Options = [binary, {broadcast, true}, {fd, RxSockFD}],
    case gen_udp:open(Port, Options) of
	{ok, RxSocket} ->
	    error_logger:info_msg("Starting DHCP server..."),
	    {ok, #state{rxsocket = RxSocket,
			txsocket = TxSocket,
			mac = packet:macaddress(RxSockFD, Intf), 
			ifindex = packet:ifindex(RxSockFD, Intf),
			server_id = ServerId,
			next_server = NextServer,
			bootfile = BootFile}};
	{error, Reason} ->
	    error_logger:error_msg("Cannot open udp port ~w",
				   [?DHCP_SERVER_PORT]),
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({udp, _Socket, _IP, _Port, Packet}, State) ->
    DHCP = dhcp_lib:decode(Packet),
    case optsearch(?DHO_DHCP_MESSAGE_TYPE, DHCP) of
	{value, MsgType} ->
	    handle_dhcp(MsgType, DHCP, State);
	false ->
	    ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    error_logger:logfile(close),
    gen_udp:close(State#state.rxsocket),
    procket:close(State#state.txsocket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% The DHCP message handler
%%%-------------------------------------------------------------------
handle_dhcp(?DHCPDISCOVER, D, State) ->
    error_logger:info_msg("DHCPDISCOVER from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)]),
    ClientId = get_client_id(D),
    Gateway = D#dhcp.giaddr,
    RequestedIP = get_requested_ip(D),
    case dhcp_alloc:reserve(ClientId, Gateway, RequestedIP) of
	{ok, IP, Options} ->
	    send_offer(State, D, IP, Options);
	{error, Reason} ->
	    error_logger:error_msg(Reason)
    end;
handle_dhcp(?DHCPREQUEST, D, State) ->
    ClientId = get_client_id(D),
    error_logger:info_msg("DHCPREQUEST from ~s ~s ~s",
			  [fmt_clientid(D), fmt_hostname(D), fmt_gateway(D)]),
    case client_state(D) of
	{selecting, ServerId} ->
	    case {ServerId, State#state.server_id} of
		{X, X} ->
		    IP = get_requested_ip(D),
		    case dhcp_alloc:allocate(ClientId, IP) of
			{ok, IP, Options} ->
			    send_ack(State, D, IP, Options);
			{error, Reason} ->
			    error_logger:error_msg(Reason)
		    end;
		_ ->
		    %% Client selected someone else, do nothing
		    ok
	    end;
	{init_reboot, RequestedIP} ->
	    Gateway = D#dhcp.giaddr,
	    case dhcp_alloc:verify(ClientId, Gateway, RequestedIP) of
		{ok, IP, Options} ->
		    send_ack(State, D, IP, Options);
		noclient ->
		    error_logger:error_msg("Client ~s has no current bindings",
					   [fmt_clientid(D)]);
		{error, Reason} ->
		    send_nak(State, D, Reason)
	    end;
	{ClientIs, IP} when ClientIs == renewing; ClientIs == rebinding ->
	    case dhcp_alloc:extend(ClientId, IP) of
		{ok, IP, Options} ->
		    send_ack(State, D, IP, Options);
		{error, Reason} ->
		    send_nak(State, D, Reason)
	    end
    end;
handle_dhcp(?DHCPDECLINE, D, _State) ->
    IP = get_requested_ip(D),
    error_logger:info_msg("DHCPDECLINE of ~s from ~s ~s",
			  [fmt_ip(IP), fmt_clientid(D), fmt_hostname(D)]),
    dhcp_alloc:decline(IP);
handle_dhcp(?DHCPRELEASE, D, _State) ->
    ClientId = get_client_id(D),
    error_logger:info_msg("DHCPRELEASE of ~s from ~s ~s ~s",
			  [fmt_ip(D#dhcp.ciaddr), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)]),
    dhcp_alloc:release(ClientId, D#dhcp.ciaddr);
handle_dhcp(?DHCPINFORM, D, State) ->
    Gateway = D#dhcp.giaddr,
    IP = D#dhcp.ciaddr,
    error_logger:info_msg("DHCPINFORM from ~s", [fmt_ip(IP)]), 
    case dhcp_alloc:local_conf(Gateway) of
	{ok, Opts} ->
	    %% No Lease Time (RFC2131 sec. 4.3.5)
	    OptsSansLease = lists:keydelete(?DHO_DHCP_LEASE_TIME, 1, Opts),
	    send_ack(State, D, IP, OptsSansLease);
	{error, Reason} ->
	    error_logger:error_msg(Reason)
    end;
handle_dhcp(MsgType, _D, _State) ->
    error_logger:error_msg("Invalid DHCP message type ~p", [MsgType]).

client_state(D) when is_record(D, dhcp) ->
    case optsearch(?DHO_DHCP_SERVER_IDENTIFIER, D) of
	{value, ServerId} ->
	    {selecting, ServerId};
	false ->
	    case optsearch(?DHO_DHCP_REQUESTED_ADDRESS, D) of
		{value, RequestedIP} ->
		    {init_reboot, RequestedIP};
		false ->
		    case is_broadcast(D) of
			false ->
			    {renewing, D#dhcp.ciaddr};
			_ ->
			    {rebinding, D#dhcp.ciaddr}
		    end
	    end
    end.

send_offer(S, D, IP, Options) ->
    DHCPOffer = D#dhcp{
		  op = ?BOOTREPLY,
		  hops = 0,
		  secs = 0,
		  ciaddr = {0, 0, 0, 0},
		  yiaddr = IP,
		  siaddr = S#state.next_server,
		  file = S#state.bootfile,
		  options = [{?DHO_DHCP_MESSAGE_TYPE, ?DHCPOFFER},
			     {?DHO_DHCP_SERVER_IDENTIFIER, S#state.server_id} |
			     Options]},
    error_logger:info_msg("DHCPOFFER on ~s to ~s ~s ~s",
			  [fmt_ip(IP), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)]),
    send_dhcp(S, DHCPOffer).

send_ack(S, D, IP, Options) ->
    DHCPAck = D#dhcp{
		op = ?BOOTREPLY,
		hops = 0,
		secs = 0,
		yiaddr = IP,
		siaddr = S#state.next_server,
		file = S#state.bootfile,
		options = [{?DHO_DHCP_MESSAGE_TYPE, ?DHCPACK},
			   {?DHO_DHCP_SERVER_IDENTIFIER, S#state.server_id} |
			   Options]},
    error_logger:info_msg("DHCPACK on ~s to ~s ~s ~s",
			  [fmt_ip(IP), fmt_clientid(D),
			   fmt_hostname(D), fmt_gateway(D)]),
    send_dhcp(S, DHCPAck).

send_nak(S, D, Reason) ->
    DHCPNak = D#dhcp{
		op = ?BOOTREPLY,
		hops = 0,
		secs = 0,
		ciaddr = {0, 0, 0, 0},
		yiaddr = {0, 0, 0, 0},
		siaddr = {0, 0, 0, 0},
		flags = D#dhcp.flags bor 16#8000, %% set broadcast bit
		options = [{?DHO_DHCP_MESSAGE_TYPE, ?DHCPNAK},
			   {?DHO_DHCP_SERVER_IDENTIFIER, S#state.server_id},
			   {?DHO_DHCP_MESSAGE, Reason}]},
    error_logger:info_msg("DHCPNAK to ~s ~s ~s. ~s",
			  [fmt_clientid(D), fmt_hostname(D),
			   fmt_gateway(D), Reason]),
    send_dhcp(S, DHCPNak).

send_dhcp(S, D) ->
    {TargetAddr, DPort} = get_dest(D),

    {DMac, DAddr} = case TargetAddr of
		    broadcast -> {?ETHER_BROADCAST, {255,255,255,255}};
		    _ -> {D#dhcp.chaddr, TargetAddr}
		 end,

    Payload = dhcp_lib:encode(D),
    UDP = #udp{sport=?DHCP_SERVER_PORT,
	       dport=DPort,
	       ulen=size(Payload)+?UDPHDRLEN
	      },
    IP = #ipv4{saddr=S#state.server_id,
	       daddr=DAddr,
	       p=?IPPROTO_UDP,
	       len=UDP#udp.ulen+?IPV4HDRLEN
	      },

    UdpChecksum = epcap_net:makesum([IP, UDP, Payload]),
    EncodedUDP = epcap_net:udp(UDP#udp{sum=UdpChecksum}),
    EncodedIP = epcap_net:ipv4(IP#ipv4{sum=epcap_net:makesum(IP)}),
    Ether = epcap_net:ether(#ether{dhost=mac_to_binary(DMac),
				   shost=mac_to_binary(S#state.mac),
				   type=?ETH_P_IP}),

    packet:send(S#state.txsocket, S#state.ifindex,
		list_to_binary([Ether, EncodedIP, EncodedUDP, Payload])).

%%% Behaviour is described in RFC2131 sec. 4.1
get_dest(D) when is_record(D, dhcp) ->
    IP = case D#dhcp.giaddr of
	     {0, 0, 0, 0} ->
		 case D#dhcp.ciaddr of
		     {0, 0, 0, 0} ->
			 case is_broadcast(D) of
			     true -> broadcast;
			     _    -> D#dhcp.yiaddr
			 end;
		     CiAddr -> CiAddr
		 end;
	     GiAddr -> GiAddr
	 end,
    Port = case D#dhcp.giaddr of
	       {0, 0, 0, 0} -> ?DHCP_CLIENT_PORT;
	       _            -> ?DHCP_SERVER_PORT
	   end,
    {IP, Port}.

is_broadcast(D) when is_record(D, dhcp) ->
    (D#dhcp.flags bsr 15) == 1.

optsearch(Option, D) when is_record(D, dhcp) ->
    case lists:keysearch(Option, 1, D#dhcp.options) of
	{value, {Option, Value}} ->
	    {value, Value};
	false ->
	    false
    end.
    
get_client_id(D) when is_record(D, dhcp) ->
    case optsearch(?DHO_DHCP_CLIENT_IDENTIFIER, D) of
        {value, ClientId} ->
	    ClientId;
	false ->
	    D#dhcp.chaddr
    end.

get_requested_ip(D) when is_record(D, dhcp) ->
    case optsearch(?DHO_DHCP_REQUESTED_ADDRESS, D) of
        {value, IP} ->
	    IP;
	false ->
	    {0, 0, 0, 0}
    end.

fmt_clientid(D) when is_record(D, dhcp) ->
    fmt_clientid(get_client_id(D));
fmt_clientid([_T, E1, E2, E3, E4, E5, E6]) ->
    fmt_clientid({E1, E2, E3, E4, E5, E6});
fmt_clientid({E1, E2, E3, E4, E5, E6}) ->
    lists:flatten(
      io_lib:format("~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b:~2.16.0b",
	     [E1, E2, E3, E4, E5, E6])).

fmt_gateway(D) when is_record(D, dhcp) ->
    case D#dhcp.giaddr of
	{0, 0, 0, 0} -> [];
	IP           -> lists:flatten(io_lib:format("via ~s", [fmt_ip(IP)]))
    end.

fmt_hostname(D) when is_record(D, dhcp) ->
    case optsearch(?DHO_HOST_NAME, D) of
        {value, Hostname} ->
            lists:flatten(io_lib:format("(~s)", [Hostname]));
	false ->
	    []
    end.

fmt_ip({A1, A2, A3, A4}) ->
    io_lib:format("~w.~w.~w.~w", [A1, A2, A3, A4]).

mac_to_binary({M1, M2, M3, M4, M5, M6}) ->
    <<M1, M2, M3, M4, M5, M6>>.

compute_intf(ServerId) ->
    {ok, Intfs} = inet:getiflist(),
    F = fun(Intf) ->
		{ok, [{addr, Addr}]} = inet:ifget(Intf, [addr]),
		Addr
	end,
    Addrs = dict:from_list([{F(Intf), Intf} || Intf <- Intfs]),
    case dict:find(ServerId, Addrs) of
	{ok, Intf} -> Intf;
	_ -> throw({nomatch, ServerId})
    end.
	     
