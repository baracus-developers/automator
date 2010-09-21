-module (index).
-behavior(gen_event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("hostinfo.hrl").

main() -> #template { file="./monitor/site/templates/bare.html" }.

title() -> "Welcome to Cloud-Builder".

body() ->
    [
     #panel{ class="titlebar", body=#image{ image="/images/cloudbuilder-logo.png"}},
     #container_12 { body=[
			   #grid_8 { alpha=true, prefix=2, suffix=2, omega=true, body=inner_body() }
			  ]}
    ].

get_host(Host) ->
    {ok, HostInfo} = hosts_server:get_hostinfo(Host),
    [Host, atom_to_list(HostInfo#hostinfo.state)].

get_data() ->
    {ok, Hosts} = hosts_server:enum(),
    [get_host(Host) || Host <- Hosts].

get_map() -> 
    [
        macLabel@text, 
        statusLabel@text
    ].

inner_body() -> 
    {ok, Pid} = wf:comet(fun() -> async_init() end),

    render_table(get_data()).

render_rows(Data) ->
    Map = get_map(),
    #bind { id=tableBinding,
	    data=Data,
	    map=Map,
	    transform=fun alternate_color/2,
	    body=#tablerow {
	      id=row,
	      cells=[
		     #tablecell { id=macLabel },
		     #tablecell { id=statusLabel }
		    ]
	     }
	  }.
	

render_table(Data) ->
    #table { id=table,
	     rows=[
		   #tablerow {cells=[
				      #tableheader { text="MAC" },
				      #tableheader { text="Status" }
				     ]},
		   render_rows(get_data())
		  ]}.

-record(acc, {color, row}).

subst(X) ->
    case X of
	$: -> $-;
	Else -> Else
    end.

row(Id) ->
    "row" ++ [subst(X) || X <- Id].

-record(state, {pid}).

async_init() ->
    gen_event:add_sup_handler(host_events, ?MODULE, [self()]),
    async_loop().

init([Pid]) ->
    {ok, #state{pid=Pid}}.

async_loop() ->
    receive
	'INIT' -> ok;
	update ->
	    wf:update(table, render_table(get_data())),
	    wf:flush()
    end,
    async_loop().

handle_event(_, State) ->
    Pid = State#state.pid,
    Pid ! update,

    {ok, State}.

event(_) -> ok.

reverse_class(Acc) ->
    case Acc#acc.color of
	even -> odd;
	odd -> even
    end.

%%% ALTERNATE BACKGROUND COLORS %%%
alternate_color(DataRow, []) ->
    alternate_color(DataRow, #acc{color=odd, row=1});

alternate_color([Id, Status]=DataRow, Acc) ->
    Row = Acc#acc.row,
    {Next, Class} = case Acc#acc.color of
	even -> {odd, evenrow};
	odd -> {even, oddrow}
    end,
    {DataRow, Acc#acc{row=Row+1, color=Next}, [{row@class, Class}, {row@id, row(Id)}]}.
    
