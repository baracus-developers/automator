-module (index).
-behavior(gen_event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("hostinfo.hrl").
-include_lib("wf_elements.hrl").

main() -> #template { file="./monitor/site/templates/bare.html" }.

title() -> "Welcome to Cloud-Builder".

toolbar() ->
    [ {"/images/Search.png", "/images/Search-bw.png", "Monitor"} ].

body() ->
    [
     #panel{ class="titlebar",
	     body=[
		   #panel{body=#image{ image="/images/cloudbuilder-logo.png"}},
		   #panel{body=#gbar{tabs=toolbar(), postback=selected}}
		  ]
	   },
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

async_init() ->
    comet_event_server:start_link(),
    async_loop().

async_loop() ->
    receive
	'INIT' -> ok;
	{relay, Event} ->
	    wf:update(table, render_table(get_data())),
	    wf:flush()
    end,
    async_loop().

event({selected, Label}) ->
    io:format("Selected: ~p~n", [Label]);
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
    
