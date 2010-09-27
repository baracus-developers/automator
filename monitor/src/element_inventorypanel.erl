-module(element_inventorypanel).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include_lib("hostinfo.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventorypanel).

get_host(Host) ->
    {ok, HostInfo} = hosts_server:get_hostinfo(Host),
    [Host, atom_to_list(HostInfo#hostinfo.state)].

get_data() ->
    {ok, Hosts} = hosts_server:enum(),
    [get_host(Host) || Host <- Hosts].

render_rows(Data) ->
    Map = [
	   macLabel@text, 
	   statusLabel@text
	  ],

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

-record(acc, {color, row}).

subst(X) ->
    case X of
       $: -> $-;
       Else -> Else
    end.

row(Id) ->
    "row" ++ [subst(X) || X <- Id].

%%% ALTERNATE BACKGROUND COLORS %%%
alternate_color(DataRow, []) ->
    alternate_color(DataRow, #acc{color=odd, row=1});

alternate_color([Id, Status]=DataRow, Acc) ->
    Row = Acc#acc.row,
    {Next, Class} = case Acc#acc.color of
       even -> {odd, evenrow};
       odd -> {even, oddrow}
    end,
    {DataRow,
     Acc#acc{row=Row+1, color=Next},
     [{row@class, Class}, {row@id, row(Id)}]
    }.

render_table() ->
    #table{ rows=[
		  #tablerow {cells=[
				    #tableheader { text="MAC" },
				    #tableheader { text="Status" }
				   ]
			    },
		  render_rows(get_data())
		 ]
	  }.

handle_event(Id, Event) ->
    wf:update(Id, render_table()),
    wf:flush().

render_element(R) ->
    Id = wf:temp_id(),
    comet_event_relay:add_handler(host_events, Id,
				  fun(Event) -> handle_event(Id, Event) end),

    Panel = #panel{ body=#panel{ id=Id, body=render_table()}},
    element_panel:render_element(Panel).
