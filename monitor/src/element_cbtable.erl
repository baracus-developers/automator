-module(element_cbtable).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, cbtable).

render_body(R) ->
    #table{ class=R#cbtable.class,
	    rows=[
		  #tablerow{cells=R#cbtable.header},
		  #bind {data=R#cbtable.data,
			 map=R#cbtable.map,
			 transform=fun alternate_color/2,
			 body=#tablerow {id=row,
					 cells=R#cbtable.rowspec
					}
			}
		 ]
	  }.

render_element(R) ->
    Panel = #panel{class="cbtable", body=render_body(R)},
    element_panel:render_element(Panel).

-record(acc, {color, row}).

%%% ALTERNATE BACKGROUND COLORS %%%
alternate_color(DataRow, []) ->
    alternate_color(DataRow, #acc{color=odd, row=1});

alternate_color([Id, Status]=DataRow, Acc) ->
    Row = Acc#acc.row,
    {Next, Class} = case Acc#acc.color of
       even -> {odd, evenrow};
       odd -> {even, oddrow}
    end,

    {DataRow, Acc#acc{row=Row+1, color=Next}, [{row@class, Class}]}.
