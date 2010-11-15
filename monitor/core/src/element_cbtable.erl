-module(element_cbtable).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, cbtable).

-record(state, {id=wf:temp_id(), rowids=[], record}).
-record(acc, {color, row, state}).


generate_ids(Count, State) when Count > 0 ->
    Ids = State#state.rowids,
    generate_ids(Count-1, State#state{rowids = Ids ++ [wf:temp_id()]});
generate_ids(Count, State) ->
    State.

render_body(R) ->
    RowCount = length(R#cbtable.data),
    State = generate_ids(RowCount, #state{record=R}),

    send(none, State),
    
    #table{ id=State#state.id,
	    class=R#cbtable.class,
	    rows=[
		  #tablerow{cells=R#cbtable.header},
		  #bind {data=R#cbtable.data,
			 map=R#cbtable.map,
			 acc=#acc{color=odd, row=1, state=State},
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

alternate_color(DataRow, Acc) ->
    Row = Acc#acc.row,
    State = Acc#acc.state,
    R = State#state.record,
    Id = lists:nth(Row, State#state.rowids),

    {Next, Class} = case Acc#acc.color of
       even -> {odd, evenrow};
       odd -> {even, oddrow}
    end,

    case R#cbtable.selectable of
	true ->
	    wf:wire(Id, #event{type=click, delegate=?MODULE,
			       postback={row_clicked, Row, Id, State}
			      }
		   );
	false -> ok
    end,

    {DataRow, Acc#acc{row=Row+1, color=Next},
     [{row@class, Class}, {row@id, Id}]}.

redraw(SelectedId, [Id | T]) when SelectedId =:= Id ->
    wf:wire(Id, #add_class{class=selectedrow, speed=500}),
    redraw(SelectedId, T);
redraw(SelectedId, [Id | T]) ->
    wf:wire(Id, #remove_class{class=selectedrow, speed=500}),
    redraw(SelectedId, T);
redraw(SelectedId, []) ->
    ok.

send(Index, State) ->
    R = State#state.record,
    Module = wf:coalesce([R#cbtable.delegate, wf_context:page_module()]),

    case R#cbtable.postback of
	undefined -> ok;
	Postback ->
	    Module:event({Postback, Index})
    end.    

event({row_clicked, Index, Id, State}) ->
    send(Index, State),
    redraw(Id, State#state.rowids).
