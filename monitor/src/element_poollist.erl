-module(element_poollist).
-export([reflect/0, render_element/1, event/1, get_selected/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

-record(entry, {pool, selected=false}).
-record(state, {entries=dict:new()}).

reflect() -> record_info(fields, poollist).

get_selected(Id) ->
    State = get_state(Id),
    Entries = dict:filter(fun(_, Entry) -> Entry#entry.selected end,
			  State#state.entries),
    [Entry#entry.pool || {_, Entry} <- dict:to_list(Entries)].

flatten(Pool, R, State) ->
    Entry = dict:fetch(Pool, State#state.entries),

    SelectedId = wf:temp_id(),
    [_, Zone] = string:tokens(atom_to_list(node()), "@"),

    [
     SelectedId,
     Entry#entry.selected,
     {select_toggle, R, SelectedId, {Zone, Pool}},
     Zone,
     Pool
    ].

render_body(R, State) ->
    {ok, Pools} = pools_server:enum(),

    F = fun(Pool) ->
		case dict:find(Pool, State#state.entries) of
		    {ok, Entry} -> {Pool, Entry};
		    _ -> {Pool, #entry{pool=Pool}}
		end
	end,

    Entries = dict:from_list([F(Pool) || Pool <- Pools]),
    UpdatedState = State#state{entries=Entries},

    Rows = [flatten(Pool, R, UpdatedState) || Pool <- Pools],

    [
     #cbtable{class="pool-list",
	      data=Rows,
	      map= [
		    selected@id,
		    selected@checked,
		    selected@postback,
		    zone@text,
		    pool@text
		   ],
	      header=[
		      #tableheader{body=[
					 #link{text="all",
					       delegate=?MODULE,
					       postback={select_all, R}},
					 " ",
					 #link{text="none",
					       delegate=?MODULE,
					       postback={select_none, R}}
					]
				  },
		      #tableheader { text="Zone" },
		      #tableheader { text="Pool" }
		     ],
	      rowspec=[
		       #tablecell { body=#checkbox{id=selected, delegate=?MODULE}},
		       #tablecell { id=zone },
		       #tablecell { id=pool }
		      ]
	     },
     #hidden{id=R#poollist.hid, text=wf:pickle(UpdatedState)}
    ].

render_element(R) ->
    Panel = #panel{ id=R#poollist.id,
		    anchor=R#poollist.anchor,
		    class=R#poollist.class,
		    body=render_body(R, #state{})
		  },
    element_panel:render_element(Panel).

get_state(R) when is_record(R, poollist) ->
    get_state(R#poollist.hid);
get_state(Id) ->
    wf:depickle(wf:q(Id)).    

set_state(R, State) ->
    wf:replace(R#poollist.hid, #hidden{id=R#poollist.hid, text=wf:pickle(State)}).

update_selections(R, Value) ->
    State = get_state(R),
    F = fun(_, Entry) -> Entry#entry{selected=Value} end,
    Entries = dict:map(F, State#state.entries),
    wf:update(R#poollist.id, render_body(R, State#state{entries=Entries})). 

event({select_all, R}) ->
    update_selections(R, true);

event({select_none, R}) ->
    update_selections(R, false);

event({select_toggle, R, Id, {Zone, Pool}}) ->
    Value = case wf:q(Id) of
		"on" -> true;
		_ -> false
	    end,

    State = get_state(R),

    F = fun(Entry) -> Entry#entry{selected=Value} end,
    Entries = dict:update(Pool, F, State#state.entries),

    set_state(R, State#state{entries=Entries});

event(_) ->
    ok.
