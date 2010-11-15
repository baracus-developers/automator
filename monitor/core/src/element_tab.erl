-module(element_tab).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

-record(controls, {mainpanel=wf:temp_id(),
		   text=wf:temp_id()}).
-record(state, {selected}).

reflect() -> record_info(fields, tab).

postback(Event, Controls, R) ->
    #event{type=Event, postback={Event, Controls, R}, delegate=?MODULE}.

effect_speed() -> 500.
    
select(Controls) ->
    wf:wire(Controls#controls.mainpanel,
	    #add_class{class="tab_selected", speed=effect_speed()}),
    wf:wire(Controls#controls.text,
	    #add_class{class="tabtext_selected", speed=effect_speed()}).
 	
render_body(R) ->
 
    Controls = #controls{},

    State = case R#tab.state of
		selected ->
		    select(Controls),
		    #state{selected = true};
		unselected ->
		    #state{selected = false}
    end,

    MainId = Controls#controls.mainpanel,

    wf:wire(MainId, postback(click, Controls, R)),

    wf:state(MainId, State),

    #panel{id=MainId,
	   class="tab",
	   body=[
		 #panel{id=Controls#controls.text,
			class="tabtext", body=#span{text=R#tab.text}} 
		]
	  }.
	
render_element(R) ->
    Panel = #panel{body=render_body(R)},
    element_panel:render_element(Panel).

event({click, Controls, R}) ->
    State = wf:state(Controls#controls.mainpanel),
    case State#state.selected of
	false ->
	    case R#tab.postback of
		undefined -> ok;
		Postback ->
		    Module = wf:coalesce([R#tab.delegate,
					  wf_context:page_module()]),
		    Module:event(Postback)
	    end,

	    select(Controls),
	    wf:state(Controls#controls.mainpanel, State#state{selected = true});

	true -> ok
    end.


