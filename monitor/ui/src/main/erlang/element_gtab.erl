-module(element_gtab).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

-record(controls, {mainpanel=wf:temp_id(),
		   uimg=wf:temp_id(),
		   img=wf:temp_id(),
		   text=wf:temp_id()}).
-record(state, {selected}).

reflect() -> record_info(fields, gtab).

postback(Event, Controls, R) ->
    #event{type=Event, postback={Event, Controls, R}, delegate=?MODULE}.

effect_speed() -> 500.
    
select(Controls) ->
    wf:wire(Controls#controls.mainpanel,
	    #add_class{class="gtab_selected", speed=effect_speed()}),
    wf:wire(Controls#controls.text,
	    #add_class{class="gtabtext_selected", speed=effect_speed()}).
 	
render_body(R) ->
 
    Controls = #controls{},

    State = case R#gtab.state of
		selected ->
		    select(Controls),
		    #state{selected = true};
		unselected ->
		    wf:wire(Controls#controls.img, #hide{}),
		    #state{selected = false}
    end,

    MainId = Controls#controls.mainpanel,

    % disable mouseover for now
    %wf:wire(MainId, postback(mouseover, Controls, R)),
    %wf:wire(MainId, postback(mouseout, Controls, R)),
    wf:wire(MainId, postback(click, Controls, R)),

    wf:wire(Controls#controls.img,
	    #add_class{class="gtabimage_selected"}),

    wf:state(MainId, State),

    #panel{id=MainId,
	   body=[
		 #panel{id=Controls#controls.uimg,
                        class="gtabimage",
			body=#image{image=R#gtab.unselected_image}},
		 #panel{id=Controls#controls.img,
			class="gtabimage",
			body=#image{image=R#gtab.selected_image}},
		 #panel{id=Controls#controls.text,
			class="gtabtext", body=#span{text=R#gtab.text}} 
		]
	  }.
	
render_element(R) ->
    Panel = #panel{class="gtab", body=render_body(R)},
    element_panel:render_element(Panel).

event({mouseover, Controls, R}) ->
    State = wf:state(Controls#controls.mainpanel),
    case State#state.selected of
	false -> wf:wire(Controls#controls.img, #appear{ speed=effect_speed() });
	true -> ok
    end;
event({mouseout, Controls, R}) ->
    State = wf:state(Controls#controls.mainpanel),
    case State#state.selected of
	false -> wf:wire(Controls#controls.img, #fade{ speed=effect_speed() });
        true -> ok
    end;
event({click, Controls, R}) ->
    State = wf:state(Controls#controls.mainpanel),
    case State#state.selected of
	false ->
	    case R#gtab.postback of
		undefined -> ok;
		Postback ->
		    Module = wf:coalesce([R#gtab.delegate,
					  wf_context:page_module()]),
		    Module:event(Postback)
	    end,

	    wf:wire(Controls#controls.img, #appear{ speed=effect_speed() }),
	    select(Controls),

	    wf:state(Controls#controls.mainpanel, State#state{selected = true});

	true -> ok
    end.


