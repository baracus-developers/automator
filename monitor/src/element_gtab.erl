-module(element_gtab).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, gtab).

postback(Event, Id, R) ->
    #event{type=Event, postback={Event, Id, R}, delegate=?MODULE}.
	
render_body(Id, R, State) ->
 
    wf:wire(Id, postback(mouseover, Id, R)),
    wf:wire(Id, postback(mouseout, Id, R)),
    wf:wire(Id, postback(click, Id, R)),

    #panel{id=Id, body=render_body(R, State)}.

render_body(R, State) ->
    {Image, Class} = case {R#gtab.state, State} of
	{unselected, unselected} ->
			     {R#gtab.unselected_image, gtabtext};
	{unselected, selected} ->
			     {R#gtab.selected_image, gtabtext};
	{selected, _} ->
			     {R#gtab.selected_image, selected_gtabtext}
    end,

    [
     #panel{body=#image{image=Image}},
     #panel{body=#span{class=Class, text=R#gtab.text}} 
    ].
	
render_element(R) ->
    Panel = #panel{class=gtab, body=render_body(wf:temp_id(), R, unselected)},
    element_panel:render_element(Panel).

event({mouseover, Id, R}) ->
    wf:update(Id, render_body(R, selected));
event({mouseout, Id, R}) ->
    wf:update(Id, render_body(R, unselected));
event({click, Id, R}) ->
    case R#gtab.postback of
	undefined -> ok;
	Postback ->
	    Module = wf:coalesce([R#gtab.delegate, wf_context:page_module()]),
	    Module:event(Postback)
    end,
    wf:replace(Id, render_body(wf:temp_id(),
			       R#gtab{state=selected}, unselected)).


