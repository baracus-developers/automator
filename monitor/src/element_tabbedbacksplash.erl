-module(element_tabbedbacksplash).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

-record(tabbedpanel, {id=wf:temp_id(), tab, panel}).
-record(controls, {main=wf:temp_id(), tabs=wf:temp_id(), panel=wf:temp_id()}).
-record(state, {controls=#controls{}, record, panels=dict:new(), current}).

reflect() -> record_info(fields, tabbedbacksplash).

render_panel(State, Key) ->
    {ok, Panel} = dict:find(Key, State#state.panels),
    Panel#tabbedpanel.panel.

render_cell(State, Key, Default) ->
    TabState = case Default of
		Key -> selected;
		_ -> unselected
	    end,

    {ok, Panel} = dict:find(Key, State#state.panels),
    Tab = Panel#tabbedpanel.tab,

    #panel{id=Panel#tabbedpanel.id, class=tabcell, body=Tab#tab{state=TabState}}.

process_panel(State, [{Text, Body} | T ]) ->
    Controls = State#state.controls,

    Tab = #tab{	text=Text,
		delegate=?MODULE,
		postback={selected, Controls#controls.main, Text}
	       },
    Panel = #tabbedpanel{tab=Tab, panel=Body},

    Panels = dict:store(Text, Panel, State#state.panels),
    process_panel(State#state{panels=Panels}, T);
process_panel(State, []) ->
    State.

render_element(R) ->
    [{First, _} | _ ] = R#tabbedbacksplash.panels,
    Default = wf:coalesce([R#tabbedbacksplash.default, First]),
    State = process_panel(#state{record=R, current=Default}, R#tabbedbacksplash.panels),

    Controls = State#state.controls,

    wf:state(Controls#controls.main, State),

    Cells = [render_cell(State, Text, Default) || {Text, _} <- R#tabbedbacksplash.panels ],
    Panel = #panel{class="tabbedbacksplash",
		   body=[
			 #panel{id=Controls#controls.tabs,
			        class="tabbar",
				body=Cells
			       },
			 #panel{class="outer-backsplash",
				body=#panel{id=Controls#controls.panel,
				            class="inner-backsplash",
					    body=render_panel(State, Default)
					   }
			       }
			]
		  },
    element_panel:render_element(Panel).

event({selected, Id, Selected}) ->
    State = wf:state(Id),
    Current = State#state.current,
    event({selected, Current, Selected, State});

event({selected, Current, Next, State}) when Current =:= Next ->
    ok;
event({selected, Current, Next, State}) ->
    Controls = State#state.controls,

    {ok, Panel} = dict:find(Current, State#state.panels),
    Tab = Panel#tabbedpanel.tab,

    wf:update(Panel#tabbedpanel.id, Tab#tab{state=unselected}),
    wf:update(Controls#controls.panel, render_panel(State, Next)),

    wf:state(Controls#controls.main, State#state{current=Next}),
    R = State#state.record,

    Module = wf:coalesce([R#tabbedbacksplash.delegate, wf_context:page_module()]),

    case R#tabbedbacksplash.postback of
	undefined -> ok;
	selected ->
	    Module:event({selected, Next});
	Postback ->
	    Module:event(Postback)
    end.
