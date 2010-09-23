-module(element_gbar).
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

-record(state, {record, tabs=dict:new(), currenttab}).

reflect() -> record_info(fields, gbar).

render_tab(State, Key, TabState) ->
    {ok, Tab} = dict:find(Key, State#state.tabs),
    Tab#gtab{state=TabState}.

render_cell(State, Key, Default) ->
    TabState = case Default of
		Key -> selected;
		_ -> unselected
	    end,
    #panel{id=Key, class=gcell, body=render_tab(State, Key, TabState)}.

process_tab(State, [{Sel, Unsel, Text} | T ]) ->
    Tab = #gtab{selected_image=Sel,
		unselected_image=Unsel,
		text=Text,
		delegate=?MODULE,
		postback={selected, Text}
	       },

    Tabs = dict:store(Text, Tab, State#state.tabs),
    process_tab(State#state{tabs = Tabs}, T);
process_tab(State, []) ->
    State.

render_element(R) ->
    [{_, _, First} | _ ] = R#gbar.tabs,
    Default = wf:coalesce([R#gbar.default, First]),
    State = process_tab(#state{record = R, currenttab=Default}, R#gbar.tabs),
    wf:state(state, State),

    Cells = [render_cell(State, Text, Default) || {_, _, Text} <- R#gbar.tabs ],
    Panel = #panel{class=gbar, body=Cells},
    element_panel:render_element(Panel).

event({selected, Selected}) ->
    State = wf:state(state),
    Current = State#state.currenttab,
    event({selected, Current, Selected, State});

event({selected, Current, Next, State}) when Current =:= Next ->
    ok;
event({selected, Current, Next, State}) ->
    wf:update(Current, render_tab(State, Current, unselected)),

    wf:state(state, State#state{currenttab=Next}),
    R = State#state.record,

    Module = wf:coalesce([R#gbar.delegate, wf_context:page_module()]),

    case R#gbar.postback of
	undefined -> ok;
	selected ->
	    Module:event({selected, Next});
	Postback ->
	    Module:event(Postback)
    end.
