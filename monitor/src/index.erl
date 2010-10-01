-module (index).
-behavior(gen_event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("hostinfo.hrl").
-include_lib("wf_elements.hrl").

main() -> #template { file="./monitor/site/templates/bare.html" }.

title() -> "Novell Cloud-Builder".

toolbar() ->
    [
     {"/images/inventory.png", "/images/inventory-bw.png", "Inventory"},
     {"/images/tickets-small.png", "/images/tickets-small-bw.png", "Admission"},
     {"/images/Warning.png", "/images/Warning-bw.png", "Alerts"},
     {"/images/Profile.png", "/images/Profile-bw.png", "Users"}
    ].

body() ->
    S = self(),
    {ok, TaskPid} = wf:comet(fun() -> comet_init(S) end),

    receive
	{ready, ServerPid} ->
	    comet_server:config(TaskPid, ServerPid),
	    ok
    after 1000 ->
	    throw(comet_error)
    end,

    [
     #panel{ class="titlebar",
	     body=[
		   #panel{class="logo",
			  body=#image{ image="/images/cloudbuilder-logo.png"}
			 },
		   #panel{class="salutation",
			  body=[
				"Welcome, " ++ wf:user() ++ " ",
				#link{ body="Log Out", postback=logout}
			       ]
			 },
		   #panel{class="toolbar",
			  body=#gbar{tabs=toolbar(), postback=selected}}
		  ]
	   },
     #panel { id=mainPanel, class="mainpanel", body=render_mainpanel("Inventory")}
    ].

render_mainpanel(Id) ->
    comet_server:flush_jobs(),

    case Id of
	"Inventory" -> #inventorypanel{};
	"Admission" -> #admissionpanel{};
	"Alerts" -> #panel{};
	"Users" -> #users{}
    end.

comet_init(From) ->
    {ok, ServerPid} = comet_server:start_link(),

    From ! {ready, ServerPid},

    comet_loop().

comet_loop() ->
    receive
       'INIT' -> ok;
       {execute, Job} ->
	    Job()
    end,
    comet_loop().

event({selected, Id}) ->
    wf:update(mainPanel, render_mainpanel(Id));
event(logout) ->
    wf:logout(),
    wf:redirect("/");
event(_) -> ok.

    
