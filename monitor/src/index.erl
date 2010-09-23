-module (index).
-behavior(gen_event).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("hostinfo.hrl").
-include_lib("wf_elements.hrl").

main() -> #template { file="./monitor/site/templates/bare.html" }.

title() -> "Novell Cloud-Builder".

toolbar() ->
    [ {"/images/Search.png", "/images/Search-bw.png", "Pools"},
      {"/images/Profile.png", "/images/Profile-bw.png", "Users"}
    ].

body() ->
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
     #panel { id=mainPanel, class="mainpanel", body=render_mainpanel("Pools")}
    ].

render_mainpanel(Id) ->
    case Id of
	"Pools" -> #pools{};
	"Users" -> #users{}
    end.

event({selected, Id}) -> 
    wf:update(mainPanel, render_mainpanel(Id));
event(logout) ->
    wf:logout(),
    wf:redirect("/");
event(_) -> ok.

    
