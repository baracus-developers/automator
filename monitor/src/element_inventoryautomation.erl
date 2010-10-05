-module(element_inventoryautomation).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include("hostinfo.hrl").
-include("staging.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventoryautomation).

render_rule(StagingRule) ->
    #listitem{body=[
		    StagingRule#stagingrule.name ++ " ",
		    #link{ text="delete", delegate=?MODULE, postback={delete_rule, StagingRule#stagingrule.name}}
		   ]
	     }.

render_rules() ->
    {ok, StagingRules} = staging_server:enum_rules(),

    #list{ class="rules",
	   body=[render_rule(StagingRule) || StagingRule <- StagingRules]
	 }.

render_profile(Profile) ->
    #listitem{body=[
		    Profile#stagingprofile.name ++ " ",
		    #link{ text="delete",
			   delegate=?MODULE,
			   postback={delete_profile, Profile#stagingprofile.name}}
		   ]
	     }.

render_profiles() ->
    {ok, Profiles} = staging_server:enum_profiles(),

    #list{ class="profiles",
	   body=[
		 render_profile(Profile) ||
		    Profile <- Profiles
		]
	 }.

render_body() ->
    [
     #panel{ class="config-pane" ,
	     body=[
		   #label{text="Rules:"},
		   #panel{ id="rules-list",
			   body=render_rules()},
		   #button{text="+",
			   postback=add_rule, delegate=?MODULE}
		  ]
	   },
     #panel{ class="config-pane",
	     body=[
		   #label{text="Profiles:" },
		   #panel{ id="profiles-list",
			   body=render_profiles()
			 },
		   #button{text="+",
			   postback=add_profile, delegate=?MODULE}
		  ]
	   }
    ].

handle_event({system, stagingrule, _Operation, _StagingRule}) ->
    wf:update("rules-list", render_rules()),
    wf:flush();
handle_event({system, stagingprofile, _Operation, _Profile}) ->
    wf:update("profiles-list", render_profiles()),
    wf:flush();
handle_event(Event) ->
    ok.

render_element(R) ->
    comet_event_relay:add_handler(host_events, "staging-panel",
				  fun(Event) -> handle_event(Event) end),   

    Panel = #panel{ body=#panel{id="automation-panel",
				body=render_body()
			       }
		  },
    element_panel:render_element(Panel).

event(add_rule) -> 
    {ok, Profiles} = staging_server:enum_profiles(),
    Actions = [none, accept, reject],

    Panel = #lightbox{ id="add-rule",
		       body=#panel{class="general-lightbox", 
				   body=[
					 #h2{ text="Add new host rule" },
					 #flash{},
					 #label{text="Rule name:"},
					 #textbox{id=name },
					 #label{text="Rule:"},
					 #textbox{id=xpath},
					 #label{text="Profile:"},
					 #dropdown{id=profile,
						   value="none",
						   options=[#option{text=Val#stagingprofile.name,
								    value=Val#stagingprofile.name}
							    || Val <- Profiles]
						  },
					 #label{text="Action:"},
					 #dropdown{id=action,
						   options=[#option{text=Val, value=Val}
							    || Val <- Actions]
						   },
					 #button{text="Cancel", postback=cancel_rule, delegate=?MODULE},
					 #button{text="Save", postback=save_rule, delegate=?MODULE}
					 ]
				  }
		     },
    wf:insert_top("automation-panel", Panel);
event(save_rule) ->
    Name = wf:q(name),
    XPath = wf:q(xpath),
    Profile = wf:q(profile),
    Action = wf:q(action),
    
    case staging_server:add_rule(Name, XPath) of
	ok ->
	    wf:remove("add-rule");
	{error, Error} ->
	    Msg = wf:f("Error saving rule: ~p", [Error]), 
	    wf:flash(Msg)
    end;

event(cancel_rule) ->
    wf:remove("add-rule");
event({delete_rule, Name}) ->
    staging_server:delete_rule(Name);

event(add_profile) ->
    Panel = #lightbox{ id="add-profile",
		       body=#panel{class="general-lightbox", 
				   body=[
					 #h2{ text="Add new profile" },
					 #flash{},
					 #label{text="Profile name:"},
					 #textbox{id=profilename },
					 #label{text="Pool:"},
					 #textbox{id=pool },
					 #label{text="Type:"},
					 #dropdown{id=type,
						   options=[
							    #option{text=Type, value=Type} ||
							       Type <- baracus_driver:get_bmctypes()
							   ]
						  },
					 #label{text="Host:"},
					 #textbox{id=host },
					 #label{text="BMC Address:"},
					 #textbox{id=bmcaddr },
					 #label{text="Username:"},
					 #textbox{id=username },
					 #label{text="Password:"},
					 #password{id=password },
					 #button{text="Cancel", postback=cancel_profile, delegate=?MODULE},
					 #button{text="Save", postback=save_profile, delegate=?MODULE}
					 ]
				  }
		     },
    wf:insert_top("automation-panel", Panel);

event(save_profile) ->
    Name = wf:q(profilename),
    Pool = wf:q(pool),
    Type = wf:q(type),
    Host = wf:q(host),
    BMCAddr = wf:q(bmcaddr),
    Username = wf:q(username),
    Password = wf:q(password),

    Profile = #stagingprofile{name=Name, pool=Pool, type=Type, host=Host, bmcaddr=BMCAddr,
			      username=Username, password=Password}, 
    
    case staging_server:add_profile(Profile) of
	ok ->
	    wf:remove("add-profile");
	{error, Error} ->
	    Msg = wf:f("Error saving profile: ~p", [Error]), 
	    wf:flash(Msg)
    end;

event(cancel_profile) ->
    wf:remove("add-profile");

event({delete_profile, Name}) ->
    staging_server:delete_profile(Name).

