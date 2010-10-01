-module(element_admissionpanel).
-export([reflect/0, render_element/1, event/1]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include_lib("hostinfo.hrl").
-include_lib("power.hrl").
-include_lib("rules.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, admissionpanel).

render_rule(HostRule) ->
    #listitem{body=[
		    HostRule#hostrule.name ++ " ",
		    #link{ text="delete", delegate=?MODULE, postback={delete_rule, HostRule#hostrule.name}}
		   ]
	     }.

render_rules_list() ->
    {ok, HostRules} = rules_server:enum(),

    #list{ class="rules",
	   body=[render_rule(HostRule) || HostRule <- HostRules]
	 }.


render_rules() ->
    #backsplash{ id="rules-panel",
	         body=[
		       #h1{ text="Rules"},
		       #panel{ id="rules-list", body=render_rules_list()},
		       #button{text="+", postback=add_rule, delegate=?MODULE}
		      ]
	       }.

render_powerprofile(Profile) ->
    #listitem{body=[
		    Profile#powerprofile.name ++ " ",
		    #link{ text="delete",
			   delegate=?MODULE,
			   postback={delete_profile, Profile#powerprofile.name}}
		   ]
	     }.

render_powerprofiles() ->
    {ok, Profiles} = power_server:enum_profiles(),

    #list{ class="profiles",
	   body=[
		 render_powerprofile(Profile) ||
		    Profile <- Profiles
		]
	 }.


render_power() ->
    #backsplash{ id="power-panel",
	         body=[
		       #h1{ text="Power Configuration"},
		       #label{text="Profiles"},
		       #panel{ id="power-profiles",
			       body=render_powerprofiles()
			     },
		       #button{text="+", postback=add_profile, delegate=?MODULE},
		       #powernodes{}
		      ]
	       }.

handle_event({system, hostrule, _Operation, _HostRule}) ->
    wf:update("rules-list", render_rules_list()),
    wf:flush();
handle_event({system, powerprofile, _Operation, _Profile}) ->
    wf:update("power-profiles", render_powerprofiles()),
    wf:flush();
handle_event(Event) ->
    ok.

render_element(R) ->
    comet_event_relay:add_handler(host_events, "admission-panel",
				  fun(Event) -> handle_event(Event) end),   

    Panel = #panel{ body=#panel{id="admission-panel",
				body=[
				      render_rules(),
				      render_power()
				     ]
			       }
		  },
    element_panel:render_element(Panel).

event(add_rule) ->
    Panel = #lightbox{ id="add-rule",
		       body=#panel{class="general-lightbox", 
				   body=[
					 #h2{ text="Add new host rule" },
					 #flash{},
					 #label{text="Rule name:"},
					 #textbox{id=name },
					 #label{text="Rule:"},
					 #textbox{id=xpath},
					 #button{text="Cancel", postback=cancel_rule, delegate=?MODULE},
					 #button{text="Save", postback=save_rule, delegate=?MODULE}
					 ]
				  }
		     },
    wf:insert_top("admission-panel", Panel);
event(save_rule) ->
    Name = wf:q(name),
    XPath = wf:q(xpath),
    
    case rules_server:add(Name, XPath) of
	ok ->
	    wf:remove("add-rule");
	{error, Error} ->
	    Msg = wf:f("Error saving rule: ~p", [Error]), 
	    wf:flash(Msg)
    end;

event(cancel_rule) ->
    wf:remove("add-rule");
event({delete_rule, Name}) ->
    rules_server:delete(Name);

event(add_profile) ->
    {ok, Rules} = rules_server:enum(),
    Panel = #lightbox{ id="add-profile",
		       body=#panel{class="general-lightbox", 
				   body=[
					 #h2{ text="Add new power profile" },
					 #flash{},
					 #label{text="Profile name:"},
					 #textbox{id=profilename },
					 #label{text="Rule:"},
					 #dropdown{id=rule,
						   options=[
							    #option{text=Rule#hostrule.name,
								    value=Rule#hostrule.name} ||
							       Rule <- Rules
							   ]
						  },
					 #label{text="Type:"},
					 #dropdown{id=type,
						   value="IPMI",
						   options=[
							    #option{text="IPMI",
								    value="IPMI"}
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
    wf:insert_top("admission-panel", Panel);

event(save_profile) ->
    Name = wf:q(profilename),
    Rule = wf:q(rule),
    Type = wf:q(type),
    Host = wf:q(host),
    BMCAddr = wf:q(bmcaddr),
    Username = wf:q(username),
    Password = wf:q(password),

    Profile = #powerprofile{name=Name, rule=Rule, type=Type, host=Host, bmcaddr=BMCAddr,
			    username=Username, password=Password}, 
    
    case power_server:add_profile(Profile) of
	ok ->
	    wf:remove("add-profile");
	{error, Error} ->
	    Msg = wf:f("Error saving profile: ~p", [Error]), 
	    wf:flash(Msg)
    end;

event(cancel_profile) ->
    wf:remove("add-profile");

event({delete_profile, Name}) ->
    power_server:delete_profile(Name).

