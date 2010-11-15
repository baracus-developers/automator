-module(element_inventoryautomation).
-export([reflect/0, render_element/1, event/1, sort_event/2]).

-import(util, [render_edititem/2]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include("hostinfo.hrl").
-include("staging.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventoryautomation).

render_rule(StagingRule) ->
    Id = wf:temp_id(),

    #sortitem{ tag=StagingRule#stagingrule.name,
	       body=#singlerow{id=Id,
		               cells=[
				      #tablecell{class=["rule-cell", "rule-name"],
						 text=StagingRule#stagingrule.name
						},
				      #tablecell{class=["rule-cell", "rule-action"],
						 body=[
						       #link{ text="delete",
							      delegate=?MODULE,
							      postback={delete_rule,
									StagingRule#stagingrule.name}}
						      ]
						}
				     ],
			       actions=#event{type=click,
					      delegate=?MODULE,
					      postback={click, Id, StagingRule#stagingrule.name}}
			      }
	     }.

render_rules() ->
    {ok, StagingRules} = staging_server:enum_rules(),

    [
     #singlerow{cells=[
		  #tableheader{class=["rule-cell", "rule-name"],
			       text="Name"
			      },
		  #tableheader{class=["rule-cell", "rule-action"],
			       text="Actions"
			      }
		 ]
	   },
     #sortblock{ class="rules",
		 delegate=?MODULE,
		 items=[render_rule(StagingRule) || StagingRule <- StagingRules]
	       }
    ].

coalesce(Value) ->
    wf:coalesce([Value, "-"]).

handle_event({system, stagingrule, _Operation, _StagingRule}) ->
    wf:update("rules-list", render_rules()),
    wf:flush();
handle_event(Event) ->
    ok.

render_element(R) ->
    comet_event_relay:add_handler(host_events, "staging-panel",
				  fun(Event) -> handle_event(Event) end),   

    Panel = #panel{ body=[
			  #h1{},
			  #panel{ id="rules-list",
				  body=render_rules()},
			  #button{class="addrule-button",
				  text="+",
				  postback=add_rule, delegate=?MODULE}
			 ]
		  },

    element_panel:render_element(Panel).

resolver_to_string(Resolver) ->
    io_lib:format("~s (v~B)", [Resolver#resolver.name, Resolver#resolver.version]).

render_addrule() ->
    {ok, Profiles} = staging_server:enum_profiles(),
    {ok, Resolvers} = staging_server:enum_resolvers(),
    Actions = [none, deploy, reject],
   
    [
     render_edititem(name, "Rule name"),
     render_edititem(xpath, "Rule"),
     #panel{class="edititem",
	    body=[
		  #label{class="edititem-label",
			 text="Profile:"
			},
		  #dropdown{id=profile,
			    class="edititem-input",
			    options=[#option{text=undefined, value=undefined} |
				     [#option{text=Val#stagingprofile.name,
					      value=Val#stagingprofile.name}
				      || Val <- Profiles
				     ]
				    ]
			   }
		 ]
	   },
     #panel{class="edititem",
	    body=[
		  #label{class="edititem-label",
			 text="Resolver:"
			},
		  #dropdown{id=resolver,
			    class="edititem-input",
			    options=[#option{text=undefined, value=undefined} |
				     [#option{text=resolver_to_string(Val),
					      value=Val#resolver.id}
				      || Val <- Resolvers]
				    ]
			   }
		 ]
	   },
     #panel{class="edititem",
	    body=[
		  #label{class="edititem-label",
			 text="Action:"
			},
		  #dropdown{id=action,
			    class="edititem-input", 
			    options=[#option{text=Val, value=Val}
				     || Val <- Actions]
			   }
		 ]
	   },
     #flash{},
     #panel{class="dialog-controls",
	    body=[
		  #button{text="Cancel", postback=cancel_rule, delegate=?MODULE},
		  #button{text="Save", postback=save_rule, delegate=?MODULE}
		 ]
	   }
    ].

event(add_rule) ->  
    Panel = #dialog{ id="add-rule",
		     title="Add new host rule",
		     body=#panel{body=render_addrule()}
		   },
    wf:insert_top("application", Panel);
event(save_rule) ->
    Name = wf:q(name),
    XPath = wf:q(xpath),
    Profile = normalize_undefined(wf:q(profile)),
    Resolver = normalize_undefined(wf:q(resolver)),
    Action = wf:q(action),

    case staging_server:add_rule(Name, XPath, Profile, Resolver, Action) of
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
event({click, Id, Name}) ->
    wf:wire(Id, #effect{effect=highlight}).

sort_event(_Tag, Items) -> 
    ok = staging_server:order_rules(Items),
    ok.

normalize_undefined(Val) ->
    case Val of 
	"undefined" -> undefined;
	V -> V
    end.
