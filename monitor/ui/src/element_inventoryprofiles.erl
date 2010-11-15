-module(element_inventoryprofiles).
-export([reflect/0, render_element/1, event/1]).

-import(util, [render_edititem/2]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include("hostinfo.hrl").
-include("staging.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventoryprofiles).

coalesce(Value) ->
    wf:coalesce([Value, "-"]).

render_profile(Profile) ->
    [
     Profile#stagingprofile.name,
     coalesce(Profile#stagingprofile.pool),
     coalesce(Profile#stagingprofile.type),
     coalesce(Profile#stagingprofile.host),
     coalesce(Profile#stagingprofile.bmcaddr),
     coalesce(Profile#stagingprofile.username),
     coalesce(Profile#stagingprofile.password),
     #link{ text="delete",
	    delegate=?MODULE,
	    postback={delete_profile, Profile#stagingprofile.name}}
    ].

render_profiles() ->
    {ok, RawProfiles} = staging_server:enum_profiles(),
    
    Profiles = [render_profile(Profile) || Profile <- RawProfiles],
    
    #cbtable{class="profiles-table",
	     data=Profiles,
	     map= [
		   sn_name@text,
		   sn_pool@text,
		   sn_type@text,
		   sn_host@text,
		   sn_bmcaddr@text,
		   sn_username@text,
		   sn_password@text,
		   sn_actions@body
		  ],
             header=[
		     #tableheader{text="Name"},
		     #tableheader{text="Pool"},
		     #tableheader{text="Type"},
		     #tableheader{text="Host"},
		     #tableheader{text="BMC Address"},
		     #tableheader{text="Username"},
		     #tableheader{text="Password"},
		     #tableheader{text="Actions"}
		    ],
	     rowspec=[
		      #tablecell { id=sn_name },
		      #tablecell { id=sn_pool },
		      #tablecell { id=sn_type},
		      #tablecell { id=sn_host},
		      #tablecell { id=sn_bmcaddr},
		      #tablecell { id=sn_username},
		      #tablecell { id=sn_password},
		      #tablecell { id=sn_actions }
		     ]
	    }.

render_body() ->
    [
     #h1{},
     #panel{ id="profiles-list",
	     body=render_profiles()
	   },
     #button{text="+",
	     postback=add_profile, delegate=?MODULE}
    ].

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

event(add_profile) ->
    BmcTypes = [undefined | baracus_driver:get_bmctypes()],

    Panel = #dialog{ id="add-profile",
		     title="Add new profile",
		     body=[
			   #panel{body=[
					render_edititem(profilename, "Profile name"),
					render_edititem(pool, "Pool"),
					#panel{class="edititem",
					       body=[
						     #label{class="edititem-label",
							    text="Type:"
							   },
						     #dropdown{id=type,
							       class="edititem-input",
							       options=[
									#option{text=Type, value=Type} ||
									   Type <- BmcTypes
								       ]
							      }
						    ]
					      },
					render_edititem(host, "Host"),
					render_edititem(bmcaddr, "BMC Address"),
					render_edititem(username, "Username"),
					render_edititem(password, "Password")
				       ]
				 },
			   #flash{},
			   #panel{class="dialog-controls",
				  body=[
					#button{text="Cancel", postback=cancel_profile, delegate=?MODULE},
					#button{text="Save", postback=save_profile, delegate=?MODULE}
				       ]
				 }
			  ]
		   },
    wf:insert_top("application", Panel);

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

