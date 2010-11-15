-module(element_inventoryresolvers).
-export([reflect/0, render_element/1, event/1]).
-export([start_upload_event/1, finish_upload_event/4]).

-import(util, [render_edititem/2]).

-include_lib("nitrogen/include/wf.inc").
-include_lib ("nitrogen/include/google_chart.hrl").
-include("hostinfo.hrl").
-include("staging.hrl").
-include("wf_elements.hrl").

reflect() -> record_info(fields, inventoryresolvers).

universaltime_to_string(TimeStamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = TimeStamp,
    
    lists:flatten(io_lib:format("~2.10.0B/~2.10.0B/~B ~2.10.0B:~2.10.0B:~2.10.0B GMT",
				[Month, Day, Year, Hour, Minute, Second])).

flatten_data(Resolver) ->
    [
     Resolver#resolver.name,
     io_lib:format("~B", [Resolver#resolver.version]),
     universaltime_to_string(Resolver#resolver.uploaded),
     Resolver#resolver.owner,
     #link{text="delete", delegate=?MODULE, postback={delete, Resolver#resolver.id}}
    ].

render_table() ->
    {ok, Resolvers} = staging_server:enum_resolvers(),

    Data = [flatten_data(Resolver) || Resolver <- Resolvers],

    #cbtable{class="resolvers",
	     data=Data,
	     map= [
		   name@text,
		   version@text,
		   uploaded@text,
		   owner@text,
		   actions@body
		  ],
             header=[
		     #tableheader{text="Name"},
		     #tableheader{text="Version"},
		     #tableheader{text="Uploaded"},
		     #tableheader{text="Owner"},
		     #tableheader{text="Actions"}
		    ],
	     rowspec=[
		      #tablecell { id=name },
		      #tablecell { id=version },
		      #tablecell { id=uploaded },
		      #tablecell { id=owner},
		      #tablecell { id=actions }
		     ]
	    }.


render_body() ->
    [
     #h1{},
     #panel{id="resolver-list", body=render_table()},
     #upload { tag=upload, delegate=?MODULE, show_button=false }  
    ].

render_element(R) ->
    Panel = #panel{ body=#panel{id="resolvers-panel",
				body=render_body()
			       }
		  },
    element_panel:render_element(Panel).

event({delete, Id}) ->
    ok = staging_server:delete_resolver(Id),
    wf:update("resolver-list", render_table()).


start_upload_event(_) ->
    ok.

finish_upload_event(_Tag, FileName, LocalFileData, Node) ->
    case staging_server:add_resolver(FileName, LocalFileData) of
	ok ->
	    wf:update("resolver-list", render_table());
	_ ->
	    wf:wire(#alert{text="Invalid script"})
    end,
    ok.
