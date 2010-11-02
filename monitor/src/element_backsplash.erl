-module(element_backsplash).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, backsplash).

render_element(R) ->
    Panel = #panel{ id=R#backsplash.id,
		    anchor=R#backsplash.anchor,
		    class=["outer-backsplash", R#backsplash.class],
		    body=#panel{ class="inner-backsplash",
				 body=R#backsplash.body
			       }
		  },
    element_panel:render_element(Panel).
