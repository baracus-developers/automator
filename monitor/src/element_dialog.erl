-module(element_dialog).
-export([reflect/0, render_element/1]).

-include_lib("nitrogen/include/wf.inc").
-include("wf_elements.hrl").

reflect() -> record_info(fields, dialog).

render_element(R) ->
    Panel = #panel{ id=R#dialog.id,
		    anchor=R#dialog.anchor,
		    class=R#dialog.class, 
		    body=[
			  #panel{ class="dialog-background"},
			  #panel{ class="outer-dialog",
				  body=[
					#h2{text=R#dialog.title},
					#panel{class="inner-dialog",
					       body=R#dialog.body
					      }
				       ],
				  actions=[
					   #hide{},
					   #appear{speed=500}
					  ]
				}
			 ]
		  },
    element_panel:render_element(Panel).
