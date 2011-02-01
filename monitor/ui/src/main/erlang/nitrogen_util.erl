-module(nitrogen_util).
-export([render_edititem/2, template/1]).

-include_lib("nitrogen/include/wf.inc").

render_edititem(Id, Label) ->
    #panel{ class="edititem",
	    body=[
		  #label{class="edititem-label", text=wf:f("~s: ", [Label])},
		  #textbox{id=Id,
			   class="edititem-input",
			   delegate=?MODULE}
		 ]
	  }.

template(Name) ->
    PrivDir = code:priv_dir(cloudbuilder_ui),
    #template { file= PrivDir ++ "/webui/templates/" ++ Name }.
