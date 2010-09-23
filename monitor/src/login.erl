-module (login).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./monitor/site/templates/bare.html" }.

title() -> "CloudBuilder: Authentication Required".

layout() ->
    [
     #image { class="login-logo", image="/images/cloudbuilder-logo.png"},
     #panel { body=[
		    #image { class="openid-logo",
			     image="http://openid.net/images/login-bg.gif"},
		    #textbox {class="login-input",
			      id=claimed_id},
		    #button { class="login-button",
			      id=button, text="Login", postback=click}
		   ]
	    }
    ].

body() ->
     #lightbox { body=[#panel {class="login-lightbox", body = layout()}]}.
	
event(click) ->
    ClaimedId = wf:q(claimed_id),
    Url = wf:f("/openid/submit?openid.claimed_id=~s", [ClaimedId]),
    wf:redirect(Url).
