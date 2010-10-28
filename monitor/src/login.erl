-module (login).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> util:template("bare.html").

title() -> "CloudBuilder: Authentication Required".

layout() ->
    ClaimedId = wf:cookie("openid.claimed_id"),

    case wf:q(msg) of
	undefined -> ok;
	Msg -> wf:flash(wf:url_decode(Msg))
    end,

    [
     #panel{ class="login-logo",
	     body=#image{image="/images/cloudbuilder-logo.png"}},
     #flash{},
     #panel { class="login-input-panel",
	      body=[
		    #image { class="openid-logo",
			     image="http://openid.net/images/login-bg.gif"},
		    #textbox {class="login-input",
			      id=claimed_id,
			      text=ClaimedId},
		    #button { class="login-button",
			      id=button, text="Login", postback=click}
		   ]
	    },
     #panel { class="workloadiq-logo",
	      body=#image{image="/images/WorkloadIQ_logo.png"}
	    }
    ].

body() ->
     #lightbox { body=[#panel {class="general-lightbox", body = layout()}]}.
	
event(click) ->
    ClaimedId = wf:q(claimed_id),

    wf:cookie("openid.claimed_id", ClaimedId),

    Url = wf:f("/openid/submit?openid.claimed_id=~s", [ClaimedId]),
    wf:redirect(Url).
