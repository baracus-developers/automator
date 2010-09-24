
-record(gtab, {?ELEMENT_BASE(element_gtab),
	       selected_image, unselected_image,
	       state=unselected, text=undefined,
	       postback, delegate}).
-record(gbar, {?ELEMENT_BASE(element_gbar),
	       tabs, default, postback, delegate}).

-record(backsplash, {?ELEMENT_BASE(element_backsplash), body}).


% main panels
-record(pools, {?ELEMENT_BASE(element_pools)}).
-record(users, {?ELEMENT_BASE(element_users)}).
