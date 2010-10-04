
-record(gtab, {?ELEMENT_BASE(element_gtab),
	       selected_image, unselected_image,
	       state=unselected, text=undefined,
	       postback, delegate}).
-record(gbar, {?ELEMENT_BASE(element_gbar),
	       tabs, default, postback, delegate}).

-record(backsplash, {?ELEMENT_BASE(element_backsplash), body}).
-record(cbtable, {?ELEMENT_BASE(element_cbtable), header, rowspec, map, data}).
-record(nodestaging, {?ELEMENT_BASE(element_nodestaging)}).


% main panels
-record(inventorypanel, {?ELEMENT_BASE(element_inventorypanel)}).
-record(admissionpanel, {?ELEMENT_BASE(element_admissionpanel)}).
-record(users, {?ELEMENT_BASE(element_users)}).
