
-record(gtab, {?ELEMENT_BASE(element_gtab),
	       selected_image, unselected_image,
	       state=unselected, text=undefined,
	       postback, delegate}).
-record(gbar, {?ELEMENT_BASE(element_gbar),
	       tabs, default, postback, delegate}).

-record(tab, {?ELEMENT_BASE(element_tab), state=unselected, text, postback, delegate}).

-record(backsplash, {?ELEMENT_BASE(element_backsplash), body}).
-record(tabbedbacksplash, {?ELEMENT_BASE(element_tabbedbacksplash),
			   panels, default, delegate, postback}).
-record(cbtable, {?ELEMENT_BASE(element_cbtable), header, rowspec, map, data}).
-record(nodestaging, {?ELEMENT_BASE(element_nodestaging)}).


% main panels
-record(inventorypanel, {?ELEMENT_BASE(element_inventorypanel)}).
-record(stagingpanel, {?ELEMENT_BASE(element_stagingpanel)}).
-record(users, {?ELEMENT_BASE(element_users)}).
