
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

-record(dialog, {?ELEMENT_BASE(element_dialog), title, body}).

% main panels
-record(inventorypanel, {?ELEMENT_BASE(element_inventorypanel)}).
-record(users, {?ELEMENT_BASE(element_users)}).

% inventory subpanel
-record(activeinventory, {?ELEMENT_BASE(element_activeinventory)}).
-record(inventoryautomation, {?ELEMENT_BASE(element_inventoryautomation)}).
-record(nodestaging, {?ELEMENT_BASE(element_nodestaging)}).
