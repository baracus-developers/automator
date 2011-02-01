
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
-record(cbtable, {?ELEMENT_BASE(element_cbtable), header, rowspec, map, data, selectable=false, delegate, postback}).

-record(poollist, {?ELEMENT_BASE(element_poollist), hid=wf:temp_id()}).

-record(dialog, {?ELEMENT_BASE(element_dialog), title, body}).

% main panels
-record(servicespanel, {?ELEMENT_BASE(element_servicespanel)}).
-record(inventorypanel, {?ELEMENT_BASE(element_inventorypanel)}).
-record(users, {?ELEMENT_BASE(element_users)}).

% service subpanel
-record(services, {?ELEMENT_BASE(element_services), delegate, postback}).
-record(serviceelements, {?ELEMENT_BASE(element_serviceelements), name}).
-record(catalog, {?ELEMENT_BASE(element_catalog)}).

% inventory subpanel
-record(activeinventory, {?ELEMENT_BASE(element_activeinventory)}).
-record(inventoryautomation, {?ELEMENT_BASE(element_inventoryautomation)}).
-record(inventoryprofiles, {?ELEMENT_BASE(element_inventoryprofiles)}).
-record(inventoryresolvers, {?ELEMENT_BASE(element_inventoryresolvers)}).
-record(nodestaging, {?ELEMENT_BASE(element_nodestaging)}).
