
-record(resolver, {id, name, uploaded, owner, size}).
-record(stagingrule, {name, priority, xpath, profile, resolver, action}).
-record(stagingprofile, {name, pool, type, host, username, password, bmcaddr}).
-record(stagingnode, {mac, inventory, zone, pool, type, host,
		      username, password, bmcaddr}).

