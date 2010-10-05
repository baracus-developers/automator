
-record(stagingrule, {name, xpath, profile, resolver, action}).
-record(stagingprofile, {name, pool, type, host, username, password, bmcaddr}).
-record(stagingnode, {mac, zone, pool, type, host, username, password, bmcaddr}).

