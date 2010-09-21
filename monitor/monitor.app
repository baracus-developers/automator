{application, monitor, [{description, "Cloud Monitor"},
{vsn, "0.1.0"},
{modules, [__MODULES__]},
{registered, [mon_sup]},
{env, [
    {port, 8000}
]},
{applications, [kernel, stdlib, sasl, inets, ssl, yaws, crypto, eopenid,
		nprocreg, simple_bridge, nitrogen]},
{mod, {mon_app, []}} ]}.
