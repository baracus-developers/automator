{application, cloudbuilder, [{description, "Cloud Monitor"},
{vsn, "__VSN__"},
{modules, [__MODULES__]},
{registered, [mon_sup]},
{env, [
    {port, 8000}
]},
{applications, [kernel, stdlib, sasl, inets, ssl, yaws, crypto, public_key,
		eopenid, nprocreg, simple_bridge, nitrogen]},
{mod, {mon_app, []}} ]}.
