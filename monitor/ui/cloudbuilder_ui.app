{application, cloudbuilder_ui, [{description, "CloudBuilder user-interface service "},
{vsn, "__VSN__"},
{modules, [__MODULES__]},
{registered, [ui_sup]},
{env, [
    {port, 8000}
]},
{applications, [kernel, stdlib, sasl, inets, ssl, yaws, crypto, public_key,
		eopenid, nprocreg, simple_bridge, nitrogen]},
{mod, {ui_app, []}} ]}.
