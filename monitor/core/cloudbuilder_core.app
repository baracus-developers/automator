{application, cloudbuilder_core, [{description, "CloudBuilder core service "},
{vsn, "__VSN__"},
{modules, [__MODULES__]},
{registered, [mon_sup]},
{env, [
    {port, 8000}
]},
{applications, [kernel, stdlib, sasl, inets, ssl, crypto, public_key]},
{mod, {mon_app, []}} ]}.
