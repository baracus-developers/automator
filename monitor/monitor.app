{application, monitor, [{description, "Cloud Monitor"},
{vsn, "0.1.0"}, {modules, [__MODULES__]},
	 {registered, [mon_sup]},
{applications, [kernel, stdlib, sasl]},
{mod, {mon_app, []}} ]}.
