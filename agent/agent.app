{application, agent, [{description, "Cloud Agent"},
{vsn, "0.1.0"}, {modules, [__MODULES__]},
	 {registered, [agent_sup]},
{applications, [kernel, stdlib, sasl]},
{mod, {agent_app, []}} ]}.
