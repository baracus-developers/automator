{application, cloudbuilder_dhcp, [{description, "CloudBuilder DHCP service "},
{vsn, "__VSN__"},
{modules, [__MODULES__]},
{registered, [dhcp_sup, dhcp_server, dhcp_alloc]},
{applications, [kernel, stdlib, cloudbuilder_core]},
{mod, {dhcp_app, []}} ]}.
