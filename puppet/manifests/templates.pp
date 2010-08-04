class baseclass {
}

node default {
        Package { provider => zypper }

	include baseclass
        include erlang
	include agent
	include sshd
}
