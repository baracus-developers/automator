class agent {

	file { "/usr/sbin/agent.tgz":
		source => "puppet:///agent/agent.tgz",
		mode => 500,
		require => Service["epmd"]
	}

	file { "/etc/init.d/agent":
		source => "puppet:///agent/agent",
		mode => 755,
		require => File["/usr/sbin/agent.tgz"]
	}

	file { "/etc/murdock-id":
		content => template("agent/murdock-id.erb"),
		mode => 700
	}

	service { "agent": ensure => running, require => File["/etc/init.d/agent", "/etc/murdock-id"] }
}

