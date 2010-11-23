class erlang {

        define zypper_repo($path) {
                exec { "/usr/bin/zypper -n ar -f -c $path $title":
                        unless => "/usr/bin/test -e /etc/zypp/repos.d/$title.repo",
                }
        }

        zypper_repo { erlang_repo: path => 'http://download.opensuse.org/repositories/home:/ghaskins:/erlang/SLE_11_SP1/' }

	package { "erlang": ensure => installed, require => Zypper_repo["erlang_repo"] }
	
	file { "/etc/init.d/epmd":
		source => "puppet:///erlang/epmd",
		mode => 755,
		require => Package["erlang"]
	}

	service { "epmd": ensure => running, require => File["/etc/init.d/epmd"] }

}

