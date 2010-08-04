class sshd {

    package { "openssh": 
        ensure => installed 
    }

    service { "sshd":
        require => Package['openssh'],
	ensure => running,
    }

}
