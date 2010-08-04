Puppet.type(:package).provide :zypper, :parent => :rpm do
    desc "Support for suse ``zypper`` package manager."

    has_feature :versionable

    commands :zypper => "/usr/bin/zypper"
    commands :rpm => "rpm"
    defaultfor :operatingsystem => :opensuse 
    confine    :operatingsystem => :opensuse

    # Install a package using 'zypper'.
    def install
        should = @resource.should(:ensure)
        self.debug "Ensuring => #{should}"
        wanted = @resource[:name]

        # XXX: We don't actually deal with epochs here.
        case should
        when true, false, Symbol
            # pass
        else
            # Add the package version
            wanted += "-%s" % should
        end
	output = zypper "--no-gpg-check", "--quiet", :install, "-y", wanted

        unless self.query
            raise Puppet::ExecutionFailure.new(
                "Could not find package %s" % self.name
            )
        end
    end

    # What's the latest package version available?
    def latest
        #zypper can only get a list of *all* available packages?
        output = zypper "list-updates"

        if output =~ /#{@resource[:name]}\s*\|\s*([0-9\.\-]+)/
            return $1
        else
            # zypper didn't find updates, pretend the current
            # version is the latest
            return @property_hash[:ensure]
        end
    end

    def update
        # zypper install can be used for update, too
        self.install
    end
end
