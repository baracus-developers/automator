%define conf cloudbuilder.conf.example
%define rootdir %{_libdir}/%{name}
%define homedir %{_localstatedir}/lib/%{name}
%define logdir %{_localstatedir}/log/%{name}

BuildRequires: erlang maven

Summary: cloudbuilder - A tool to manage large scale service provisioning
Name: cloudbuilder
Version: ${project.version}
License: GPL
Release: 1
Requires: erlang puppet-server baracus
Group: Systems Management
Source: %{name}-%{version}.tar.gz
Source1: cloudbuilder-shell
Source2: cloudbuilderd.init
Source3: %{conf}
BuildRoot: %{_tmppath}/%{name}-%{version}-root

%description
Authors
--------------------------
  Gregory Haskins <ghaskins@novell.com>

%debug_package
%prep
%setup -c

%build

%install
mvn install -DoutputDirectory=$RPM_BUILD_ROOT%{rootdir}
mkdir -p $RPM_BUILD_ROOT/%{homedir}/resolvers
mkdir -p $RPM_BUILD_ROOT/%{logdir}
mkdir -p $RPM_BUILD_ROOT/%{_bindir}
install -m 755 %{SOURCE1} $RPM_BUILD_ROOT/%{_bindir}

mkdir -p $RPM_BUILD_ROOT/etc
uuid=$(uuidgen)
cat %{SOURCE3} | sed "s/__COOKIE__/$uuid/" > $RPM_BUILD_ROOT/etc/%{conf}

mkdir -p $RPM_BUILD_ROOT/etc/init.d

cat %{SOURCE2} | sed "s|%FINAL_ROOTDIR%|%{rootdir}|" | sed "s|%FINAL_HOME%|%{homedir}|" | sed "s|%FINAL_LOGDIR%|%{logdir}|" | sed "s|%FINAL_VSN%|%{version}.%{release}|" > $RPM_BUILD_ROOT/etc/init.d/cloudbuilderd

chmod a+x $RPM_BUILD_ROOT/etc/init.d/cloudbuilderd

# Install documentation  
%clean
mvn clean

%files
%defattr(-,root,root)
%{rootdir}
%{homedir}
%{logdir}
/etc/%{conf}
/etc/init.d/cloudbuilderd
%{_bindir}/cloudbuilder-shell

%changelog
