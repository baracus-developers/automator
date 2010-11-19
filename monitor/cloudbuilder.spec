%define rpmrel _RPM_RELEASE
%define conf cloudbuilder.conf.example
%define rootdir %{_libdir}/%{name}
%define homedir %{_localstatedir}/lib/%{name}
%define logdir %{_localstatedir}/log/%{name}

BuildRequires: erlang yaws yaws_security nitrogen xmerl_dom

Summary: cloudbuilder - A tool to manage large scale service provisioning
Name: cloudbuilder
Version: _RPM_VERSION
License: GPL
Release: %{rpmrel}
Requires: erlang puppet-server baracus
Group: Systems Management
Source: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-root

%description
Authors
--------------------------
  Gregory Haskins <ghaskins@novell.com>

%debug_package
%prep
%setup

%build
make RELEASE=%{release}

%install
make install INSTPATH=$RPM_BUILD_ROOT%{rootdir} RELEASE=%{release}
mkdir -p $RPM_BUILD_ROOT/%{homedir}/resolvers
mkdir -p $RPM_BUILD_ROOT/%{logdir}
mkdir -p $RPM_BUILD_ROOT/%{_bindir}
install -m 755 cloudbuilder-shell $RPM_BUILD_ROOT/%{_bindir}

mkdir -p $RPM_BUILD_ROOT/etc
uuid=$(uuidgen)
cat %{conf} | sed "s/__COOKIE__/$uuid/" > $RPM_BUILD_ROOT/etc/%{conf}

mkdir -p $RPM_BUILD_ROOT/etc/init.d

cat init.d/cloudbuilderd | sed "s|%FINAL_ROOTDIR%|%{rootdir}|" | sed "s|%FINAL_HOME%|%{homedir}|" | sed "s|%FINAL_LOGDIR%|%{logdir}|" | sed "s|%FINAL_VSN%|%{version}.%{release}|" > $RPM_BUILD_ROOT/etc/init.d/cloudbuilderd

chmod a+x $RPM_BUILD_ROOT/etc/init.d/cloudbuilderd

# Install documentation  
%clean
make clean

%files
%defattr(-,root,root)
%{rootdir}
%{homedir}
%{logdir}
/etc/%{conf}
/etc/init.d/cloudbuilderd
%{_bindir}/cloudbuilder-shell

%changelog
