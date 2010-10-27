%define rpmrel _RPM_RELEASE
%define conf cloudbuilder.conf.example

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
make RELEASE=%{rpmrel}

%install
make install INSTPATH=$RPM_BUILD_ROOT%{_bindir}/%{name} RELEASE=%{rpmrel}

mkdir -p $RPM_BUILD_ROOT/var/%{name}
pushd $RPM_BUILD_ROOT/var/%{name}
for i in resolvers logs
do
    mkdir $i
done
popd

ROOTDIR=%{_bindir}/%{name}

mkdir -p $RPM_BUILD_ROOT/etc
uuid=$(uuidgen)
cat %{conf} | sed "s/__COOKIE__/$uuid/" > $RPM_BUILD_ROOT/etc/%{conf}

mkdir -p $RPM_BUILD_ROOT/etc/init.d

HOMEDIR=/var/%{name}

cat init.d/cloudbuilderd | sed "s|%FINAL_ROOTDIR%|$ROOTDIR|" | sed "s|%FINAL_HOME%|$HOMEDIR|" | sed "s|%FINAL_VSN%|%{version}.%{rpmrel}|" > $RPM_BUILD_ROOT/etc/init.d/cloudbuilderd

chmod a+x $RPM_BUILD_ROOT/etc/init.d/cloudbuilderd

# Install documentation  
%clean
make clean

%files
%defattr(-,root,root)
%{_bindir}/%{name}
/var/%{name}
/etc/%{conf}
/etc/init.d/cloudbuilderd

%changelog
