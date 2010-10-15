%define debug_package %{nil}
%define erlname ser


Name:		erlrtpproxy-ser
Version:	0.1
Release:	1%{?dist}
Summary:	RTP proxying daemon interface
Group:		Applications/Internet
License:	GPLv3+
URL:		http://code.google.com/p/erlrtpproxy/
Source0:	http://erlrtpproxy.googlecode.com/files/%{name}-%{version}.tar.bz2
Source0:	%{name}-%{version}.tar.bz2
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:	erlang
BuildRequires:	erlang-erlsyslog
Requires:	erlang
Requires:	erlang-erlsyslog
Requires(post): /sbin/chkconfig
Requires(preun): /sbin/chkconfig
Requires(preun): /sbin/service
Requires(postun): /sbin/service


%description
RTP proxying daemon interface, compatible with
OpenSER/OpenSIPs/Kamailio/SIP-Router nathelper module.


%prep
%setup -q


%build
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
make install


%pre
getent group %{erlname} >/dev/null || groupadd -r %{erlname}
getent passwd %{erlname} >/dev/null || useradd -r -g %{erlname} -d / -s /sbin/nologin -c "RTP proxying daemon" %{erlname}
exit 0


%post
/sbin/chkconfig --add %{name}


%preun
if [ $1 = 0 ]; then
        /sbin/service %{name} stop >/dev/null 2>&1
        /sbin/chkconfig --del %{name}
fi

%postun
if [ "$1" -ge "1" ]; then
        /sbin/service %{name} reload >/dev/null 2>&1
fi


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%{_initrddir}/%{name}
%config(noreplace) %{_sysconfdir}/%{name}.config
%config(noreplace) %{_sysconfdir}/sysconfig/%{name}
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser.app
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser_app.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser_ctl.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser_sup.beam

%changelog
* Tue Jul 13 2010 Peter Lemenkov <lemenkov@gmail.com> 0.1-1
- Initial package

