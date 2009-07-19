%define debug_package %{nil}
%define erlname rtpproxy


Name:		erlrtpproxy
Version:	0.1
Release:	1%{?dist}
Summary:	RTP proxying daemon
Group:		Applications/Internet
License:	GPLv3+
URL:		http://code.google.com/p/erlrtpproxy/
Source0:	http://erlrtpproxy.googlecode.com/files/%{name}-%{version}.tar.bz2
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:	erlang
BuildRequires:	erlang-erlsyslog
BuildRequires:	erlang-eradius
Requires:	erlang
Requires:	erlang-erlsyslog
Requires:	erlang-eradius
Requires(post): /sbin/chkconfig
Requires(preun): /sbin/chkconfig
Requires(preun): /sbin/service
Requires(postun): /sbin/service


%description
RTP proxying daemon for OpenSER and compatible SIP-servers.


%package ser
Summary:	OpenSER-compatible interface for %{name}
Group:		Applications/Internet
Requires:	%{name} = %{version}-%{release}
Requires:	erlang
Requires:	erlang-erlsyslog
Requires(post): /sbin/chkconfig
Requires(preun): /sbin/chkconfig
Requires(preun): /sbin/service
Requires(postun): /sbin/service

%description ser
An OpenSER-compatible interface for %{name}.


%prep
%setup -q


%build
make %{?_smp_mflags}


%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/%{erlname}.app $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/%{erlname}.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/%{erlname}_app.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/%{erlname}_sup.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/call.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/media.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/player.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/rtcp.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/ser.app $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/ser.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/ser_app.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
install -m 644 ebin/ser_sup.beam $RPM_BUILD_ROOT%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin

install -D -m 755 priv/%{name}.init $RPM_BUILD_ROOT%{_initrddir}/%{name}
install -D -m 644 priv/%{name}.config $RPM_BUILD_ROOT%{_sysconfdir}/%{name}.config
install -D -m 644 priv/%{name}.sysconfig $RPM_BUILD_ROOT%{_sysconfdir}/sysconfig/%{name}

install -D -m 755 priv/%{name}-ser.init $RPM_BUILD_ROOT%{_initrddir}/%{name}-ser
install -D -m 644 priv/%{name}-ser.config $RPM_BUILD_ROOT%{_sysconfdir}/%{name}-ser.config
install -D -m 644 priv/%{name}-ser.sysconfig $RPM_BUILD_ROOT%{_sysconfdir}/sysconfig/%{name}-ser


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
        /sbin/service %{name} condrestart >/dev/null 2>&1
fi


%pre ser
getent group %{erlname} >/dev/null || groupadd -r %{erlname}
getent passwd %{erlname} >/dev/null || useradd -r -g %{erlname} -d / -s /sbin/nologin -c "RTP proxying daemon" %{erlname}
exit 0


%post ser
/sbin/chkconfig --add %{name}-ser


%preun ser
if [ $1 = 0 ]; then
        /sbin/service %{name}-ser stop >/dev/null 2>&1
        /sbin/chkconfig --del %{name}-ser
fi


%postun ser
if [ "$1" -ge "1" ]; then
        /sbin/service %{name}-ser condrestart >/dev/null 2>&1
fi


%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%dir %{_libdir}/erlang/lib/%{erlname}-%{version}
%dir %{_libdir}/erlang/lib/%{erlname}-%{version}/ebin
%{_initrddir}/%{name}
%config(noreplace) %{_sysconfdir}/%{name}.config
%config(noreplace) %{_sysconfdir}/sysconfig/%{name}
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/%{erlname}.app
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/%{erlname}.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/%{erlname}_app.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/%{erlname}_sup.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/call.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/media.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/player.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/rtcp.beam

%files ser
%defattr(-,root,root,-)
%{_initrddir}/%{name}-ser
%config(noreplace) %{_sysconfdir}/%{name}-ser.config
%config(noreplace) %{_sysconfdir}/sysconfig/%{name}-ser
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser.app
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser_app.beam
%{_libdir}/erlang/lib/%{erlname}-%{version}/ebin/ser_sup.beam

%changelog
* Sun Jun 28 2009 Peter Lemenkov <lemenkov@gmail.com> 0.1-1
- Ver. 0.1

* Wed Mar 25 2009 Peter Lemenkov <lemenkov@gmail.com> 0.1-0.3.svn
- Enabled necessary BuildRequires

* Fri Feb 27 2009 Peter Lemenkov <lemenkov@gmail.com> 0.1-0.2.svn
- Rebuild

* Fri Feb 20 2009 Peter Lemenkov <lemenkov@gmail.com> 0.1-0.1.svn
- Initial package

