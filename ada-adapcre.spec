#
# spec file for package ada-adapcre
#

%define libname libadapcre
%define soname 0_1_3

Name:           ada-adapcre
Version:        0.1.3
Release:        1
Summary:        Ada bindings for PCRE library
License:        MIT
Group:          Development/Libraries
URL:            https://github.com/ray2501/ada-adapcre
Source:         %{name}-%{version}.tar.gz
BuildRequires:  gcc-ada
BuildRequires:  make
BuildRequires:  pcre-devel

%description
PCRE is a popular C-library that implements regular expression 
pattern matching using the same syntax and semantics as Perl 5. 
PCRE means Perl Compatible Regular Expressions.

This package is Ada bindings for PCRE library.


%package -n %{libname}%{soname}
Summary:        Library files for ada-adapcre
Group:          System/Libraries

%description -n %{libname}%{soname}
The %{libname}%{soname} package contains library files for ada-adapcre.


%package devel
Summary:        Development files for ada-adapcre
Requires:       %{name} = %{version}
Requires:       %{libname}%{soname} = %{version}

%description devel 
The %{name}-devel package contains source code and linking information for
developing applications that use ada-adapcre.

%prep
%setup -q

%build
make PREFIX=/usr LIBDIR=lib64

%install
make DESTDIR=%{buildroot} PREFIX=/usr LIBDIR=lib64 install

%post -n %{libname}%{soname} -p /sbin/ldconfig
%postun -n %{libname}%{soname} -p /sbin/ldconfig

%files -n %{libname}%{soname}
%{_libdir}/*.so.*

%files
%license LICENSE
%doc README.md

%files devel
%{_includedir}/*
%{_libdir}/*.so
%{_libdir}/adapcre
%dir /usr/share/gpr
/usr/share/gpr/adapcre.gpr

%changelog

