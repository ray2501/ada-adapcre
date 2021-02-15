PREFIX   ?= /usr/local
OBJDIR    = obj
LIBDIR   ?= lib
SRCDIR    = src
DESTDIR   ?=
GPR_FILES = gnat/*.gpr
LIBRARY_TYPE = dynamic

MAJOR   = 0
MINOR   = 1
REV     = 4
VERSION = $(MAJOR).$(MINOR).$(REV)
DBUSADA = libadapcre-$(VERSION)

SO_LIBRARY   = libadapcre.so.$(VERSION)

PREPARE := $(shell mkdir -p $(OBJDIR) $(LIBDIR))

all: build_lib gpr

build_lib:
	gnatmake -Padapcre

gpr:
	gnatprep -b gnat/adapcre.gpr.gp gnat/adapcre.gpr -DInvoked_By_Makefile \
       '-DIncludedir="$(PREFIX)/include"' '-DLibdir="$(PREFIX)/$(LIBDIR)"' \
       '-DAlidir="$(PREFIX)/$(LIBDIR)"' '-DLibrary_Type="${LIBRARY_TYPE}"'

build_examples:
	gnatmake -Padapcre_examples

build_check:    
	gnatmake -Padapcre_tests

install: install_lib

install_lib: build_lib
	install -d $(DESTDIR)$(PREFIX)/include/adapcre
	install -d $(DESTDIR)$(PREFIX)/$(LIBDIR)/adapcre
	install -d $(DESTDIR)$(PREFIX)/share/gpr
	install -m 644 $(SRCDIR)/*.ad[bs] $(DESTDIR)$(PREFIX)/include/adapcre
	install -m 444 $(LIBDIR)/*.ali $(DESTDIR)$(PREFIX)/$(LIBDIR)/adapcre
	install -m 644 $(GPR_FILES) $(DESTDIR)$(PREFIX)/share/gpr
	install -m 644 $(LIBDIR)/$(SO_LIBRARY) $(DESTDIR)$(PREFIX)/$(LIBDIR)
	cd $(DESTDIR)$(PREFIX)/$(LIBDIR) && ln -sf $(SO_LIBRARY) libadapcre.so

test: build_lib build_check
	./runner

clean:
	@rm -rf $(OBJDIR)
	@rm -rf $(LIBDIR)
	@rm -rf pcre_config email test_0 test_2
	@rm -rf runner

