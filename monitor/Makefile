NAME=cloudbuilder
VERSION=0.1
RELEASE ?= 1
OBJDIR=./obj
SUBDIRS=core ui dhcp
INCLUDES = $(patsubst %,-pa $(PWD)/%/obj/ebin,$(SUBDIRS))
RPMBIN=$(OBJDIR)/$(NAME)-$(VERSION).tar.gz
INSTPATH ?= /tmp/cloudbuilder

all: release

release: $(SUBDIRS) $(OBJDIR)/$(NAME).tar.gz Makefile

$(OBJDIR):
	@mkdir -p $(OBJDIR)

$(OBJDIR)/$(NAME).rel: $(NAME).rel $(OBJDIR)
	@echo "Instailling $< to $@"
	@cat $< | sed "s/__VSN__/$(VERSION).$(RELEASE)/" > $@

SUBDIRS:=$(strip $(SUBDIRS))

.PHONY: $(SUBDIRS)

$(SUBDIRS):
	cd $@ && $(MAKE)

.PHONY: $(SUBDIRS:=-install)

.PHONY: clean
.PHONY: $(SUBDIRS:=-clean)

clean : $(SUBDIRS:=-clean)

$(SUBDIRS:=-clean):
	cd $(patsubst %-clean,%,$@) && $(MAKE) clean

$(OBJDIR)/$(NAME).boot: $(OBJDIR)/$(NAME).rel $(DIRS)
	@echo "Compiling (BOOT) $< to $@"
	@mkdir -p $(OBJDIR)
	@erlc -pa /usr/lib/yaws/ebin $(INCLUDES) -o $(OBJDIR) $<

$(OBJDIR)/$(NAME).tar.gz: $(OBJDIR)/$(NAME).boot $(ISITES)
	@echo "Creating (Release) $@"
	@cd $(OBJDIR); erl -noshell $(PKGS) \
	-pa /usr/lib/yaws/ebin $(INCLUDES) \
	-eval "systools:make_tar(\"$(NAME)\", [{dirs, [include]}])" \
	 -s init stop

clean: 
	@rm -f *~
	@rm -f src/*~
	@rm -f include/*~
	@rm -rf obj
	@rm -f *.dump

$(INSTPATH):
	@mkdir -p $(INSTPATH)

install: $(INSTPATH) release
	@echo "Installing to $(INSTPATH)"
	@cat $(OBJDIR)/$(NAME).tar.gz | (cd $(INSTPATH); tar -zxv)

$(OBJDIR)/$(NAME)-$(VERSION):
	@mkdir -p $(OBJDIR)/$(NAME)-$(VERSION)

$(OBJDIR)/$(NAME)-$(VERSION)/$(NAME).spec: $(OBJDIR)/$(NAME)-$(VERSION) $(NAME).spec Makefile
	@echo "Installing RPM specfile $@"
	@cat $(NAME).spec | sed -e 's/_RPM_VERSION/$(VERSION)/;s/_RPM_RELEASE/$(RELEASE)/' > $@

.PHONY: $(RPMBIN)

$(RPMBIN): $(OBJDIR)/$(NAME)-$(VERSION)/$(NAME).spec 
	tar -c --exclude=.git --exclude=*.spec --exclude=*~ --exclude=obj * | (cd $(OBJDIR)/$(NAME)-$(VERSION); tar xf -)
	(cd $(OBJDIR); tar cvz $(NAME)-$(VERSION)) > $(RPMBIN)

srcrpm: $(RPMBIN)
	rpmbuild -ts $(RPMBIN)


