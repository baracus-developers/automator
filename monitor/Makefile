nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

NAME=cloudbuilder
VERSION=0.1
RELEASE ?= 1
OBJDIR=./obj
EBINDIR=$(OBJDIR)/ebin
SITEDIR=$(OBJDIR)/priv/webui
SRCS += $(shell find src/*.erl)
SITE += $(shell find site/* -type f)
OBJS = $(patsubst src/%.erl,$(EBINDIR)/%.beam,$(SRCS))
ISITES = $(patsubst site/%,$(SITEDIR)/%,$(SITE))
WSMODULES = $(patsubst src/%.erl,%, $(SRCS))
MODULES = $(subst $(space),$(commaspace), $(WSMODULES))
INCLUDES += -I/usr/lib
INCLUDES += -I./include
RPMBIN=$(OBJDIR)/$(NAME)-$(VERSION).tar.gz
INSTPATH ?= /tmp/cloudbuilder
EFLAGS += -Ddebug +debug_info

all: application tests

application: $(OBJS) $(EBINDIR)/$(NAME).app Makefile

release: $(OBJDIR)/$(NAME).tar.gz tests Makefile

$(EBINDIR)/$(NAME).app: $(NAME).app Makefile
	@echo "Compiling (APPSPEC) $< to $@"
	@cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' | sed "s/__VSN__/$(VERSION).$(RELEASE)/" > $@ 

$(OBJS): $(SRCS) Makefile

$(OBJDIR):
	@mkdir -p $(OBJDIR)

$(SITEDIR)/%: site/%
	@mkdir -p $(shell dirname $@)
	cp $< $@

$(EBINDIR)/%.beam: src/%.erl Makefile
	@touch $(NAME).app
	@echo "Compiling (Erlang) $< to $@"
	@mkdir -p $(EBINDIR)
	@erlc $(EFLAGS) $(INCLUDES) -o $(EBINDIR) $<

$(OBJDIR)/$(NAME).rel: $(NAME).rel $(OBJDIR)
	@echo "Instailling $< to $@"
	@cat $< | sed "s/__VSN__/$(VERSION).$(RELEASE)/" > $@

$(OBJDIR)/$(NAME).boot: $(OBJDIR)/$(NAME).rel application
	@echo "Compiling (BOOT) $< to $@"
	@mkdir -p $(OBJDIR)
	@erlc -pa /usr/lib/yaws/ebin -pa $(EBINDIR) -o $(OBJDIR) $<

$(OBJDIR)/simple.boot: simple.rel
	@echo "Compiling (BOOT) $< to $@"
	@mkdir -p $(OBJDIR)
	@erlc -o $(OBJDIR) $<

$(OBJDIR)/$(NAME).tar.gz: $(OBJDIR)/$(NAME).boot $(ISITES)
	@echo "Creating (Release) $@"
	@cd $(OBJDIR); erl -noshell $(PKGS) \
	-pa /usr/lib/yaws/ebin -pa ./ebin \
	-eval "systools:make_tar(\"$(NAME)\", [{dirs, [include]}])" \
	 -s init stop

tests: application
	@echo "Running unit tests.."
	@erl -noshell $(PKGS) \
	-eval "eunit:test(\"$(EBINDIR)\", [verbose])" \
	-yaws embedded true \
	 -s init stop

clean: 
	@rm -f *~
	@rm -f src/*~
	@rm -f include/*~
	@rm -rf obj
	@rm -f *.dump

$(INSTPATH):
	@mkdir -p $(INSTPATH)

install: $(INSTPATH) release $(OBJDIR)/simple.boot
	@echo "Installing to $(INSTPATH)"
	@cat $(OBJDIR)/$(NAME).tar.gz | (cd $(INSTPATH); tar -zxv)
	cp $(OBJDIR)/simple.boot $(INSTPATH)/releases/$(VERSION).$(RELEASE)

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


