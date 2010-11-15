nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

NAME=cloudbuilder_ui
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
INCLUDES += -I../include
INSTPATH ?= /tmp/cloudbuilder
EFLAGS += -Ddebug +debug_info

all: application tests

application: $(ISITES) $(OBJS) $(EBINDIR)/$(NAME).app Makefile

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



