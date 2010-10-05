nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

OBJDIR=./obj
EBINDIR=$(OBJDIR)/monitor/ebin
SITEDIR=$(OBJDIR)/monitor/site
SRCS += $(shell find src/*.erl)
SITE += $(shell find site/* -type f)
OBJS = $(patsubst src/%.erl,$(EBINDIR)/%.beam,$(SRCS))
ISITES = $(patsubst site/%,$(SITEDIR)/%,$(SITE))
WSMODULES = $(patsubst src/%.erl,%, $(SRCS))
MODULES = $(subst $(space),$(commaspace), $(WSMODULES))
INCLUDES += -I/usr/lib
INCLUDES += -I./include

all: application release tests

application: $(OBJS) $(EBINDIR)/monitor.app Makefile

release: $(OBJDIR)/monitor.boot $(ISITES) Makefile

$(EBINDIR)/monitor.app: monitor.app Makefile
	@echo "Compiling (APPSPEC) $< to $@"
	@cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' > $@ 

$(OBJS): $(SRCS) Makefile

$(SITEDIR)/%: site/%
	@mkdir -p $(shell dirname $@)
	cp $< $@

$(EBINDIR)/%.beam: src/%.erl Makefile
	@touch monitor.app
	@echo "Compiling (Erlang) $< to $@"
	@mkdir -p $(EBINDIR)
	@erlc $(INCLUDES) -o $(EBINDIR) $<

$(OBJDIR)/monitor.boot: monitor.rel application
	@echo "Compiling (Release) $< to $@"
	@mkdir -p $(OBJDIR)
	@erlc -pa /usr/lib/yaws/ebin -pa $(EBINDIR) -o $(OBJDIR) $<

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


