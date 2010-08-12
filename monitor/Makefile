nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

OBJDIR=./obj
EBINDIR=$(OBJDIR)/monitor/ebin
SRCS += $(shell find *.erl)
OBJS = $(patsubst %.erl,$(EBINDIR)/%.beam,$(SRCS))
WSMODULES = $(patsubst %.erl,%, $(SRCS))
MODULES = $(subst $(space),$(commaspace), $(WSMODULES))
INCLUDES += -I/usr/lib

all: application release

application: $(OBJS) $(EBINDIR)/monitor.app Makefile

release: $(OBJDIR)/monitor.boot Makefile

$(EBINDIR)/monitor.app: monitor.app Makefile
	@echo "Compiling (APPSPEC) $< to $@"
	@cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' > $@ 

$(OBJS): $(SRCS) Makefile

$(EBINDIR)/%.beam: %.erl Makefile
	@touch monitor.app
	@echo "Compiling (Erlang) $< to $@"
	@mkdir -p $(EBINDIR)
	@erlc $(INCLUDES) -o $(EBINDIR) $<

$(OBJDIR)/monitor.boot: monitor.rel application
	@echo "Compiling (Release) $< to $@"
	@mkdir -p $(OBJDIR)
	@erlc -pa /usr/lib/yaws/ebin -pa $(EBINDIR) -o $(OBJDIR) $<

clean: 
	@rm -f *~
	@rm -rf obj
	@rm -f *.dump


