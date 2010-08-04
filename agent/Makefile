nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

NAME=agent
OBJDIR=./obj
APPDIR=$(OBJDIR)/app
EBINDIR=$(APPDIR)/$(NAME)/ebin
SRCS += $(shell find *.erl)
OBJS = $(patsubst %.erl,$(EBINDIR)/%.beam,$(SRCS))
WSMODULES = $(patsubst %.erl,%, $(SRCS))
MODULES = $(subst $(space),$(commaspace), $(WSMODULES))

all: application release

application: $(OBJS) $(EBINDIR)/$(NAME).app Makefile

release: $(OBJDIR)/$(NAME).tgz Makefile

$(EBINDIR)/$(NAME).app: $(NAME).app Makefile
	@echo "Compiling (APPSPEC) $< to $@"
	@cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' > $@ 

$(OBJS): $(SRCS) Makefile

$(EBINDIR)/%.beam: %.erl Makefile
	@touch $(NAME).app
	@echo "Compiling (Erlang) $< to $@"
	@mkdir -p $(EBINDIR)
	@erlc -o $(EBINDIR) $<

$(APPDIR)/$(NAME).boot: $(NAME).rel application
	@echo "Compiling (Release) $< to $@"
	@mkdir -p $(APPDIR)
	@erlc -pa $(EBINDIR) -o $(APPDIR) $<

$(OBJDIR)/$(NAME).tgz: $(APPDIR)/$(NAME).boot
	@cd $(APPDIR); tar -zcv * > ../$(NAME).tgz

clean: 
	@rm -f *~
	@rm -rf obj
	@rm -f *.dump


