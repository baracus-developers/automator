nullstring :=
space := $(nullstring) # a space at the end
commaspace := ,$(space)

NAME=cloudbuilder_core
VERSION=0.1
RELEASE ?= 1
OBJDIR=./obj
EBINDIR=$(OBJDIR)/ebin
SRCS += $(shell find src/*.erl)
OBJS = $(patsubst src/%.erl,$(EBINDIR)/%.beam,$(SRCS))
WSMODULES = $(patsubst src/%.erl,%, $(SRCS))
MODULES = $(subst $(space),$(commaspace), $(WSMODULES))
INCLUDES += -I/usr/lib
INCLUDES += -I../include
INCLUDES += -I../dhcp/include
EFLAGS += -Ddebug +debug_info

all: application tests

application: $(OBJS) $(EBINDIR)/$(NAME).app Makefile

$(EBINDIR)/$(NAME).app: $(NAME).app Makefile
	@echo "Compiling (APPSPEC) $< to $@"
	@cat $< | sed "s/__MODULES__/$(MODULES)/" | sed 's/\[, /\[/' | sed "s/__VSN__/$(VERSION).$(RELEASE)/" > $@ 

$(OBJDIR):
	@mkdir -p $(OBJDIR)

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




