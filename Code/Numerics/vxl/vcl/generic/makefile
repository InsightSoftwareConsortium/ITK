#
# fsm@robots.ox.ac.uk
#

BLAH := $(wildcard vcl_*.hhh)
BLAH := $(BLAH:vcl_%.hhh=%)

all: $(BLAH:%=vcl_%.h)


$(BLAH:%=vcl_%.h) : vcl_%.h : vcl_%.hhh ./zap.pl
	./zap.pl $< > $@
