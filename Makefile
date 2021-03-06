# leave these lines alone
vpath %.erl src test
vpath %.beam ebin
vpath %.hrl include

SOURCE_DIR=src
EBIN_DIR=ebin
DOC_DIR=doc
INCLUDE_DIR=include

ERL_TOP = /home/jackiedong/otp_src_R14B01
ERL = $(ERL_TOP)/bin/erl -boot start_clean
ERLC = $(ERL_TOP)/bin/erlc

ifeq ($(shell test R14A \> $$(erl -noshell -eval 'io:format(erlang:system_info(otp_release)), halt().') && echo yes),yes)
INETS_DEF=
else
INETS_DEF=-Dnew_inets
endif

ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) $(INETS_DEF) -Wall +debug_info # +native -v

# Here's a list of the erlang modules you want compiling
#DIRS= interfaces services webmin
DIRS := $(wildcard ./*)

INCLUDES=$(wildcard ./include/*.hrl)

SRCS := $(wildcard ./*/*.erl)

COMMON_PARAMS= 	-sname haf@localhost  \
		-pa $(wildcard ./*/ebin) \
		-s application start mongodb \
		-s application start interfaces

# The first target in any makefile is the default target.
compile: ${SRCS:%.erl=%.beam} subdirs

all::
	echo "Start to make"
	for i in $(DIRS); do 	\
		make -C $$i compile;	 \
		make -C $$i compile; \
	done

%.beam: %.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<

run:
	$(ERL) $(COMMON_PARAMS)

escript:
	./eshell

## special compilation requirements are added here
specical1.beam: special1.erl
	${ERL} -Dflag1 -W0 special1.erl

# the subdirs target compiles any code in sub-dirctories
subdirs:
#	cd dir1; make
#	cd dir2; make

# remove all the code
clean:
	rm -rf *.beam erl_crash.dump
	cd dir1; make clean
	cd dir2; make clean

