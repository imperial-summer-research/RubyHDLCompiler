# build Ruby compile by scripting sml

# Because build seems to need to be run from REPL, we script using expect


# define tools to use
EXPECT:=expect
SML:=sml
# note mllex and mlyacc are from MLton distribution, but seem to work fine with
# SML/NJ
ML_LEX:=mllex
ML_YACC:=mlyacc

# target of compilation: bytecode file
BYTECODE_FILE:=rc.x86-linux

# list of source files kept in file "all"
# must be kept up to date manually
# build depends on all of these
# format of file is use "foo.sml";, one SML source name per line
ALL_SRCS_FILE:=all
# use shell command to remove '"', ';' and 'use' from file
# use awk instead of sed to prevent matching in comments (hopefully)
# Note $$2 because make will substitute $2 for blank
SRCS = $(shell cat ${ALL_SRCS_FILE} | awk '/use\ \"[a-zA-Z\._0-9]*\"/{ print $$2 }' | tr '";' ' ') \

# $(warning "finished include")
# $(warning "sources = |" $(SRCS) "|")     # for debugging

# could also use make substitutition, something like the following
# \
# 	$(subst '"',' ', \
# 	$(subst ';',' ', \
# 	$(subst 'use',' ', \
# 		$(include 'all'))))


# top-level target
.PHONY : top
top: $(BYTECODE_FILE)



# build bytecode file
# new way: use an SML script which uses all source files then builds bytecode

$(BYTECODE_FILE): $(SRCS)   all_build.sml
	$(SML) all_build.sml
	@echo "Built rc :"  $@


# build parser and lexer:
# build parser
ruby.yacc.sml: ruby.yacc
	$(ML_YACC) $<

# extra depedency
ruby.yacc.sig: ruby.yacc

# build lexer
ruby.lex.sml: ruby.lex
	$(ML_LEX) $<


# run test suite using locally-built compiler,
# to try to ensure that we haven't added any bugs
# note recursive make calling makefile in current directory to run the tests
.PHONY: test
test:
	cd .. && RUBY_RC_DIR=`pwd`/compiler RUBY_RC_EXE=rc $(MAKE)
	echo "run tests OK"




# OLD CODE BELOW

# COMMENTED OUT: old, fragile way of compiling using expect
# since SML seems to require building from the interactive prompt,
# use expect script as a quick and dirty way to automate
# basically it runs:
# % sml
#   - use "all";
#   - SMLofNJ.exportFn("rc", rc);
# I used the autoexpect tool to generate this automatically from my
# interactive prompt, which is why the script is verbose and overly specific
# TODO: use SML's CM (Compilation Manager) to automate the build in a more
# thorough / less fragile way
#$(BYTECODE_FILE): $(SRCS)
#	$(EXPECT) -f script.exp
#	echo "Built file"
