FC = gfortran

OPT = -Wall -pedantic -std=f2003 -fbounds-check -O -Wuninitialized -ffpe-trap=invalid,zero,overflow -fbacktrace

OBJDIR = obj
SRCDIR = src
TESTBINDIR = tmp

TESTSRCS := $(wildcard $(SRCDIR)/*_test.f90)
TESTS := $(TESTSRCS:$(SRCDIR)/%.f90=%)
TESTOBJS := $(TESTSRCS:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)
TESTBINS := $(TESTSRCS:$(SRCDIR)/%.f90=$(TESTBINDIR)/%)

SRCS := $(filter-out $(TESTSRCS), $(wildcard $(SRCDIR)/*.f90))
OBJS := $(SRCS:$(SRCDIR)/%.f90=$(OBJDIR)/%.o)
MODOBJS := $(filter-out $(OBJDIR)/main.o, $(OBJS))

NO="\033[0m"
GREEN="\033[32m"
RED="\033[31m"
BRED="\033[31;01m"
BYELLOW="\033[33;01m"

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(COMPILE.f) -J $(OBJDIR) $(OPT) $(OUTPUT_OPTION) $<

$(OBJDIR)/%.mod: $(SRCDIR)/%.f90 $(OBJDIR)/%.o
	@:

$(TARGET): $(OBJS)
	$(LINK.f) $^ $(LOADLIBES) $(OPT) $(LDLIBS) -o $@

clean::
	$(RM) $(TARGET) ./$(OBJDIR)/*.mod ./$(OBJDIR)/*.o $(TESTBINDIR)/*_test

all:: clean $(TARGET)

.SECONDARY: $(TESTOBJS) $(TESTBINS)

$(TESTBINDIR)/%_test: $(MODOBJS) $(OBJDIR)/%_test.o
	$(LINK.f) $^ $(LOADLIBES) $(OPT) $(LDLIBS) -o $@

%_test: $(TESTBINDIR)/%_test
	@printf $(RED) && $(TESTBINDIR)/$@ && echo $(GREEN)$@ SUCCEEDED$(NO) || echo $(BRED)$@ FAILED$(NO);

test:: $(TESTS)
	@:

# dependencies
