# makefile created on 2020-06-11 17:48:22.733501
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = zbud6

SOURCEDIR1=../src
SOURCEDIR2=../../../src/Utilities

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} 

.SUFFIXES: .c .cpp .f .f90 .F90 .fpp .o

# Define the Fortran compile flags
FC = gfortran
FFLAGS = -O2 -fbacktrace -Bstatic -D_WIN32 -MMD -cpp 

# Define the C compile flags
CC = gcc
CFLAGS = -O2 -Bstatic -D_UF -MMD -cpp 

# Define the libraries
SYSLIBS = -lgfortran -lm 

OBJECTS = \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/budgetdata.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/grb.o \
$(OBJDIR)/zone.o \
$(OBJDIR)/zoneoutput.o \
$(OBJDIR)/zbud6.o 

# Define task functions

# Create the bin directory and compile and link the program
all: makebin | $(PROGRAM)

# Make the bin directory for the executable
makebin :
	mkdir -p $(BINDIR)

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(FFLAGS) -o $@ $(OBJECTS) $(SYSLIBS) -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

$(OBJDIR)/%.o : %.cpp
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@
	cat ./obj_temp/$*.d >> Dependencies
	rm -f $*.d

# Clean the object and module files and the executable
.PHONY : clean
clean : 
	-rm -r Dependencies
	-rm -rf $(OBJDIR)
	-rm -rf $(PROGRAM)

# Clean the object and module files
.PHONY : cleanobj
cleanobj : 
	-rm -rf $(OBJDIR)

# Touch dependencies
Dependencies : 
	touch Dependencies

