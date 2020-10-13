# makefile created on 2020-10-13 14:19:07.930620
# by pymake (version 1.2.0) for the 'zbud6' executable 
# using the 'gfortran' fortran compiler(s).


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../../../src/Utilities

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} 

.SUFFIXES: .f90 .fpp .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/version.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/budgetdata.o \
$(OBJDIR)/zone.o \
$(OBJDIR)/grb.o \
$(OBJDIR)/zoneoutput.o \
$(OBJDIR)/zbud6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

