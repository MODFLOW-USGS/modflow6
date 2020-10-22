# makefile created on 2020-10-22 09:28:03.593336
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
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/kind.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/budgetdata.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/BlockParser.o \
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

