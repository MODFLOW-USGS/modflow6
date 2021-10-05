# makefile created on 2021-07-30 14:36:28.583060
# by pymake (version 1.2.1) for the 'zbud6' executable 
# using the 'gfortran' fortran compiler(s).


include ./makedefaults

# Define the source file directories
SOURCEDIR1=../src
SOURCEDIR2=../../../src/Utilities

VPATH = \
${SOURCEDIR1} \
${SOURCEDIR2} 

.SUFFIXES: .f90 .o

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/compilerversion.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/defmacro.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/genericutils.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Message.o \
$(OBJDIR)/version.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/sort.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/grb.o \
$(OBJDIR)/budgetdata.o \
$(OBJDIR)/Budget.o \
$(OBJDIR)/BlockParser.o \
$(OBJDIR)/ArrayReaders.o \
$(OBJDIR)/zone.o \
$(OBJDIR)/zoneoutput.o \
$(OBJDIR)/zbud6.o

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(OPTLEVEL) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(OPTLEVEL) $(FFLAGS) -c $< -o $@ $(INCSWITCH) $(MODSWITCH)

