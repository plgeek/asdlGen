TARGET        =	zsuif_gateway
SUPER_PACKAGES_PATH = basesuif
LIBS          =	-lsuifpasses -lsuif -lpyg -lzot -lwallow -lsty
MANPAGES      =	zsuif_gateway.1
SRCS          =	zsuif.cxx StdTypes.cxx suif_base.cxx zsuif_gateway.cc \
		trans_suif.cc \
                trans_type.h  \
	        trans_statement.h \
	        trans_instruction.h

OBJS          =	zsuif.o StdTypes.o suif_base.o zsuif_gateway.o trans_suif.o 
AUTOINITLIB   =  yes
LOCALINCLDIRS = -I./ -I$(ASDL_HOME)/include/asdlGen 
LOCALLIBDIRS  = -L$(ASDL_HOME)/lib/asdlGen  -lasdl -lcii
ASDLGEN       = $(ASDL_HOME)/bin/asdlGen

all:	pass
install-bin: install-pass
include $(SUIFHOME)/Makefile.std


zsuif.cxx zsuif.hxx zsuif.typ: zsuif.asdl ../std-types.asdl 
		$(ASDLGEN) --cxx zsuif.asdl ../std-types.asdl \
	        --base_include="suif_base.hxx" --output_directory=./
		$(ASDLGEN) --sml zsuif.asdl ../std-types.asdl \
	        --output_directory=./
		$(ASDLGEN) --typ zsuif.asdl ../std-types.asdl \
	        --output_file=zsuif.typ --output_directory=./



%.o : %.cxx
	@rm -f $*.o
	$(CXX) -c $(CXXFLAGS) $<

CXXFLAGS      += -w 


