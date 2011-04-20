FC=sunf95
FFLAGS=

INCDIR=../include
INC=-M$(INCDIR)

OBJS = flu_binding.o lua_fif.o

libflu.a: $(INCDIR) $(OBJS)
	ar rc libflu.a $(OBJS)

$(INCDIR):
	mkdir -p $(INCDIR)

flu_binding.o $(INCDIR)/flu_binding.mod: flu_binding.f90 $(INCDIR)/lua_fif.mod
	$(FC) -c $(FFLAGS) $(INC) flu_binding.f90
	mv flu_binding.mod $(INCDIR)

lua_fif.o $(INCDIR)/lua_fif.mod: lua_fif.f90
	$(FC) -c $(FFLAGS) $(INC) lua_fif.f90
	mv lua_fif.mod $(INCDIR)
