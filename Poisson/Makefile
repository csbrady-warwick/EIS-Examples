  EIS_DIR = ../EIS-2
  INC = -I
  EIS_INCLUDE = $(INC)$(EIS_DIR)/common/include $(INC)$(EIS_DIR)/parser/include
  EIS_INCLUDE += $(INC)$(EIS_DIR)/deck/include
  LIB = -L
  EIS_LIB = $(LIB)$(EIS_DIR)/common/lib $(LIB)$(EIS_DIR)/parser/lib
  EIS_LIB += $(LIB)$(EIS_DIR)/deck/lib -leis_deck -leis_parser -leis_common

.PHONY: lib

poisson-eis:poisson-eis.o
	gfortran -O3 poisson-eis.o $(EIS_INCLUDE) $(EIS_LIB) -opoisson-eis
lib:
	$(MAKE) -C ../EIS-2
poisson-eis.o:poisson-eis.f90 lib
	gfortran -O3 -c poisson-eis.f90 $(EIS_INCLUDE)
clean:
	$(MAKE) -C ../EIS-2 clean
	-rm *.o
	-rm *.mod
	-rm poisson-eis
