  EIS_DIR = ../EIS-2
  INC = -I
  EIS_INCLUDE = $(INC)$(EIS_DIR)/common/include $(INC)$(EIS_DIR)/parser/include
  EIS_INCLUDE += $(INC)$(EIS_DIR)/deck/include
  LIB = -L
  EIS_LIB = $(LIB)$(EIS_DIR)/common/lib $(LIB)$(EIS_DIR)/parser/lib
  EIS_LIB += $(LIB)$(EIS_DIR)/deck/lib -leis_deck -leis_parser -leis_common

main:poisson-eis.o
	gfortran -O3 poisson-eis.o $(EIS_INCLUDE) $(EIS_LIB) -opoisson-eis
poisson-eis.o:poisson-eis.f90
	gfortran -O3 -c poisson-eis.f90 $(EIS_INCLUDE)
clean:
	-rm *.o
	-rm *.mod
	-rm poisson-eis
