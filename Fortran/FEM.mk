 CF=gfortran
 FFLAGS=-g  -O3             -DCyy -Dplevel1 -DkubLoes -Dbicgstab -DQUADER
# FFLAGS=-g  -fbounds-check -DPiepke -Dplevel1 -DkubLoes -Dlusolve -DQUADER
 #FFLAGS=-g  -fbounds-check -DCyy -Dplevel1 -DkubLoes -Dbicgstab -DWSZ_by_16
fn=FEM
lapackLib=/home/youyu/Dokumente/lapack-3.7.1
 SRC=${fn}.F90 bicgstab_solve.F90 cg_solve.F90 DPRINTR.F90 Flaechen_RB.F90 lu_solve.F90 \
               randpunkte.F90 RBdimension.F90 SteifigkeitsMatrix.F90 tetraeder_dim1.F90 tetraeder_dim.F90 \
	       tetraeder_input1.F90 tetraeder_input.F90 skalar.F90 matmulvkt.F90 matopt.F90
 OBJ=${fn}.o bicgstab_solve.o cg_solve.o DPRINTR.o Flaechen_RB.o lu_solve.o randpunkte.o \
             RBdimension.o SteifigkeitsMatrix.o tetraeder_dim1.o tetraeder_dim.o \
	     tetraeder_input1.o tetraeder_input.o skalar.o matmulvkt.o matopt.o
#

${fn}.exe:	$(OBJ)
	@echo " FEM_3D_Tetraeder+LAPACK.mk : Try to link the executable !"
	$(CF) ${FFLAGS} -o ${fn}.exe $(OBJ)  ${lapackLib}/liblapack.a ${lapackLib}/librefblas.a
	@echo " FEM_3D_Tetraeder+LAPACK.mk : the executable is linked !"

clean:
	rm $(OBJ) ${fn}.exe fort.*
${fn}.o : ${fn}.F90
	@echo " $< wird compiliert --> Objectdatei"
	$(CF) ${FFLAGS} -c $<

bicgstab_solve.o : bicgstab_solve.F90
	@echo "bicgstab_solve.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c bicgstab_solve.F90

cg_solve.o : cg_solve.F90
	@echo "cg_solve.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c cg_solve.F90

DPRINTR.o : DPRINTR.F90
	@echo "DPRINTR.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c DPRINTR.F90

Flaechen_RB.o : Flaechen_RB.F90
	@echo "Flaechen_RB.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c Flaechen_RB.F90

lu_solve.o : lu_solve.F90
	@echo "lu_solve.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c lu_solve.F90

randpunkte.o : randpunkte.F90
	@echo "randpunkte.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c randpunkte.F90

RBdimension.o : RBdimension.F90
	@echo "RBdimension.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c RBdimension.F90

SteifigkeitsMatrix.o : SteifigkeitsMatrix.F90
	@echo "SteifigkeitsMatrix.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c SteifigkeitsMatrix.F90

tetraeder_dim1.o : tetraeder_dim1.F90
	@echo "tetraeder_dim1.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c tetraeder_dim1.F90

tetraeder_input.o : tetraeder_input.F90
	@echo "tetraeder_input.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c tetraeder_input.F90

tetraeder_dim.o : tetraeder_dim.F90
	@echo "tetraeder_dim.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c tetraeder_dim.F90

tetraeder_input1.o : tetraeder_input1.F90
	@echo "tetraeder_input.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c tetraeder_input1.F90
skalar.o : skalar.F90
	@echo "skalar.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c skalar.F90

matmulvkt.o : matmulvkt.F90
	@echo "matmulvkt.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c matmulvkt.F90

matopt.o : matopt.F90
	@echo "matopt.F90 wird compiliert --> Objectdatei :"
	$(CF) ${FFLAGS} -c matopt.F90

