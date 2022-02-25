CC=g++
CFLAGS=-c -O3
fn=FEM_OBJ
SRC=${fn}.cpp dim.cpp  input.cpp randpunkte.cpp \
              SteifigkeitsMatrix.cpp Flaechen_RB.cpp bicgstab_solve.cpp matmulvkt.cpp\
	      dot_product.cpp vktminus.cpp vktplus.cpp constmulvkt.cpp matopt.cpp
OBJ=${fn}.o   dim.o    input.o    randpunkte.o      \
              SteifigkeitsMatrix.o Flaechen_RB.o     bicgstab_solve.o matmulvkt.o      \
	      dot_product.o vktminus.o vktplus.o constmulvkt.o matopt.o
${fn}.exe: $(OBJ)
	@echo "FEM.mk : Try to link the execytable !"
	$(CC) $(OBJ) -o ${fn}.exe
${fn}.o: ${fn}.cpp
	$(CC) ${CFLAGS} ${fn}.cpp
dim.o: dim.cpp
	$(CC) ${CFLAGS} dim.cpp
input.o: input.cpp
	$(CC) ${CFLAGS} input.cpp
randpunkte.o: randpunkte.cpp
	$(CC) ${CFLAGS} randpunkte.cpp
SteifigkeitsMatrix.o: SteifigkeitsMatrix.cpp
	$(CC) ${CFLAGS} SteifigkeitsMatrix.cpp
Flaechen_RB.o: Flaechen_RB.cpp
	$(CC) ${CFLAGS} Flaechen_RB.cpp
bicgstab_solve.o: bicgstab_solve.cpp
	$(CC) ${CFLAGS} bicgstab_solve.cpp
matmulvkt.o: matmulvkt.cpp
	$(CC) ${CFLAGS} matmulvkt.cpp
dot_product.o: dot_product.cpp
	$(CC) ${CFLAGS} dot_product.cpp
vktminus.o: vktminus.cpp
	$(CC) ${CFLAGS} vktminus.cpp
vktplus.o: vktplus.cpp
	$(CC) ${CFLAGS} vktplus.cpp
constmulvkt.o: constmulvkt.cpp
	$(CC) ${CFLAGS} constmulvkt.cpp
matopt.o: matopt.cpp
	$(CC) ${CFLAGS} matopt.cpp
clean:
	rm fort.* *.txt *.o ${fn}.exe
