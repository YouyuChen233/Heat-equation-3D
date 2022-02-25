/*!
* \file funktions.h
* \brief Funktionen declaration
*
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/

#include<string>
#include<iostream>
#include"Netzdim.h"
#include"Netz.h"
#include"RandPkt.h"
#include"GLS.h"
#include"OptGLS.h"
using namespace std;
Netzdim dim(string &str);
Netz input(string &str,Netzdim netzdim);
RandPkt randpunkte(string &str, Netzdim ndim, Netz n);
GLS SteifigkeitsMatrix(Netzdim ndim,Netz n);
GLS Flaechen_RB( Netzdim ndim, Netz n, RandPkt RP, GLS g);
OptGLS matopt(Netzdim ndim, Netz n, RandPkt RP, GLS g);
int bicgstab_solve(Netzdim Ndim, Netz N, RandPkt RP, OptGLS optg);
double *matmulvkt(double **a,double *b,int arow,int acol,int brow);
double dot_product(double *a,double *b,int arow,int brow);
double *vktminus(double *a,double *b,int arow,int brow);
double *vktplus(double *a,double *b,int arow,int brow);
double *constmulvkt(double a,double *b,int brow);
