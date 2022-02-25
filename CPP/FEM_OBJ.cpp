/*!
 * \file FEM_OBJ.cpp
 * \brief Hauptprogramm FEM_Tetraeder
 *
 * \author Youyu Chen
 * \version 1.0
 * \date 09.08.2018
 */

#include<cmath>
#include<fstream>
#include<iostream>
#include<string>
#include"funktions.h"
using namespace std;
/// \brief main Funktion
int main(){
	Netzdim netzdim; ///< netzdim : Netzdimension
	Netz    netz;    ///< netz    : Netz
	RandPkt RP;      ///< RP      : Randpunkte
	GLS     G;       ///< G       : Gleichungssystem
	OptGLS  Opt;     ///< Opt     : optimiertes Gleichungssystem
	
	string inputfile="Part3.msh";	 /// Name des Inputfiles
	
	netzdim=dim(inputfile);		/// Netzdimension bestimmen
	cout<<"n_tetr_pkt="<<netzdim.n_tetr_pkt<<endl;
	cout<<"n_tetr="<<netzdim.n_tetr<<endl;

	netz=input(inputfile,netzdim);  /// Input-File einlesen


	
	RP=randpunkte(inputfile,netzdim, netz);
	cout<<"*****************************************"<<endl;
	cout<<"main : Randpunktnummern abgespeichert ! *"<<endl;
	cout<<"*****************************************"<<endl;

	cout<<"pmRdim = "<<RP.pmRdim<<endl;
	
	G=SteifigkeitsMatrix(netzdim,netz); /// globale Steifigkeismatrix und Rechteseite b berechnen


	G=Flaechen_RB(netzdim, netz, RP, G);

	Opt=matopt(netzdim, netz, RP, G); /// Matrix verkuerzen
	Opt.xmR=new double[RP.pmRdim]; /// optimierte unbekante Vektor x dimensionieren
	bicgstab_solve(netzdim,netz,RP,Opt); /// BiCG Verfahren aufrufen
	return 0;
}
