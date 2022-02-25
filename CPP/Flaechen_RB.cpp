/*!
* \file Flaechen_RB.cpp
* \brief Flächen Randbedingung
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/
#include<iostream>
#include<string>
#include<fstream>
#include<cmath>
#include"funktions.h"
using namespace std;
/// \brief Funktion um Flächen Randbedingung zu berechnen \n
/// Multiplikation der Steifigkeitsmatrix mit \f$ T_k\f$ in der ki_ten Spalte \n
/// Randbedingung an der k. Randflaeche \n
/// (k=1 : linke   Tetraederflaeche) \n
/// (k=2 : untere  Tetraederflaeche) \n
/// (k=3 : vordere Tetraederflaeche) \n
/// (k=4 : rechte  Tetraederflaeche) 
/// 
/// \param NRanddim Anzahl der Randpunkten
/// \param n_tetr_pkt Anzahl aller Punkten
/// \param **PNummerF Randpunkte
/// \param **P Alle Punkte
/// \param *bglobal globale b-Matrix
/// \param *InnerePkt Innere Punkte
/// \param **Sglobal globale Steifigkeitsmatrix
/// \return geben die neue globale Matrix(Sglobal und bglobal) zurück
GLS Flaechen_RB(Netzdim ndim, Netz n, RandPkt RP, GLS g){
	double urand;	///< urand: kubische Lösung
	double x,y,z;	///< x, y, z: XYZ-Kooridinaten
	int f1zaehler;  ///< f1zaehler: Punkte auf Randflächen \f$ F_1 \f$
	int f2zaehler;  ///< f2zaehler: Punkte auf Randflächen \f$ F_2 \f$
	int f3zaehler;  ///< f3zaehler: Punkte auf Randflächen \f$ F_3 \f$
	int f4zaehler;  ///< f4zaehler: Punkte auf Randflächen \f$ F_4 \f$
	int f5zaehler;  ///< f5zaehler: Punkte auf Randflächen \f$ F_5 \f$
	int f6zaehler;  ///< f6zaehler: Punkte auf Randflächen \f$ F_6 \f$
	int ki;
	f1zaehler=0;
	f2zaehler=0;
	f3zaehler=0;
	f4zaehler=0;
	f5zaehler=0;
	f6zaehler=0;
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		if(RP.InnerePkt[i] < 0){
			ki=-1*RP.InnerePkt[i];			
			x=n.P[ki-1][0];
			y=n.P[ki-1][1];
			z=n.P[ki-1][2];
			/// kubische Lösung: \n
			/// \f$urand=20-2*y^2+x^3*y-x*y^3+z^3*x-z*x^3\f$
			urand=20.0-2.0*y*y+pow(x,3)*y-x*pow(y,3)+pow(z,3)*x-z*pow(x,3);
			for(int j=0;j<ndim.n_tetr_pkt;j++){
			/// Hier werden alle Komponenten belegt!
				g.bglobal[j]=g.bglobal[j]-g.Sglobal[j][ki-1]*urand;
				g.Sglobal[j][ki-1]=0.0;
				g.Sglobal[ki-1][j]=0.0;
			}
			g.Sglobal[ki-1][ki-1]=1.0;
		}

	}
	f1zaehler = 0; 
	f2zaehler = 0;
	f3zaehler = 0; 
	f4zaehler = 0; 
	f5zaehler = 0; 
	f6zaehler = 0; 
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		if(RP.InnerePkt[i] < 0){
			ki=-1*RP.InnerePkt[i];			
			x=n.P[ki-1][0];
			y=n.P[ki-1][1];
			z=n.P[ki-1][2];
			urand=20.0-2.0*y*y+pow(x,3)*y-x*pow(y,3)+pow(z,3)*x-z*pow(x,3);
			/// Hier wird nur die ki-te Komponente belegt!
			g.bglobal[ki-1]=urand;
		}

	}
	return g;
}
