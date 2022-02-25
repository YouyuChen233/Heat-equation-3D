/*!
* \file RBdimension.cpp
* \brief Anzahl der Randpunkte bestimmen
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/
#include<iostream>
#include<algorithm>
#include<cmath>
using namespace std;
/// \brief Funktion um Anzahl der Randpunkte zu bestimmen
/// \param n_tetr_pkt Anzahl aller Punkte
/// \param **p Punkte mit Koordinatenssystem
/// \return geben die Anzahl der Randpunkte zurück
int RBdimension(int n_tetr_pkt,double **p){
	int NRanddim=0; ///< NRanddim: Anzahl der Randpunkt
	int f1zaehler; 	///< f1zaehler: Punkte auf Randflächen \f$ F_1 \f$
	int f2zaehler; 	///< f2zaehler: Punkte auf Randflächen \f$ F_2 \f$
	int f3zaehler; 	///< f3zaehler: Punkte auf Randflächen \f$ F_3 \f$
	int f4zaehler; 	///< f4zaehler: Punkte auf Randflächen \f$ F_4 \f$
	int f5zaehler; 	///< f5zaehler: Punkte auf Randflächen \f$ F_5 \f$
	int f6zaehler; 	///< f6zaehler: Punkte auf Randflächen \f$ F_6 \f$
	double eps=0.0000001; ///< eps:  Genauigkeit
	double xmax=-10000.0; ///< xmax: Größte Zahl in X-Richtung
	double xmin=10000.0;  ///< xmin: kleinste Zahl in X-Richtung
	double ymax=-10000.0; ///< ymax: Größte Zahl in Y-Richtung
	double ymin=10000.0;  ///< ymin: kleinste Zahl in Y-Richtung
	double zmax=-10000.0; ///< zmax: Größte Zahl in Z-Richtung
	double zmin=10000.0;  ///< zmin: kleinste Zahl in Z-Richtung
	/// Randflächen suchen
	for(int i=0;i<n_tetr_pkt;i++){
		xmax=max(xmax,p[i][0]);
		xmin=min(xmin,p[i][0]);
		ymax=max(ymax,p[i][1]);
		ymin=min(ymin,p[i][1]);
		zmax=max(zmax,p[i][2]);
		zmin=min(zmin,p[i][2]);
	}
	f1zaehler=0;
	f2zaehler=0;
	f3zaehler=0;
	f4zaehler=0;
	f5zaehler=0;
	f6zaehler=0;
	for(int i=0;i<n_tetr_pkt;i++){
		if(abs(p[i][1]-ymax) <= eps) /// Punkte auf Randflächen \f$ F_1 \f$(ymax)
			f1zaehler++;
		if(abs(p[i][1]-ymin) <= eps) /// Punkte auf Randflächen \f$ F_2 \f$(ymin)
			f2zaehler++;
		if(abs(p[i][0]-xmax) <= eps) /// Punkte auf Randflächen \f$ F_3 \f$(xmax)
			f3zaehler++;
		if(abs(p[i][0]-xmin) <= eps) /// Punkte auf Randflächen \f$ F_4 \f$(xmin)
			f4zaehler++;
		if(abs(p[i][2]-zmax) <= eps) /// Punkte auf Randflächen \f$ F_5 \f$(zmax)
			f5zaehler++;
		if(abs(p[i][2]-zmin) <= eps) /// Punkte auf Randflächen \f$ F_6 \f$(zmin)
			f6zaehler++;
	}
	cout<<"NBdimension : f1zaehler= "<<f1zaehler<<endl;
	cout<<"NBdimension : f2zaehler= "<<f2zaehler<<endl;
	cout<<"NBdimension : f3zaehler= "<<f3zaehler<<endl;
	cout<<"NBdimension : f4zaehler= "<<f4zaehler<<endl;
	cout<<"NBdimension : f5zaehler= "<<f5zaehler<<endl;
	cout<<"NBdimension : f6zaehler= "<<f6zaehler<<endl;
	NRanddim=max(NRanddim,f1zaehler);
	NRanddim=max(NRanddim,f2zaehler);
	NRanddim=max(NRanddim,f3zaehler);
	NRanddim=max(NRanddim,f4zaehler);
	NRanddim=max(NRanddim,f5zaehler);
	NRanddim=max(NRanddim,f6zaehler);
	cout<<"NBdimension : NRanddim = "<<NRanddim<<endl;
	return NRanddim;
}
