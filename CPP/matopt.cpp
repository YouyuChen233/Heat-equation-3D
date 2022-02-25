/*!
* \file matopt.cpp
* \brief Steifigkeitsmatrix und rechte Seite b verkürzen
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/

#include<iostream>
#include<string>
#include<fstream>
#include<cmath>
#include<time.h>
#include<stdlib.h>
#include<limits>
#include <iomanip>
#include"funktions.h"
using namespace std;
/// \brief Funktion um die Matrix zu verkürzen
/// \param ndim Dimension des Netzes
/// \param n    Netz
/// \param RP   Randpunkte
/// \param GLS  Gleichungssystem
/// \return geben die Klasse OptGLS(Optimiertes Gleichungssystem) zurück
OptGLS matopt(Netzdim ndim, Netz n, RandPkt RP, GLS g){
	OptGLS optg; ///< optg: optimiertes Gleichungssystem
	
	int j=0;  
	int m=0;
	int r=0;
	
	clock_t st0,ste;     ///< st0,ste      : Zeit Messung für die Optimierung
	double cpu_sekunden; ///< cpu_sekunden : Zeitdauer der Optimierung
	
	double **tmp=new double *[RP.pmRdim]; ///< **tmp : Zwischen Matrix 
	for(int i=0;i<RP.pmRdim;i++){
		tmp[i]=new double[ndim.n_tetr_pkt];
	}
	double *tmp2=new double [RP.pmRdim];  ///< **tmp2: Zwischen Matrix

	optg.PmR=new double *[RP.pmRdim];     ///  optimierte Punktematrix PmR
	for(int i=0;i<RP.pmRdim;i++){
		optg.PmR[i]=new double[3];
	}	
	optg.bmR=new double [RP.pmRdim];      ///  optimierte rechte Seite \f$ \overrightarrow{bmR}\f$
	optg.SmR=new double *[RP.pmRdim];     ///  optimierte Steifigkeitsmatrix SmR
	for(int i=0;i<RP.pmRdim;i++){
		optg.SmR[i]=new double[RP.pmRdim];
	}
	optg.x=new double[ndim.n_tetr_pkt];   ///  unbekannte Vektor \f$ \vec x \f$
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		optg.x[i]=0.0;                ///  \f$ \vec x \f$ initialisieren
	}
	j=0;
	m=0;
	cout<<" Matrix verkürzen............"<<endl;
	st0=clock();
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		if(RP.InnerePkt[i]<0){
			optg.x[i]=g.bglobal[i]; /// die Temperatur am Rand ist schon bekannt
			j=j+1;
		}
		else{
			optg.bmR[m]=g.bglobal[i];
			for(int q=0;q<ndim.n_tetr_pkt;q++){
				tmp[m][q]=g.Sglobal[i][q];
			}
			/// Innere Punkte neu nummerieren ==> Punktematrix verkürzen
			optg.PmR[m][0]=n.P[i][0]; 
			optg.PmR[m][1]=n.P[i][1];
			optg.PmR[m][2]=n.P[i][2];
			m++;
		}
	}
	tmp2=matmulvkt(tmp,optg.x,RP.pmRdim,ndim.n_tetr_pkt,ndim.n_tetr_pkt);
	optg.bmR=vktminus(optg.bmR,tmp2,RP.pmRdim,RP.pmRdim);
	m=0;
	r=0;
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		for(j=0;j<ndim.n_tetr_pkt;j++){
			if(RP.InnerePkt[i] > 0 && RP.InnerePkt[j] > 0){
				optg.SmR[m][r]=g.Sglobal[RP.InnerePkt[i]-1][RP.InnerePkt[j]-1];
				r++;
				if(r>=RP.pmRdim){
					r=0;
					m++;
				}
			}
		}
	}
	ste=clock();
	cpu_sekunden=(double)(ste-st0)/CLOCKS_PER_SEC; /// Zeitdauer der Optimierung
	cout<<" ==> Matrix optimiert : Dazu wurden  "<<cpu_sekunden<<"  CPU--Sekunden benoetigt."<<endl;
	return optg;
}
