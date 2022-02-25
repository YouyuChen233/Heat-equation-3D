/*!
* \file bicgstab_solve.cpp
* \brief Das BiCG-Verfahren 
* 
*
* Ein iteratives numerisches Verfahren 
* zur approximativen Lösung eines linearen Gleichungssystems:
* \f$A*\vec{x}=\vec b, A\in \mathbb{R}^{n*n} \f$
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
/// \brief Funktion des BiCG-Verfahrens
/// \param ndim Anzahl der Matrix und des Vektors
/// \param **a  2-Dimension Matrix(linke Seite)
/// \param *b   Vektor(rechte Seite)
/// \param *x   unbekanter Vektor
/// \param **PTetr Punktnummern im Tetraeder
/// \return Keine Rückgabe, aber ein Gnuplot-Skript wird aufgerufen um den Fehler zu sehen
int bicgstab_solve(Netzdim Ndim, Netz N, RandPkt RP, OptGLS optg){
	double alpha;		///< alpha:   Reelle Zahl \f$ \alpha \f$
	double beta; 		///< beta:    Reele Zahl \f$ \beta \f$
	double delta;		///< delta:   Fehler \f$ \Delta \f$
	double omega;		///< omega:   Reelle Zahl \f$ \omega \f$
	double rho;  		///< rho:     Reelle Zahl \f$ \rho \f$
	double rho_alt;		///< rho_alt: Reelle Zahl \f$ \rho_{alt}\f$
	double eps;  		///< eps:     Genauigkei \f$ \epsilon \f$
	double u;    		///< u:       Standard Lösung 
	double xnetz;		///< xnetz:   X-Koordinaten
	double ynetz;		///< ynetz:   Y-Koordinaten
	double znetz;		///< znetz:   Z-Koordinaten 

	int ndim=RP.pmRdim;     ///< ndim    : optimierte Dimension
	double **a=optg.SmR;    ///< **a     : Linke Seite Matrix
	double *b=optg.bmR;     ///< *b      : Rechte Seite Vektor
	double *x=optg.xmR;     ///< *x      : Unbekanter Vektor
	double **PTetr=optg.PmR;///< **PTetr : optimierte Punkte  
	
	double *p=new double[ndim];	///< *p:    Vektor \f$ \vec p\f$
	double *r=new double[ndim];    	///< *r:    Vektor \f$ \vec r\f$
	double *r0=new double[ndim];	///< *r0:   Vektor \f$ \vec {r_{0}}\f$
	double *v=new double[ndim];	///< *v:    Vektor \f$ \vec v \f$
	double *s=new double[ndim];	///< *s:    Vektor \f$ \vec s\f$
	double *t=new double[ndim];	///< *t:    Vektor \f$ \vec t\f$
	double *tmp1=new double [ndim]; ///< *tmp1: Zwischen Vektor \f$ \overrightarrow{tmp1}\f$
	double *tmp2=new double [ndim];	///< *tmp2: Zwischen Vektor \f$ \overrightarrow{tmp2}\f$

	int it;				///< it:    Zähler für Iterationsschleife
	clock_t st0,ste;		///< st0,ste:       Zeit Messung des Verfahrens
	double cpu_sekunden;		///< cpu_sekunden:  Zeitdauer des Verfahrens
	cout<<" BiCGStab_solve : ndim= "<<ndim<<endl;
        eps=numeric_limits<double>::epsilon(); ///  Genauigkeit der double Zahl
	eps=eps*0.01;
	eps=eps*0.01;
	cout<<" BiCGStab_solve : relative machine precision epsilon eps= "<<eps<<endl;
	cout<<" BiCGStab_solve : Vor Conjugate-Gradient-Algorithmus"<<endl;
	/// BiCG-Verfahren anfangen:
	st0=clock();
	/// Initialisiren \f$ \vec x=3.0\f$
	for(int i=0;i<ndim;i++){
		x[i]=3.0;
	}
	p=vktminus(b,matmulvkt(a,x,ndim,ndim,ndim),ndim,ndim); 	/// Setze \f$ \vec p=\vec b-A*\vec x \f$
	r=vktminus(b,matmulvkt(a,x,ndim,ndim,ndim),ndim,ndim); 	/// Setze \f$ \vec b=\vec b-A*\vec x \f$
	r0=r;							/// Setze \f$ \vec r_0=\vec r \f$
	rho=dot_product(r,r,ndim,ndim);				/// Setze \f$ \rho = \vec r \cdot \vec r \f$
	it=0;
	cout<<" BiCGStab_solve : Vor der Iterationsschleife"<<endl;
	/// Iterationsschleife anfangen:
	while(dot_product(r,r,ndim,ndim) > eps){		/// proof \f$ \vec r\cdot\vec r >\epsilon ?\f$
		v=matmulvkt(a,p,ndim,ndim,ndim);		/// \f$ \vec v=A*\vec p\f$
		alpha=rho/dot_product(v,r0,ndim,ndim);		/// \f$ \alpha = \rho /(\vec v \cdot \vec r_0)\f$
		s=vktminus(r,constmulvkt(alpha,v,ndim),ndim,ndim);	/// \f$ \vec s=\vec r- \alpha*\vec v\f$
		t=matmulvkt(a,s,ndim,ndim,ndim);		/// \f$ \vec t= A*\vec s\f$
		/// \f$\omega=\frac{\vec t\cdot\vec s}{\vec t\cdot\vec t}\f$
		omega=dot_product(t,s,ndim,ndim)/dot_product(t,t,ndim,ndim); 
		x=vktplus(x,constmulvkt(alpha,p,ndim),ndim,ndim); 
		/// \f$ \vec x_{k+1}=\vec x_k +\alpha * \vec p +\omega*\vec s \f$
		x=vktplus(x,constmulvkt(omega,s,ndim),ndim,ndim);
		/// \f$ \vec r=\vec s- \omega*\vec t\f$
		r=vktminus(s,constmulvkt(omega,t,ndim),ndim,ndim);
		rho_alt=rho; /// \f$ \rho_k \quad speichern \f$
		rho=dot_product(r,r0,ndim,ndim);/// \f$ \rho_{k+1}=\vec r\cdot\vec r_0\f$
		beta=alpha/omega*rho/rho_alt;   /// \f$ \beta=\alpha/\omega*\rho_{k+1}/\rho_k\f$
		tmp1=vktminus(p,constmulvkt(omega,v,ndim),ndim,ndim);
		tmp2=constmulvkt(beta,tmp1,ndim);
		p=vktplus(r,tmp2,ndim,ndim);    /// \f$ \vec p=\vec r+\beta*(\vec p-\omega*\vec v)\f$
		it++;
		cout.setf(ios::scientific);
		cout<<"BiCGStab_solve : it = "<<it<<" proof = "<<setprecision(8)<<dot_product(r,r,ndim,ndim)<<endl;
	}
	cout<<"BiCGStab_solve : Nach der Iterationsschleife"<<endl;
	ste=clock();
	cpu_sekunden=(double)(ste-st0)/CLOCKS_PER_SEC; /// Zeitdauer des Verfahrens
	cout<<"BiCGStab_solve : Nach Conjugate-Gradient-Algorithmus"<<endl;
        if(it>ndim){
		cout<<"BiCGStab_solve : Nicht konvergiert --> it ="<<it<<endl;
	}
	else{
		cout<<"BiCGStab_solve :       konvergiert --> it ="<<it<<endl;
	}
	cout<<" BiCGStab_solve : Dazu wurden  "<<cpu_sekunden<<" CPU--Sekunden benoetigt."<<endl;
	ofstream fin;
	fin.open("fort.33",ios::out | ios::trunc); /// Die Lösung wird in "fort.33" ausgegeben
	int j=0;

	
	for(int i=0;i<Ndim.n_tetr_pkt;i++){
		if(RP.InnerePkt[i] < 0){
			/// Lösung am Rand ausgeben
			xnetz=N.P[i][0];
			ynetz=N.P[i][1];
			znetz=N.P[i][2];
			u=20.0-2.0*pow(ynetz,2)+pow(xnetz,3)*ynetz-xnetz*pow(ynetz,3)+\
			           pow(znetz,3)*xnetz-znetz*pow(xnetz,3);
			delta=u-optg.x[i];
			fin<<i<<" "<<xnetz<<" "<<ynetz<<" "<<znetz<<" "<<optg.x[i]<<" "<<u<<" "<<delta<<endl;
		}
		else{ 
			/// Lösung in der inneren Punkte ausgeben
			xnetz=PTetr[j][0];
			ynetz=PTetr[j][1];
			znetz=PTetr[j][2];
			u=20.0-2.0*pow(ynetz,2)+pow(xnetz,3)*ynetz-xnetz*pow(ynetz,3)+\
			           pow(znetz,3)*xnetz-znetz*pow(xnetz,3);
			delta=u-x[j];
			fin<<i<<" "<<xnetz<<" "<<ynetz<<" "<<znetz<<" "<<x[j]<<" "<<u<<" "<<delta<<endl;
			j++;


		}
	}
	fin.close();
	/// Gnuplot-Skript "cg_Tetraed.dem" aufrufen
	system("gnuplot cg_Tetraed.dem");
	return 0;
}



















