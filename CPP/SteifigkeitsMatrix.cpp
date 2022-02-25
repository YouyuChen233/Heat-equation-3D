/*!
* \file SteifigkeitsMatrix.cpp
* \brief Steifigkeitsmatrix berechnen
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
/// \brief Funktion um die Steifigkeismatrix zu berechnen
/// \param n_tetr_pkt Anzahl aller Punkte
/// \param n_tetr Anzahl der Tetraeder
/// \param **T Tetraeder Element
/// \param **P Punkte mit Koordinatenssystem
/// \return geben die Klasse GLS(Gleichungssystem) zurück
GLS SteifigkeitsMatrix(Netzdim ndim,Netz n){
	GLS g;
	g.bglobal=new double[ndim.n_tetr_pkt];	   /// rechte Seite \f$ \vec b \f$dimensionieren
        g.Sglobal=new double*[ndim.n_tetr_pkt];    /// Steifigkeitsmatrix dimensionieren
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		g.Sglobal[i]=new double[ndim.n_tetr_pkt];
	}
	int Ndim=ndim.n_tetr_pkt;     ///< Ndim: Dimension
	int ti;
	int l;
	int r;
	double det_Phi; 	      ///< det_Phi: Determinante \f$\phi\f$
	double one_by_Phi;            ///< one_by_Phi: \f$\frac{1}{det\phi}\f$
	double a,b,c,d,e,f;
	double x21,x31,x41;
	double y21,y31,y41;
	double z21,z31,z41;
	double Pinv11,Pinv12,Pinv13;
	double Pinv21,Pinv22,Pinv23;
	double Pinv31,Pinv32,Pinv33;
	double q;
	double Slocal[4][4];	      ///< Slocal[4][4]: locale Steifigkeitsmatrix
	double blocal[4];	      ///< blocal[4]   : locale b-Matrix
	/// Slocal[4][4] initialisieren
	for(int i=0;i<Ndim;i++){
		for(int j=0;j<Ndim;j++){
		 	g.Sglobal[i][j]=0.0;
		}
	}
	/// blocal[4] initialisieren
	for(int j=0;j<Ndim;j++){
		g.bglobal[j]=0.0;
	}
	det_Phi=0.0;
	q=0.0;
	for(ti=0;ti<ndim.n_tetr;ti++){
		x21=n.P[ n.T[ti+1][1]-1 ][0] - n.P[ n.T[ti+1][0]-1 ][0]; /// \f$x_2-x_1\f$
		y21=n.P[ n.T[ti+1][1]-1 ][1] - n.P[ n.T[ti+1][0]-1 ][1]; /// \f$y_2-y_1\f$
		z21=n.P[ n.T[ti+1][1]-1 ][2] - n.P[ n.T[ti+1][0]-1 ][2]; /// \f$z_2-z_1\f$

		x31=n.P[ n.T[ti+1][2]-1 ][0] - n.P[ n.T[ti+1][0]-1 ][0]; /// \f$x_3-x_1\f$
		y31=n.P[ n.T[ti+1][2]-1 ][1] - n.P[ n.T[ti+1][0]-1 ][1]; /// \f$y_3-y_1\f$
		z31=n.P[ n.T[ti+1][2]-1 ][2] - n.P[ n.T[ti+1][0]-1 ][2]; /// \f$z_3-z_1\f$

		x41=n.P[ n.T[ti+1][3]-1 ][0] - n.P[ n.T[ti+1][0]-1 ][0]; /// \f$x_4-x_1\f$
		y41=n.P[ n.T[ti+1][3]-1 ][1] - n.P[ n.T[ti+1][0]-1 ][1]; /// \f$y_4-y_1\f$
		z41=n.P[ n.T[ti+1][3]-1 ][2] - n.P[ n.T[ti+1][0]-1 ][2]; /// \f$z_4-z_1\f$

		det_Phi=x21*y31*z41+x31*y41*z21+x41*y21*z31 - x41*y31*z21-x31*y21*z41-x21*y41*z31;
		if(det_Phi < 0.0){
			cout<<"SteifigkeitsMatrix : det_Phi= "<<det_Phi<<endl;
			cout<<"SteifigkeitsMatrix : Tetr.Nr= "<<ti<<endl;
			cout<<"SteifigkeitsMatrix : negative Orientierung"<<endl;
			cout<<"SteifigkeitsMatrix : ====> Exit"<<endl;
		}
		one_by_Phi = 1.0 / det_Phi;
		Pinv11 = (y31 * z41 - y41 * z31);
		Pinv12 = (x41 * z31 - x31 * z41);
		Pinv13 = (x31 * y41 - x41 * y31);
		Pinv21 = (y41 * z21 - y21 * z41);
		Pinv22 = (x21 * z41 - x41 * z21);
		Pinv23 = (x41 * y21 - x21 * y41);
		Pinv31 = (y21 * z31 - y31 * z21);
		Pinv32 = (x31 * z21 - x21 * z31);
		Pinv33 = (x21 * y31 - x31 * y21);
		a = Pinv11*Pinv11 + Pinv12*Pinv12 + Pinv13*Pinv13;
		b = Pinv21*Pinv21 + Pinv22*Pinv22 + Pinv23*Pinv23;
		c = Pinv31*Pinv31 + Pinv32*Pinv32 + Pinv33*Pinv33;
		d = Pinv11*Pinv21 + Pinv12*Pinv22 + Pinv13*Pinv23;
		e = Pinv11*Pinv31 + Pinv12*Pinv32 + Pinv13*Pinv33;
		f = Pinv21*Pinv31 + Pinv22*Pinv32 + Pinv23*Pinv33;
		Slocal[0][0]=a+b+c+2.0*(d+e+f);
		Slocal[0][1]=-a-d-e;
		Slocal[0][2]=-b-d-f;
		Slocal[0][3]=-c-e-f;
		Slocal[1][0]=Slocal[0][1];
		Slocal[2][0]=Slocal[0][2];
		Slocal[3][0]=Slocal[0][3];
		Slocal[1][1]=a;
		Slocal[1][2]=d;
		Slocal[1][3]=e;
		Slocal[2][1]=d;
		Slocal[2][2]=b;
		Slocal[2][3]=f;
		Slocal[3][1]=e;
		Slocal[3][2]=f;
		Slocal[3][3]=c;
		for(int i=0;i<4;i++){
			for(int j=0;j<4;j++){
				Slocal[i][j]=Slocal[i][j]*one_by_Phi; /// vermeintlicher Fehler <== ELMER
			}
		}
		for(int i=0;i<4;i++){
			blocal[i]=1.0;
		}
		for(int i=0;i<4;i++){
			l=n.T[ti+1][i]-1;
			for(int j=0;j<i+1;j++){
				r=n.T[ti+1][j]-1;
				g.Sglobal[l][r]=g.Sglobal[l][r]+Slocal[i][j];
				g.Sglobal[r][l]=g.Sglobal[l][r];
			}
			g.bglobal[l]=g.bglobal[l]+det_Phi*blocal[i];
		}
}
	cout<<" SteifigkeitsMatrix : Kubisch.Loesungsfunktionsvorgabe "<<endl;
	q=4.0; /// \f$u(x,y,z) = 20 - 2*y^2 + x^3*y - x*y^3 + z^3*x - z*x^3 \f$
	        
	for(int i=0;i<Ndim;i++){
		/// Rechte Seite * q=4 und die 1/6 der Steifigkeitsmatrix auf die andere Seite
		g.bglobal[i]=q/24.0*g.bglobal[i];
	}
	for(int i=0;i<Ndim;i++){
		for(int j=0;j<Ndim;j++){
			g.Sglobal[i][j]=1.0/6.0*g.Sglobal[i][j];
		}
	}
	return g;
}
