/*!
* \file matmulvkt.cpp
* \brief Ein Unterprogramm für Matrix*Vektor
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
/// \brief Funktion Matrix*Vektor \n
/// Anzahl des Matrix-Spalts soll gleich die Vektor-Dimension sein
/// \param **a Matrix a
/// \param *b Vektor b
/// \param arow Anzahl der Zeile a
/// \param acol Anzahl des Spalts a
/// \param brow Vektor-Dimension
/// \return geben einen neuen Vektor zurück
double *matmulvkt(double **a,double *b,int arow,int acol,int brow){
	double *out=new double[arow]; ///< *out: der Lösung-Vektor
	for(int i=0;i<arow;i++){
		out[i]=0.0;
	}
	if(acol==brow){
		for(int i=0;i<arow;i++){
			for(int j=0;j<acol;j++){
				out[i]+=a[i][j]*b[j]; /// \f$ \overrightarrow{out}=A*\vec b\f$
			}
		}
	}
	else 
		cerr<<"acol!=brow"<<endl; /// Fehlermeldung
	return out;
}
