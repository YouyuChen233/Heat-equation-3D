/*!
* \file vktplus.cpp
* \brief Ein Unterprogramm für \f$ \vec a+\vec b\f$
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
/// \brief Funktion \f$ \vec a+\vec b\f$
/// 
/// Dimension \f$\vec a\f$ soll gleich wie \f$\vec b\f$ sein
/// \param *a Vektor
/// \param *b Vektor
/// \param arow Dimension des Vektors a
/// \param brow Dimension des Vektors b
/// \return geben den Lösung Vektor zurück
/// \see vktminus()
double *vktplus(double *a,double *b,int arow,int brow){
	double *out=new double [arow]; ///< *out: der Lösung-Vektor
	if(arow==brow){
		for(int i=0;i<arow;i++){
			out[i]=a[i]+b[i]; /// \f$ out_i=a_i*b_i \f$
		}
	}
	else
		cerr<<"arow!=brow"<<endl; ///Fehlermeldung
	return out;
}
