/*!
* \file constmulvkt.cpp
* \brief Ein Unterprogramm für konstant*Vektor
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
/// \brief Funktion Konstant*Vektor
/// \param a ein Konstant
/// \param *b Vektor
/// \param brow Dimension des Vektors
/// \return geben den Lösung Vektor zurück
double *constmulvkt(double a,double *b,int brow){
	double *out=new double[brow]; ///< *out: der Lösung-Vektor
	for(int i=0;i<brow;i++){
		out[i]=a*b[i];        ///  \f$ out_i=a*b_i \f$
	}
	return out;
}
