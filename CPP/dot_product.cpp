/*!
* \file dot_product.cpp
* \brief Skalarprodukt
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
/// \brief Funktion Skalarprodukt
/// \param *a Vektor a
/// \param *b Vektor b
/// \param arow Dimension des Vektors a
/// \param brow Dimension des Vektors b
/// \return geben die Lösung \f$ \vec a\cdot\vec b\f$ zurück
double dot_product(double *a,double *b,int arow,int brow){
	double out=0; ///< out: Die Lösung
	if(arow==brow){
		for(int i=0;i<arow;i++){
			out+=a[i]*b[i]; /// \f$ \sum_{i=1}^{n}a_i*b_i\f$
		}
	}
	else
		cerr<<" Vecot arow!= brow"<<endl;
	return out;
}
