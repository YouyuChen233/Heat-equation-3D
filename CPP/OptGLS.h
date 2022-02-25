/*!
* \file OptGLS.h
* \brief Klasse das optimierte Gleichungssystem
*
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/

#include<iostream>
using namespace std;
class OptGLS{
	public:
		int ndim;      ///< ndim  : Dimension
		double *bmR;   ///< *bmR  : optimierte \f$ \vec b \f$
		double **SmR;  ///< **SmR : optimierte Steifigkeitsmatrix
		double *x;     ///< *x    : unbekannter Vektor \f$ \vec x \f$
		double *xmR;   ///< *xmR  : optimierter unbekannter Vektor \f$ \vec x \f$
		double **PmR;  ///< **PmR : neu nummerierte innere Punkte
};
