/*!
* \file GLS.h
* \brief Klasse Gleichungssystem
*
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/

#include<iostream>
using namespace std;
class GLS{
	public:
		int ndim;         ///< ndim     : Dimension
		double *bglobal;  ///< *bglobal : globale rechte Seite \f$ \vec b \f$
		double **Sglobal; ///< **Sglobal: globale Steifigkeitsmatrix
};
