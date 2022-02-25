/*!
* \file Netz.h
* \brief Klasse Netz
*
*
* \author Youyu Chen
* \version 1.0
* \date 09.08.2018
*/

#include<iostream>
using namespace std;
class Netz{
	public:
		double **P;  ///< **P: Koordinatenssystem der Punkte
		int    **T;  ///< **T: Tetraederelement(besteht aus 4 Knotennummer)
};
