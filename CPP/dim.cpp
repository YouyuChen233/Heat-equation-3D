/*!
 * \file dim.cpp
 * \brief Anzahl der Punkten und Tetraederelement bestimmen
 *
 * Ein "*.msh" File wird als Input eingelesen
 * 
 *
 * \author Youyu Chen
 * \version 1.0
 * \date 09.08.2018
 */

#include<iostream>
#include<string>
#include<fstream>
#include"funktions.h"
using namespace std;
/// \brief Funktion um die Anzahl der Punkten zu bestimmen
/// \param &str Name des Inputfiles
/// \return geben die Klasse Netzdim(Netzdimension) zurück
Netzdim dim(string &str){
	int j; ///< dim: Zähler der Punkten
	int a;   ///< a: 1.Spalt des Files
	int b;   ///< b: 2.Spalt des Files
	Netzdim netzdim;
	fstream fin(str.c_str());
	if(fin){
		string s;
		for(int i=1;i<=4;i++) 
			getline(fin,s);
		fin>>j; /// Die Anzahl steht in die 5.Zeile des Files, kann direkt eingelesen werden
		netzdim.n_tetr_pkt=j;
		getline(fin,s);
		for(int i=0;i<j+3;i++)
			getline(fin,s);
		j=0;
		/// 1. Spalt ist die Nummer des Elements \n
		/// 2. Spalt ist der Typ des Elements \n
		/// Typ Nummer: \n
		/// 1:  Linie(2 Knoten) \n
		/// 2:  Dreiecke(3 Knoten) \n
		/// 3:  Vierecke(4 Knoten) \n
		/// 4:  Tetraeder(4 Knoten) \n
		/// 5:  Hexaeder(8 Knoten) \n
		/// 6:  Prisma(6 Knoten) \n
		/// 7:  Pyramide(5 Knoten) \n
		/// 8:  Linie 2.Ordnung(3 Knoten) \n
		/// 9:  Dreiecke 2.Ordnung(6 Knoten) \n
		/// 11: Tetraeder 2.Ordnung(10 Knoten) \n
		/// 15: Punkt(1 Knoten) \n
		while(fin){
			fin>>a>>b;getline(fin,s);
			if(a!=0&&b==4)
				j++;	
		}
		netzdim.n_tetr=j;
	}
	else
		cerr<<"Fail"<<endl;
	fin.close();
	return netzdim;
}

