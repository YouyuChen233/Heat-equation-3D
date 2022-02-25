/*!
 * \file randpunkte.cpp
 * \brief Randpunkte bestimmen
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
/// \brief Funktion um alle Randpunkte zu bestimmen \n
/// diese Funktion ist gleich wie randpunkte_InnerePkt() \n
/// der Unterschied dazwischen ist nur die Rückgabe
/// \param n_tetr_pkt Anzahl aller Punkten
/// \param NRanddim Dimension des Randpunkts
/// \param **p Punkte mit Koordinatenssystem
/// \return geben die Klasse RandPkt(Randpunkte) zurück
RandPkt randpunkte(string &str, Netzdim ndim, Netz n){
	RandPkt RP;
	int dim_element;
	int idummy1=0; ///< idummy1: Element Index
	int idummy2=0; ///< idummy2: Element Typ
	int idummy3=0; ///< idummy3: Nummer der tags
	int idummy4=0; ///< idummy4: <tags>
	int idummy5=0; ///< idummy5: Knoten Nummer-List
	int a,b,c;
	RP.pmRdim=0;
	RP.InnerePkt=new int[ndim.n_tetr_pkt];
	for(int i=0;i<ndim.n_tetr_pkt;i++)
		RP.InnerePkt[i]=i+1;
	fstream fin(str.c_str());
	string s;
	for(int i=0;i<ndim.n_tetr_pkt+7;i++){
		getline(fin,s);
	}
	fin>>dim_element;getline(fin,s);
	cout<<"s = "<<s<<"dim_element = "<<dim_element<<endl;
	for(int i=0;i<dim_element;i++){
		fin>>idummy1>>idummy2>>idummy3>>idummy4>>idummy5;
		if(idummy2==15){
			fin>>a;getline(fin,s);
			if(RP.InnerePkt[a-1]>0)
				RP.InnerePkt[a-1]=-1*RP.InnerePkt[a-1];
		}
		else if(idummy2==1){
			fin>>a>>b;getline(fin,s);
			if(RP.InnerePkt[a-1]>0)
				RP.InnerePkt[a-1]=-1*RP.InnerePkt[a-1];
			if(RP.InnerePkt[b-1]>0)
				RP.InnerePkt[b-1]=-1*RP.InnerePkt[b-1];
		} 
		else if(idummy2==2){
			fin>>a>>b>>c;getline(fin,s);
			if(RP.InnerePkt[a-1]>0)
				RP.InnerePkt[a-1]=-1*RP.InnerePkt[a-1];
			if(RP.InnerePkt[b-1]>0)
				RP.InnerePkt[b-1]=-1*RP.InnerePkt[b-1];
			if(RP.InnerePkt[c-1]>0)
				RP.InnerePkt[c-1]=-1*RP.InnerePkt[c-1];
		}
		else
			getline(fin,s);

	}
	ofstream fio;
	fio.open("InnerePkt.txt",ios::out | ios::trunc);
	for(int i=0;i<ndim.n_tetr_pkt;i++){
		if(RP.InnerePkt[i]<0)
		RP.pmRdim++;
		fio<<i<<" "<<RP.InnerePkt[i]<<endl;
	}
	RP.pmRdim=ndim.n_tetr_pkt-RP.pmRdim;
	return RP;
}
