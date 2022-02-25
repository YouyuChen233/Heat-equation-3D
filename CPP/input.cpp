/*!
* \file input.cpp
* \brief Punkte und Tetraederelement einlesen
*
* Ein "*.msh" File wird als Input eingelesen
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
/// \brief Funktion um alle Punkte einzulesen
/// \param &str Name der Input-File
/// \param netzdim Die Klasse Netzdim(Netzdimension)
Netz input(string &str,Netzdim netzdim){
	Netz netz;
	int idummy1=0; ///< idummy1: Element Index
	int idummy2=0; ///< idummy2: Element Typ
	int idummy3=0; ///< idummy3: Nummer der tags
	int idummy4=0; ///< idummy4: <tags>
	int idummy5=0; ///< idummy5: Knoten Nummer-List
	int dim_element;
	int j=0;
	netz.P = new double*[netzdim.n_tetr_pkt]; ///< **p: Punktefeld dimensionieren
	for (int i = 0; i < netzdim.n_tetr_pkt; i++)
	{
		netz.P[i] = new double[3];
	}
	netz.T=new int*[netzdim.n_tetr+1];
	for(int i=0;i<netzdim.n_tetr+1;i++){
		netz.T[i]=new int[4];
	}
	netz.T[0][0]=1;
	netz.T[0][1]=2;
	netz.T[0][2]=3;
	netz.T[0][3]=4;
	int idummy=0; ///< idummy: Punktenummer
	double a,b,c,d; ///< a,b,c   : XYZ-Koordinatenssystem wenn Punkte einlesen
	                ///< a,b,c,d : 4 Knotennummer wenn Tetraeder einlesen
	fstream fin(str.c_str());
	if(fin){
		string s;
		for(int i=1;i<=5;i++){
			getline(fin,s);
		}
		for(int i=0;i<netzdim.n_tetr_pkt;i++){
			fin>>idummy>>a>>b>>c;getline(fin,s); /// XYZ-Koordinatenssystem einlesen
			netz.P[i][0]=a;
			netz.P[i][1]=b;
			netz.P[i][2]=c;
		}
		getline(fin,s);
		getline(fin,s);
		fin>>dim_element; /// Anzahle aller Elemente einlesen
		getline(fin,s);
                for(int i=1;i<dim_element+1;i++){
                        fin>>idummy1>>idummy2>>idummy3>>idummy4>>idummy5;
                        if(idummy2==4){
                                j++;
                                fin>>a>>b>>c>>d; /// 4 Knotennummer einlesen
                                netz.T[j][0]=a;
                                netz.T[j][1]=b;
                                netz.T[j][2]=c;
                                netz.T[j][3]=d;
                        }
                        getline(fin,s);
                }


	}
	else
		cerr<<"Fail"<<endl; /// Fehlermeldung

	fin.close();
	return netz;

}
