/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DMembraneTest.cxx
  Language:  C++
  Date: $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElement3DC0LinearHexahedronMembrane.h"
#include "itkFEMElementBase.h"

#include <iostream>

using namespace std;
using namespace itk;
using namespace fem;

//
int itkFEMElement3DMembraneTest(int, char *[])
{
    Node::Pointer n0,n1,n2,n3,n4,n5,n6,n7;
    Element::VectorType pt(3);

    n0=Node::New();
    pt[0]=0.;
    pt[1]=0.;
    pt[2]=0.;
    n0->SetCoordinates(pt);

    n1=Node::New();
    pt[0]=1.;
    pt[1]=0.;
    pt[2]=0.;
    n1->SetCoordinates(pt);

    n2=Node::New();
    pt[0]=1.;
    pt[1]=1.;
    pt[2]=0.;
    n2->SetCoordinates(pt);

    n3=Node::New();
    pt[0]=0.;
    pt[1]=1.;
    pt[2]=0.;
    n3->SetCoordinates(pt);

    n4=Node::New();
    pt[0]=0.;
    pt[1]=0.;
    pt[2]=1.;
    n4->SetCoordinates(pt);

    n5=Node::New();
    pt[0]=1.;
    pt[1]=0.;
    pt[2]=1.;
    n5->SetCoordinates(pt);

    n6=Node::New();
    pt[0]=1.;
    pt[1]=1.;
    pt[2]=1.;
    n6->SetCoordinates(pt);

    n7=Node::New();
    pt[0]=0.;
    pt[1]=1.;
    pt[2]=1.;
    n7->SetCoordinates(pt);

    MaterialLinearElasticity::Pointer m;
    m=MaterialLinearElasticity::New();
    m->GN=0;
    m->E=10000.0;
    m->A=0.02;
    m->I=0.004;
    m->nu=0.4;

    Element3DC0LinearHexahedronMembrane::Pointer e0 = Element3DC0LinearHexahedronMembrane::New();

    e0->GN=0;
    e0->SetNode(0, &*n0);
    e0->SetNode(1, &*n1);
    e0->SetNode(2, &*n2);
    e0->SetNode(3, &*n3);
    e0->SetNode(4, &*n4);
    e0->SetNode(5, &*n5);
    e0->SetNode(6, &*n6);
    e0->SetNode(7, &*n7);
    e0->m_mat=dynamic_cast<MaterialLinearElasticity*>(&*m);

    Element::MatrixType D, Me;

    e0->GetMassMatrix(Me);
    e0->GetMaterialMatrix(D);
    cout << "Mass matrix: " << endl << Me << endl;
    cout << "Material matrix: " << endl << D << endl;
    cout << "#dof per node = " << e0->GetNumberOfDegreesOfFreedomPerNode() << endl;

    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


