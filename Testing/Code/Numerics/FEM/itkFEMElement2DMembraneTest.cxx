/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DMembraneTest.cxx
  Language:  C++
  Date: $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElementBase.h"

#include <iostream>

using namespace std;
using namespace itk;
using namespace fem;

//
int itkFEMElement2DMembraneTest(int, char *[])
{
    Node::Pointer n0,n1,n2,n3;
    Element::VectorType pt(2);

    n0=Node::New();
    pt[0]=0.;
    pt[1]=0.;
    n0->SetCoordinates(pt);

    n1=Node::New();
    pt[0]=1.;
    pt[1]=1.;
    n1->SetCoordinates(pt);

    n2=Node::New();
    pt[0]=3.;
    pt[1]=2.;
    n2->SetCoordinates(pt);

    n3=Node::New();
    pt[0]=0.;
    pt[1]=3.;
    n3->SetCoordinates(pt);

    MaterialLinearElasticity::Pointer m;
    m=MaterialLinearElasticity::New();
    m->GN=0;
    m->E=30000.0;
    m->A=0.02;
    m->I=0.004;

    Element2DC0LinearQuadrilateralMembrane::Pointer e0 = Element2DC0LinearQuadrilateralMembrane::New();

    e0->GN=0;
    e0->SetNode(0, &*n0);
    e0->SetNode(1, &*n1);
    e0->SetNode(2, &*n2);
    e0->SetNode(3, &*n3);
    e0->m_mat=dynamic_cast<MaterialLinearElasticity*>(&*m);

    Element::MatrixType D, Me;

    e0->GetMassMatrix(Me);
    e0->GetMaterialMatrix(D);
    cout << "Mass matrix: " << endl << Me << endl;
    cout << "Material matrix: " << endl << D << endl;
    cout << "#dof per node = " << e0->GetNumberOfDegreesOfFreedomPerNode() << endl;

#ifndef FEM_USE_SMART_POINTERS
    delete e0;
    delete m;
    delete n0;
    delete n1;
    delete n2;
    delete n3;
#endif
    
    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


