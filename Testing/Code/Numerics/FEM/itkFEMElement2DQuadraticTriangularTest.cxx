/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement2DQuadraticTriangularTest.cxx
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

#include "itkFEMElement2DC0QuadraticTriangularStrain.h"
#include "itkFEMElementBase.h"

#include <iostream>

using namespace std;
using namespace itk;
using namespace fem;

//
int itkFEMElement2DQuadraticTriangularTest(int, char *[])
{
    Node::Pointer n0,n1,n2;
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
    pt[0]=0.;
    pt[1]=2.;
    n2->SetCoordinates(pt);

    MaterialLinearElasticity::Pointer m;
    m=MaterialLinearElasticity::New();
    m->GN=0;
    m->E=300.0;
    m->A=0.02;
    m->I=0.004;

    Element2DC0QuadraticTriangularStrain::Pointer e0 = Element2DC0QuadraticTriangularStrain::New();

    e0->GN=0;
    e0->SetNode(0, &*n0);
    e0->SetNode(1, &*n1);
    e0->SetNode(2, &*n2);
    e0->m_mat=dynamic_cast<MaterialLinearElasticity*>(&*m);

    pt[0]=0.5;
    pt[1]=0.5;

    cout << "# integration points = " << e0->GetNumberOfIntegrationPoints(2) << endl;
    cout << "shape fxns at " << pt << ":\n" << e0->ShapeFunctions(pt) << endl;

    Element::MatrixType shapeD;
    e0->ShapeFunctionDerivatives(pt, shapeD);
    cout << "shape fxn derivatives:" << endl << shapeD << endl;

    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


