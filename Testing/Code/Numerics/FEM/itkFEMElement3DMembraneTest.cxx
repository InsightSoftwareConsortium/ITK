/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElement3DMembraneTest.cxx
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

#include "itkFEMElement3DC0LinearHexahedronMembrane.h"
#include "itkFEMElementBase.h"

#include <iostream>


//
int itkFEMElement3DMembraneTest(int, char *[])
{
    typedef itk::fem::Node        NodeType;
    typedef itk::fem::Element     ElementType;

    NodeType::Pointer n0,n1,n2,n3,n4,n5,n6,n7;
    ElementType::VectorType pt(3);

    n0=NodeType::New();
    pt[0]=0.;
    pt[1]=0.;
    pt[2]=0.;
    n0->SetCoordinates(pt);

    n1=NodeType::New();
    pt[0]=1.;
    pt[1]=0.;
    pt[2]=0.;
    n1->SetCoordinates(pt);

    n2=NodeType::New();
    pt[0]=1.;
    pt[1]=1.;
    pt[2]=0.;
    n2->SetCoordinates(pt);

    n3=NodeType::New();
    pt[0]=0.;
    pt[1]=1.;
    pt[2]=0.;
    n3->SetCoordinates(pt);

    n4=NodeType::New();
    pt[0]=0.;
    pt[1]=0.;
    pt[2]=1.;
    n4->SetCoordinates(pt);

    n5=NodeType::New();
    pt[0]=1.;
    pt[1]=0.;
    pt[2]=1.;
    n5->SetCoordinates(pt);

    n6=NodeType::New();
    pt[0]=1.;
    pt[1]=1.;
    pt[2]=1.;
    n6->SetCoordinates(pt);

    n7=NodeType::New();
    pt[0]=0.;
    pt[1]=1.;
    pt[2]=1.;
    n7->SetCoordinates(pt);

    typedef itk::fem::MaterialLinearElasticity   ElasticityType;
    ElasticityType::Pointer m = ElasticityType::New();
    m->GN=0;
    m->E=10000.0;
    m->A=0.02;
    m->I=0.004;
    m->nu=0.4;

    typedef itk::fem::Element3DC0LinearHexahedronMembrane ElementMembraneType;
    ElementMembraneType::Pointer e0 = ElementMembraneType::New();

    e0->GN=0;
    e0->SetNode(0, &*n0);
    e0->SetNode(1, &*n1);
    e0->SetNode(2, &*n2);
    e0->SetNode(3, &*n3);
    e0->SetNode(4, &*n4);
    e0->SetNode(5, &*n5);
    e0->SetNode(6, &*n6);
    e0->SetNode(7, &*n7);
    e0->m_mat=dynamic_cast< ElasticityType * >(&*m);

    ElementType::MatrixType D, Me;

    e0->GetMassMatrix(Me);
    e0->GetMaterialMatrix(D);
    std::cout << "Mass matrix: " << std::endl << Me << std::endl;
    std::cout << "Material matrix: " << std::endl << D << std::endl;
    std::cout << "#dof per node = " << e0->GetNumberOfDegreesOfFreedomPerNode() << std::endl;

#ifndef FEM_USE_SMART_POINTERS
    delete e0;
    delete m;
    delete n0;
    delete n1;
    delete n2;
    delete n3;
    delete n4;
    delete n5;
    delete n6;
    delete n7;
#endif

    std::cout << "Test PASSED!" << std::endl;;
    return EXIT_SUCCESS;
}


