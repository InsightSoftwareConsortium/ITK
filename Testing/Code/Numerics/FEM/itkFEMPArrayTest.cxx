/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMPArrayTest.cxx
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

#include <iostream>
#include "itkFEMLoadImplementationGenericLandmarkLoad.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEM.h"
#include "itkFEMLinearSystemWrapperItpack.h"


//
int itkFEMPArrayTest(int, char*[])
{

    typedef itk::fem::Node        NodeType;
    typedef itk::fem::Element     ElementType;
    typedef NodeType::ArrayType   ArrayType;

    typedef itk::fem::FEMP<NodeType>  FEMPointer;


    ArrayType  array;

    NodeType::Pointer n1 = NodeType::New();
    ElementType::VectorType pt(2);

    pt[0]=0.;
    pt[1]=0.;
    n1->SetCoordinates(pt);
    array.push_back( FEMPointer(&*n1));

    n1=NodeType::New();
    pt[0]=1.;
    pt[1]=1.;
    n1->SetCoordinates(pt);
    array.push_back( FEMPointer(&*n1));

    n1=NodeType::New();
    pt[0]=3.;
    pt[1]=2.;
    n1->SetCoordinates(pt);
    array.push_back( FEMPointer(&*n1));

    n1=NodeType::New();
    pt[0]=0.;
    pt[1]=3.;
    n1->SetCoordinates(pt);
    array.push_back( FEMPointer(&*n1));

    array.Renumber();

    std::cout << "Nodes\n";


    try 
      { 
      NodeType::Pointer node = &*array.Find(0);
      }
    catch ( itk::ExceptionObject &e) 
      {
      std::cout << "Exception caught: " << e << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


