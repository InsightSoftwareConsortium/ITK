/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMPArrayTest.cxx
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

#include <iostream>
#include "itkFEMLoadImplementationGenericLandmarkLoad.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEM.h"


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
      array.Find(0);
      array.Find(1);
      array.Find(2);
      array.Find(3);
      }
    catch ( itk::ExceptionObject &e) 
      {
      std::cout << "Exception caught: " << e << std::endl;
      return EXIT_FAILURE;
      }


    // try an element with GN larger than the array size
    n1=NodeType::New();
    pt[0]=0.;
    pt[1]=3.;
    n1->SetCoordinates(pt);
    n1->GN = 200;
    array.push_back( FEMPointer(&*n1));

    NodeType::Pointer node;

    try 
      { 
      node = &*array.Find(200);
      }
    catch ( itk::ExceptionObject &e) 
      {
      std::cout << "Exception caught: " << e << std::endl;
      return EXIT_FAILURE;
      }


    try 
      { 
      // Intentionally fail, by asking for a non-existing element
      node = &*array.Find(1000);
      std::cout << "Error: exception should have been thrown here... " << std::endl;
      return EXIT_FAILURE;
      }
    catch ( itk::ExceptionObject &e) 
      {
      std::cout << "Passed Exception test: " << e << std::endl;
      }


    // Use the node in order to avoid warning for unused variable
    ElementType::VectorType coordinates = node->GetCoordinates();

    std::cout << "Coordinates = " << std::endl;
    for( unsigned int c=0; c<coordinates.size(); c++)
      {
      std::cout << coordinates[c] << "  " << std::endl; 
      }

    std::cout << "Test PASSED!\n";
    return EXIT_SUCCESS;
}


