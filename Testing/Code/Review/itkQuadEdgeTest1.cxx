/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkQuadEdge.h"
#include <iostream>

int itkQuadEdgeTest1( int , char* [] )
{
  typedef itk::QuadEdge        QuadEdgeType;

  // Tests for the GetRot() SetRot() methods
  { // create a local scope for these tests
  QuadEdgeType * quadEdge1 = new QuadEdgeType;
  QuadEdgeType * quadEdge2 = new QuadEdgeType;
  QuadEdgeType * quadEdge3 = new QuadEdgeType;

  quadEdge1->GetRot();

  // Verify that it can be set.
  quadEdge1->SetRot( quadEdge2 );
  if( quadEdge1->GetRot() != quadEdge2 )
    {
    std::cerr << "Error in SetRot() / GetRot() " << std::endl;
    return EXIT_FAILURE;
    }

  // Verify that it can be changed.
  quadEdge1->SetRot( quadEdge3 );
  if( quadEdge1->GetRot() != quadEdge3 )
    {
    std::cerr << "Error in SetRot() / GetRot() " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;

  delete quadEdge1;
  delete quadEdge2;
  delete quadEdge3;
  } // end of local scope for tests

  // Tests for the GetOnext() SetOnext() methods
  { // create a local scope for these tests
  QuadEdgeType * quadEdge1 = new QuadEdgeType;
  QuadEdgeType * quadEdge2 = new QuadEdgeType;
  QuadEdgeType * quadEdge3 = new QuadEdgeType;

  quadEdge1->GetOnext();

  // Verify that it can be set.
  quadEdge1->SetOnext( quadEdge2 );
  if( quadEdge1->GetOnext() != quadEdge2 )
    {
    std::cerr << "Error in SetOnext() / GetOnext() " << std::endl;
    return EXIT_FAILURE;
    }

  // Verify that it can be changed.
  quadEdge1->SetOnext( quadEdge3 );
  if( quadEdge1->GetOnext() != quadEdge3 )
    {
    std::cerr << "Error in SetOnext() / GetOnext() " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;

  delete quadEdge1;
  delete quadEdge2;
  delete quadEdge3;
  } // end of local scope for tests


  // Tests for the IsEdgeIn*() methods
  { // create a local scope for these tests
  QuadEdgeType * quadEdge1 = new QuadEdgeType;
  QuadEdgeType * quadEdge2 = new QuadEdgeType;

  bool itis = quadEdge1->IsEdgeInOnextRing( quadEdge2 );
  if( itis )
    {
     
    }

  delete quadEdge1;
  delete quadEdge2;
  } // end of local scope for tests
  
  return EXIT_SUCCESS;
}

