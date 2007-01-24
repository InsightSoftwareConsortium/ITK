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
  const QuadEdgeType * quadEdge1c = quadEdge1;

  quadEdge1->GetRot(); // testing null case

  // Verify that it can be set.
  quadEdge1->SetRot( quadEdge2 );
  if( quadEdge1->GetRot() != quadEdge2 )
    {
    std::cerr << "Error in SetRot() / GetRot() " << std::endl;
    return EXIT_FAILURE;
    }
  // Test the const version
  if( quadEdge1c->GetRot() != quadEdge2 )
    {
    std::cerr << "Error in const GetRot() " << std::endl;
    return EXIT_FAILURE;
    }


  // Verify that it can be changed.
  quadEdge1->SetRot( quadEdge3 );
  if( quadEdge1->GetRot() != quadEdge3 )
    {
    std::cerr << "Error in changing SetRot() / GetRot() " << std::endl;
    return EXIT_FAILURE;
    }
  // Test the const version
  if( quadEdge1c->GetRot() != quadEdge3 )
    {
    std::cerr << "Error in changed const GetRot() " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "GetRot()/SetRot() Test passed ! " << std::endl;
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
  const QuadEdgeType * quadEdge1c = quadEdge1;

  quadEdge1->GetOnext(); // testing null case

  // Verify that it can be set.
  quadEdge1->SetOnext( quadEdge2 );
  if( quadEdge1->GetOnext() != quadEdge2 )
    {
    std::cerr << "Error in SetOnext() / GetOnext() " << std::endl;
    return EXIT_FAILURE;
    }
  // Test the const version
  if( quadEdge1c->GetOnext() != quadEdge2 )
    {
    std::cerr << "Error in const GetOnext() " << std::endl;
    return EXIT_FAILURE;
    }


  // Verify that it can be changed.
  quadEdge1->SetOnext( quadEdge3 );
  if( quadEdge1->GetOnext() != quadEdge3 )
    {
    std::cerr << "Error in changing SetOnext() / GetOnext() " << std::endl;
    return EXIT_FAILURE;
    }
  // Test the const version
  if( quadEdge1c->GetOnext() != quadEdge3 )
    {
    std::cerr << "Error changed const GetOnext() " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "GetOnext()/SetOnext() Test passed ! " << std::endl;

  delete quadEdge1;
  delete quadEdge2;
  delete quadEdge3;
  } // end of local scope for tests



 
  // Tests for the GetSym() methods
  { // create a local scope for these tests
  QuadEdgeType * quadEdge1 = new QuadEdgeType;
  QuadEdgeType * quadEdge2 = new QuadEdgeType;
  QuadEdgeType * quadEdge3 = new QuadEdgeType;
  QuadEdgeType * quadEdge4 = new QuadEdgeType;
  const QuadEdgeType * quadEdge1c = quadEdge1;
  const QuadEdgeType * quadEdge2c = quadEdge2;
  const QuadEdgeType * quadEdge3c = quadEdge3;
  const QuadEdgeType * quadEdge4c = quadEdge4;

  quadEdge1->GetSym(); // testing null case

  quadEdge1->SetRot( quadEdge2 );
  quadEdge2->SetRot( quadEdge3 );
  quadEdge3->SetRot( quadEdge4 );
  quadEdge4->SetRot( quadEdge1 );

  if( quadEdge1->GetSym() != quadEdge3 )
    {
    std::cerr << "Error in GetSym() " << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge1c->GetSym() != quadEdge3 )
    {
    std::cerr << "Error in const GetSym() " << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge2->GetSym() != quadEdge4 )
    {
    std::cerr << "Error in GetSym() B " << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge2c->GetSym() != quadEdge4 )
    {
    std::cerr << "Error in const GetSym() B" << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge3->GetSym() != quadEdge1 )
    {
    std::cerr << "Error in GetSym() C " << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge3c->GetSym() != quadEdge1 )
    {
    std::cerr << "Error in const GetSym() C" << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge4->GetSym() != quadEdge2 )
    {
    std::cerr << "Error in GetSym() D " << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge4c->GetSym() != quadEdge2 )
    {
    std::cerr << "Error in const GetSym() D" << std::endl;
    return EXIT_FAILURE;
    }

  delete quadEdge1;
  delete quadEdge2;
  delete quadEdge3;
  delete quadEdge4;

  std::cout << "GetSym() Test passed ! " << std::endl;
  }

 
  // Tests for the GetLnext() methods
  // returns the next edge with same left face
  { // create a local scope for these tests
  QuadEdgeType * quadEdge1 = new QuadEdgeType;
  QuadEdgeType * quadEdge2 = new QuadEdgeType;
  QuadEdgeType * quadEdge3 = new QuadEdgeType;
  QuadEdgeType * quadEdge4 = new QuadEdgeType;
  QuadEdgeType * quadEdgeA = new QuadEdgeType;
  QuadEdgeType * quadEdgeB = new QuadEdgeType;
  const QuadEdgeType * quadEdge1c = quadEdge1;

  quadEdge1->GetLnext(); // testing null case

  //                        /
  //                       / quadEdgeA
  //          Face        /
  //                     /
  //                    /
  //   --------------->O
  //   quadEdge1
  //
  
  quadEdgeB->SetRot( quadEdgeA );
  quadEdge4->SetOnext( quadEdgeB );

  quadEdge1->SetRot( quadEdge2 );
  quadEdge2->SetRot( quadEdge3 );
  quadEdge3->SetRot( quadEdge4 );
  quadEdge4->SetRot( quadEdge1 );

  if( quadEdge1->GetLnext() != quadEdgeA )
    {
    std::cerr << "Error in GetLnext()" << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdge1c->GetLnext() != quadEdgeA )
    {
    std::cerr << "Error in const GetLnext()" << std::endl;
    return EXIT_FAILURE;
    }

  delete quadEdge1;
  delete quadEdge2;
  delete quadEdge3;
  delete quadEdge4;
  delete quadEdgeA;
  delete quadEdgeB;

  std::cout << "GetLnext() Test passed ! " << std::endl;
  }


  // Tests for the GetRnext() methods
  // returns the next edge with same left face
  { // create a local scope for these tests
  QuadEdgeType * quadEdge1 = new QuadEdgeType;
  QuadEdgeType * quadEdge2 = new QuadEdgeType;
  QuadEdgeType * quadEdge3 = new QuadEdgeType;
  QuadEdgeType * quadEdge4 = new QuadEdgeType;
  QuadEdgeType * quadEdgeA = new QuadEdgeType;
  QuadEdgeType * quadEdgeB = new QuadEdgeType;
  const QuadEdgeType * quadEdgeAc = quadEdgeA;

  quadEdge1->GetRnext(); // testing null case

  //
  //        quadEdge1
  //   --------------------->O
  //                        /
  //                       / 
  //          Face        /
  //                     /quadEdgeA
  //                    /
  //
  
  quadEdgeA->SetRot( quadEdgeB );
  quadEdgeB->SetOnext( quadEdge2 );

  quadEdge1->SetRot( quadEdge2 );
  quadEdge2->SetRot( quadEdge3 );
  quadEdge3->SetRot( quadEdge4 );
  quadEdge4->SetRot( quadEdge1 );

  if( quadEdgeA->GetRnext() != quadEdge1 )
    {
    std::cerr << "Error in GetRnext()" << std::endl;
    return EXIT_FAILURE;
    }

  if( quadEdgeAc->GetRnext() != quadEdge1 )
    {
    std::cerr << "Error in const GetRnext()" << std::endl;
    return EXIT_FAILURE;
    }

  delete quadEdge1;
  delete quadEdge2;
  delete quadEdge3;
  delete quadEdge4;
  delete quadEdgeA;
  delete quadEdgeB;

  std::cout << "GetRnext() Test passed ! " << std::endl;
  }


  return EXIT_SUCCESS;
}

