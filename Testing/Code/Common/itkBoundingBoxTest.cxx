/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBoxTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkBoundingBox.h"

int main ( int argc, char* argv[] )
{
  // Test out the bounding box code

  typedef itk::BoundingBox<unsigned long, 1, double> BB;
  BB::Pointer myBox = BB::New();

  BB::PointsContainerPointer Points = BB::PointsContainer::New();
  BB::PointsContainerPointer NewPoints = BB::PointsContainer::New();

  int i;
  itk::Point<double, 1> P;

  double coordinates[2];
  std::cout << "Testing Bounding Box" <<std::endl;
  if ( myBox->GetBoundingBox ( coordinates ) != NULL )
    {
    return 1;
    }
  std::cout << "Null GetBoundingBox test passed" <<std::endl;
  
  if ( myBox->GetCenter ( coordinates ) != NULL )
    {
    return 1;
    }
  std::cout << "Null GetCenter test passed" <<std::endl;
  
  if ( myBox->GetDiagonalLength2 ( ) != itk::NumericTraits<double>::Zero )
    {
    return 1;
    }
  std::cout << "Null GetDiagonalLength2 test passed" <<std::endl;
  
  if ( myBox->GetPoints () )
    {
    return 1;
    }
  std::cout << "Null GetPoints test passed" <<std::endl;

  
  for ( i = 0; i < 10; i++ )
    {
    P[0] = (double)i;
    Points->InsertElement ( i, P );
    }
  std::cout << "Insert points passed" <<std::endl;

  myBox->SetPoints ( Points );
  if ( !myBox->ComputeBoundingBox() )
    {
    return 1;
    }
  std::cout << "Compute Bounding Box passed" <<std::endl;

  // Now we should have something
  if ( myBox->GetBoundingBox ( coordinates ) == NULL )
    {
    return 1;
    }
  std::cout << "GetBoundingBox passed" <<std::endl;

  std::cout << "Got: " << coordinates[0] << ", " << coordinates[1] << std::endl;
  if ( coordinates[0] != 0.0 || coordinates[1] != 9.0 )
    {
    return 1;
    }
  std::cout << "Correct BoundingBox passed" <<std::endl;

  if ( myBox->GetCenter ( coordinates ) == NULL )
    {
    return 1;
    }
  std::cout << "GetCenter passed" << std::endl;

  if ( coordinates[0] != 4.5 )
    {
    return 1;
    }

  itk::NumericTraits<double>::AccumulateType diagonal;
  diagonal = myBox->GetDiagonalLength2();
  if ( diagonal != 81.0 )
    {
    return 1;
    }
  std::cout << "GetDiagonalLength2 passed" << std::endl;
  
  NewPoints = myBox->GetPoints();

  // End with a Print.
  myBox->Print( std::cout );


  // Test the IsInside method in 3D
  std::cout << " Some Testing in 3D " <<std::endl;

  typedef itk::BoundingBox<unsigned long, 3, double> CC;
  CC::Pointer my3DBox = CC::New();

  CC::PointsContainerPointer Points3D = CC::PointsContainer::New();

  CC::PointType Q;
  Q = -1.0f, -1.0f, -1.0f;
  Points3D->InsertElement( 0, Q );

  Q =  1.0f,  1.0f,  1.0f;
  Points3D->InsertElement( 1, Q );
  std::cout << "Insert points passed" <<std::endl;

  my3DBox->SetPoints ( Points3D );
  if ( !my3DBox->ComputeBoundingBox() )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Compute Bounding Box passed" <<std::endl;

  Q = 0.0f, 0.0f, 0.0f;
  if( !my3DBox->IsInside( Q ) )
    {
    std::cerr << "Point " << Q << " Should be repoted inside " << std::endl;
    return EXIT_FAILURE;
    }

  Q = 2.0f, 0.0f, 0.0f;
  if( my3DBox->IsInside( Q ) )
    {
    std::cerr << "Point " << Q << " Should be repoted outside " << std::endl;
    return EXIT_FAILURE;
    }



  return EXIT_SUCCESS;
}

