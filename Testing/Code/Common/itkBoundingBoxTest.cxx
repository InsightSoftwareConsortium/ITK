/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkBoundingBoxTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

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
  itk::Point<1,double> P;

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
    double coord;
    coord = (double) i;
    P.SetCoords ( &coord );
    Points->InsertElement ( i, P );
    }
  std::cout << "Insert ponits passed" <<std::endl;

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
  return 0;
}
