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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif'
#include <iostream>
#include "itkBoundingBox.h"

int itkBoundingBoxTest (int, char*[])
{
  // Test out the bounding box code

  typedef itk::BoundingBox<unsigned long, 1, double> BB;
  BB::Pointer myBox = BB::New();

  BB::PointsContainerPointer Points = BB::PointsContainer::New();

  int i;
  itk::Point<double, 1> P;

  std::cout << "Testing Bounding Box" <<std::endl;
  {
  const BB::BoundsArrayType & bounds = myBox->GetBounds();
  for(unsigned int i=0; i< 1 * 2; i++)
    {
    if( bounds[i] != itk::NumericTraits< BB::CoordRepType >::Zero )
      {
      std::cerr << "Bounding Box initialization test failed" << std::endl;
      std::cerr << bounds << std::endl;
      return EXIT_FAILURE;
      }   
    }
  }
  std::cout << "Null GetBoundingBox test passed" <<std::endl;
  
  {
  BB::PointType center = myBox->GetCenter();
  for(unsigned int i=0; i< 1; i++)
    {
    if( center[i] != itk::NumericTraits< BB::CoordRepType >::Zero )
      {
      std::cerr << "Empty Box GetCenter initialization test failed" << std::endl;
      return EXIT_FAILURE;
      }   
    }
  std::cout << "Null GetCenter test passed" <<std::endl;
  }

  
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
  {
  const BB::BoundsArrayType & bounds = myBox->GetBounds();
  if( ( bounds[0] != 0.0 )  || ( bounds[1] != 9.0 ) )
    {
    std::cerr << "Bounding Box initialization test failed" << std::endl;
    std::cerr << bounds << std::endl;
    return EXIT_FAILURE;
    }   
  std::cout << "GetBoundingBox passed" <<std::endl;
  }


  {
  BB::PointType center = myBox->GetCenter();
  for(unsigned int i=0; i< 1; i++)
    {
    if( center[i] != 4.5 )
      {
      std::cerr << "Empty Box GetCenter initialization test failed" << std::endl;
      return EXIT_FAILURE;
      }   
    }
  std::cout << "Null GetCenter test passed" <<std::endl;
  }




  itk::NumericTraits<double>::AccumulateType diagonal;
  diagonal = myBox->GetDiagonalLength2();
  if ( diagonal != 81.0 )
    {
    return 1;
    }
  std::cout << "GetDiagonalLength2 passed" << std::endl;
  
  BB::PointsContainerConstPointer NewPoints = myBox->GetPoints();

  // End with a Print.
  myBox->Print( std::cout );


  // Test the IsInside method in 3D
  std::cout << " Some Testing in 3D " <<std::endl;

  typedef itk::BoundingBox<unsigned long, 3, double> CC;
  CC::Pointer my3DBox = CC::New();

  CC::PointsContainerPointer Points3D = CC::PointsContainer::New();

  CC::PointType::ValueType qval1[3] = {-1.0f, -1.0f, -1.0f};
  CC::PointType Q = qval1;
  Points3D->InsertElement( 0, Q );

  CC::PointType::ValueType qval2[3] = {1.0f, 1.0f, 1.0f};
  Q =  qval2;
  Points3D->InsertElement( 1, Q );
  std::cout << "Insert points passed" <<std::endl;

  my3DBox->SetPoints ( Points3D );
  if ( !my3DBox->ComputeBoundingBox() )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Compute Bounding Box passed" <<std::endl;

  CC::PointType::ValueType qval3[3] = {0.0f, 0.0f, 0.0f};
  Q = qval3;
  if( !my3DBox->IsInside( Q ) )
    {
    std::cerr << "Point " << Q << " Should be repoted inside " << std::endl;
    return EXIT_FAILURE;
    }

  CC::PointType::ValueType qval4[3] = {2.0f, 0.0f, 0.0f};
  Q = qval4;
  if( my3DBox->IsInside( Q ) )
    {
    std::cerr << "Point " << Q << " Should be repoted outside " << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "BoundingBox test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}

