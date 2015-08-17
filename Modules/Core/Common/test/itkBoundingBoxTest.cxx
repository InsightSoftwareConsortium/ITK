/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkBoundingBox.h"
#include "itkMath.h"

int itkBoundingBoxTest (int, char*[])
{
  // Test out the bounding box code

  typedef itk::BoundingBox<unsigned long, 1, double> BB;
  BB::Pointer myBox = BB::New();

  BB::PointsContainerPointer Points = BB::PointsContainer::New();

  itk::Point<double, 1> P;

  std::cout << "Testing Bounding Box" <<std::endl;
  {
  const BB::BoundsArrayType & bounds = myBox->GetBounds();
  for(unsigned int i=0; i< bounds.Size(); i++)
    {
    if( itk::Math::NotExactlyEquals(bounds[i], itk::NumericTraits< BB::CoordRepType >::ZeroValue()) )
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
    if( itk::Math::NotExactlyEquals(center[i], itk::NumericTraits< BB::CoordRepType >::ZeroValue()) )
      {
      std::cerr << "Empty Box GetCenter initialization test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "Null GetCenter test passed" <<std::endl;
  }


  if ( itk::Math::NotExactlyEquals(myBox->GetDiagonalLength2 ( ), itk::NumericTraits<double>::ZeroValue()) )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Null GetDiagonalLength2 test passed" <<std::endl;

  if ( myBox->GetPoints () )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Null GetPoints test passed" <<std::endl;


  for ( unsigned int i = 0; i < 10; i++ )
    {
    P[0] = (double)i;
    Points->InsertElement ( i, P );
    }
  std::cout << "Insert points passed" <<std::endl;

  myBox->SetPoints ( Points );
  if ( !myBox->ComputeBoundingBox() )
    {
    return EXIT_FAILURE;
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
    return EXIT_FAILURE;
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

  // Testing the corners
  std::cout << "Testing GetCorners() : ";
  const CC::PointsContainer * corners = my3DBox->GetCorners();
  CC::PointsContainer::const_iterator it = corners->begin();
  unsigned int j=0;
  while(it != corners->end())
    {
    for(unsigned int i=0; i<3;i++)
      {
      if((*it)[i] != std::pow(-1.0,(double)(j/(int(std::pow(2.0,(double)i))))))
        {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
        }
      }
    j++;
    ++it;
    }
  std::cout << "[PASSED]" << std::endl;

  // Testing the DeepCopy method
  {
  const double tolerance = 1e-10;
  CC::Pointer clone = my3DBox->DeepCopy();
  const CC::BoundsArrayType & originalBounds = my3DBox->GetBounds();
  const CC::BoundsArrayType & clonedbounds   = clone->GetBounds();
  for(unsigned int i=0; i< originalBounds.Size(); i++)
    {
    if( std::fabs( originalBounds[i] - clonedbounds[i] ) > tolerance )
      {
      std::cerr << "Clonning test failed" << std::endl;
      std::cerr << originalBounds << std::endl;
      std::cerr << clonedbounds   << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  std::cout << "BoundingBox test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
