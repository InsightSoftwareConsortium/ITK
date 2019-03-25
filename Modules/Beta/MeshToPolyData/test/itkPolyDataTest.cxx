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
#include "itkPolyData.h"

#include "itkTestingMacros.h"

int itkPolyDataTest( int, char *[] )
{
  using PixelType = double;

  using PolyDataType = itk::PolyData< PixelType >;
  PolyDataType::Pointer polyData = PolyDataType::New();
  constexpr unsigned int PointDimension = PolyDataType::PointDimension;

  polyData->Initialize();


  PolyDataType::PointsContainer::Pointer pointsContainer = PolyDataType::PointsContainer::New();

  PolyDataType::PointType point;
  point[0] = 1.0;
  point[1] = 3.0;
  point[2] = 5.0;
  pointsContainer->InsertElement( 0, point );
  point[0] = 2.0;
  point[1] = 4.0;
  point[2] = 6.0;
  pointsContainer->InsertElement( 1, point );
  polyData->SetPoints( pointsContainer );

  for( unsigned int dim = 0; dim < PointDimension; ++dim )
    {
    TEST_SET_GET_VALUE( point[dim], polyData->GetPoint( 1 )[dim] );
    }

  point[0] = 3.0;
  point[1] = 5.0;
  point[2] = 7.0;
  polyData->SetPoint( 2, point );
  for( unsigned int dim = 0; dim < PointDimension; ++dim )
    {
    TEST_SET_GET_VALUE( point[dim], polyData->GetPoint( 2 )[dim] );
    }
  polyData->SetPoint( 1, point );
  for( unsigned int dim = 0; dim < PointDimension; ++dim )
    {
    TEST_SET_GET_VALUE( point[dim], polyData->GetPoint( 1 )[dim] );
    }


  PolyDataType::PointDataContainer::Pointer pointDataContainer = PolyDataType::PointDataContainer::New();
  pointDataContainer->InsertElement( 0, 2.0 );
  pointDataContainer->InsertElement( 1, 7.0 );
  polyData->SetPointData( pointDataContainer );
  double pointData;
  polyData->GetPointData( 1, &pointData );
  TEST_SET_GET_VALUE( 7.0, pointData );
  polyData->SetPointData( 2, 9.9 );
  polyData->GetPointData( 2, &pointData );
  TEST_SET_GET_VALUE( 9.9, pointData );

  EXERCISE_BASIC_OBJECT_METHODS( polyData, PolyData, DataObject );

  return EXIT_SUCCESS;
}
