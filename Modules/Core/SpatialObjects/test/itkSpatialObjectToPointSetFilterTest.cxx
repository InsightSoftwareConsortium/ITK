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

#include "itkTubeSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkSpatialObjectToPointSetFilter.h"
#include "itkMath.h"

int itkSpatialObjectToPointSetFilterTest(int, char* [] )
{
  // Typedefs
  typedef itk::TubeSpatialObject<2> TubeType;
  typedef TubeType::Pointer         TubePointer;
  typedef itk::PointSet<float,2>    PointSetType;
  typedef TubeType::PointListType   TubePointListType;
  typedef TubeType::TubePointType   TubePointType;

  TubePointer tube1 = TubeType::New();
  TubePointListType list;

  for( unsigned int i=0; i<10; i++)
    {
    TubePointType p;
    p.SetPosition(i,i);
    p.SetRadius(1);
    list.push_back(p);
    }

  tube1->SetPoints(list);

  typedef itk::SpatialObjectToPointSetFilter<TubeType,PointSetType> SpatialObjectToPointSetFilterType;
  SpatialObjectToPointSetFilterType::Pointer PointSetFilter = SpatialObjectToPointSetFilterType::New();
  PointSetFilter->SetInput(tube1);
  PointSetFilter->Update();
  PointSetType::Pointer pointSet = PointSetFilter->GetOutput();

  std::cout << "Testing pointSet exists : ";
  if(!pointSet.GetPointer())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet size : ";
  if(pointSet->GetPoints()->Size() != 10)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet validity : ";

  typedef PointSetType::PointsContainer::ConstIterator  PointIterator;
  PointIterator pointItr = pointSet->GetPoints()->Begin();
  PointIterator pointEnd = pointSet->GetPoints()->End();

  unsigned int val = 0;
  while( pointItr != pointEnd )
    {
    if( (itk::Math::NotExactlyEquals(pointItr.Value()[0], val))
        || (itk::Math::NotExactlyEquals(pointItr.Value()[1], val))
      )
      {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
    val++;
    pointItr++;
    }
  std::cout << "[PASSED]" << std::endl;

  // Create a group spatial object
  typedef itk::PointBasedSpatialObject<3> Group3DType;
  typedef itk::TubeSpatialObject<3>       Tube3DType;
  typedef Tube3DType::PointListType       Tube3DPointListType;
  typedef Tube3DType::TubePointType       Tube3DPointType;
  typedef itk::LineSpatialObject<3>       Line3DType;
  typedef Line3DType::PointListType       Line3DPointListType;
  typedef Line3DType::LinePointType       Line3DPointType;

  Group3DType::Pointer group3D = Group3DType::New();


  Tube3DType::Pointer tube3D = Tube3DType::New();
  Tube3DPointListType tubePointList;

  for( unsigned int i=0; i<10; i++)
    {
    Tube3DPointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetRadius(1);
    tubePointList.push_back(p);
    }


  tube3D->SetPoints(tubePointList);

  Line3DType::Pointer line3D = Line3DType::New();
  Line3DPointListType linePointList;

  for( unsigned int i=10; i<20; i++)
    {
    Line3DPointType p;
    p.SetPosition(i,i+1,i+2);
    linePointList.push_back(p);
    }

  line3D->SetPoints(linePointList);

  group3D->AddSpatialObject(tube3D);
  group3D->AddSpatialObject(line3D);


  // Create the 3D filter
  typedef itk::PointSet<float,3> PointSet3DType;
  typedef itk::SpatialObjectToPointSetFilter<Group3DType,PointSet3DType> SpatialObjectToPointSet3DFilterType;
  SpatialObjectToPointSet3DFilterType::Pointer PointSetFilter3D = SpatialObjectToPointSet3DFilterType::New();
  PointSetFilter3D->SetInput(group3D);
  PointSetFilter3D->SetChildrenDepth(999999);
  PointSetFilter3D->Update();
  PointSet3DType::Pointer pointSet3D = PointSetFilter3D->GetOutput();

  std::cout << "Testing pointSet3D exists : ";
  if(!pointSet3D.GetPointer())
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet3D size : ";
  if(pointSet3D->GetPoints()->Size() != 20)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet3D validity : ";

  typedef PointSet3DType::PointsContainer::ConstIterator  PointIterator3D;
  PointIterator3D pointItr2 = pointSet3D->GetPoints()->Begin();
  PointIterator3D pointEnd2 = pointSet3D->GetPoints()->End();

  val = 0;
  while( pointItr2 != pointEnd2 )
    {
    if( (itk::Math::NotExactlyEquals(pointItr2.Value()[0], val))
        || (itk::Math::NotExactlyEquals(pointItr2.Value()[1], val+1))
        || (itk::Math::NotExactlyEquals(pointItr2.Value()[2], val+2))
      )
      {
      std::cout << pointItr2.Value()[0] << " :" << pointItr2.Value()[1] << " : " << pointItr2.Value()[2] << std::endl;
      std::cout << val << " :" << val+1 << " : " << val+2 << std::endl;
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
    val++;
    pointItr2++;
    }
  std::cout << "[PASSED]" << std::endl;


  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;
}
