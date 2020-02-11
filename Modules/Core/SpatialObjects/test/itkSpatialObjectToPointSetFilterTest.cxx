/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkTestingMacros.h"

int
itkSpatialObjectToPointSetFilterTest(int, char *[])
{

  // Typedefs
  using PixelType = float;
  using TubeType = itk::TubeSpatialObject<2>;
  using TubePointer = TubeType::Pointer;
  using PointSetType = itk::PointSet<PixelType, 2>;
  using TubePointListType = TubeType::TubePointListType;
  using TubePointType = TubeType::TubePointType;

  TubePointer       tube1 = TubeType::New();
  TubePointListType list;

  for (unsigned int i = 0; i < 10; ++i)
  {
    TubePointType p;
    p.SetPositionInObjectSpace(i, i);
    p.SetRadiusInObjectSpace(1);
    list.push_back(p);
  }

  tube1->SetPoints(list);
  tube1->Update();

  using SpatialObjectToPointSetFilterType = itk::SpatialObjectToPointSetFilter<TubeType, PointSetType>;
  SpatialObjectToPointSetFilterType::Pointer pointSetFilter = SpatialObjectToPointSetFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(pointSetFilter, SpatialObjectToPointSetFilter, MeshSource);

  unsigned int childrenDepth = 0;
  pointSetFilter->SetChildrenDepth(childrenDepth);
  ITK_TEST_SET_GET_VALUE(childrenDepth, pointSetFilter->GetChildrenDepth());

  unsigned int samplingFactor = 1;
  pointSetFilter->SetSamplingFactor(samplingFactor);
  ITK_TEST_SET_GET_VALUE(samplingFactor, pointSetFilter->GetSamplingFactor());

  std::cout << " tnop = " << tube1->GetNumberOfPoints() << std::endl;
  pointSetFilter->SetInput(tube1);

  pointSetFilter->Update();

  PointSetType::Pointer pointSet = pointSetFilter->GetOutput();

  std::cout << "Testing pointSet exists : ";
  if (!pointSet.GetPointer())
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet size : ";
  if (pointSet->GetPoints()->Size() != 10)
  {
    std::cout << "[FAILURE]" << std::endl;
    std::cout << "   " << pointSet->GetPoints()->Size() << " != 10 (ideal)" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet validity : ";

  using PointIterator = PointSetType::PointsContainer::ConstIterator;
  PointIterator pointItr = pointSet->GetPoints()->Begin();
  PointIterator pointEnd = pointSet->GetPoints()->End();

  unsigned int val = 0;
  while (pointItr != pointEnd)
  {
    if ((itk::Math::NotExactlyEquals(pointItr.Value()[0], val)) ||
        (itk::Math::NotExactlyEquals(pointItr.Value()[1], val)))
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
    val++;
    pointItr++;
  }
  std::cout << "[PASSED]" << std::endl;

  // Create a group spatial object
  using Group3DType = itk::PointBasedSpatialObject<3>;
  using Tube3DType = itk::TubeSpatialObject<3>;
  using Tube3DPointListType = Tube3DType::TubePointListType;
  using Tube3DPointType = Tube3DType::TubePointType;

  Group3DType::Pointer group3D = Group3DType::New();


  Tube3DType::Pointer tube3D = Tube3DType::New();
  Tube3DPointListType tubePointList;
  for (unsigned int i = 0; i < 10; ++i)
  {
    Tube3DPointType p;
    p.SetPositionInObjectSpace(i, i + 1, i + 2);
    p.SetRadiusInObjectSpace(1);
    tubePointList.push_back(p);
  }
  tube3D->SetPoints(tubePointList);
  tube3D->Update();

  Tube3DType::Pointer tube3D2 = Tube3DType::New();
  Tube3DPointListType tubePointList2;
  for (unsigned int i = 10; i < 20; ++i)
  {
    Tube3DPointType p;
    p.SetPositionInObjectSpace(i, i + 1, i + 2);
    tubePointList2.push_back(p);
  }
  tube3D2->SetPoints(tubePointList2);
  tube3D2->Update();

  group3D->AddChild(tube3D);
  group3D->AddChild(tube3D2);


  // Create the 3D filter
  using PointSet3DType = itk::PointSet<PixelType, 3>;
  using SpatialObjectToPointSet3DFilterType = itk::SpatialObjectToPointSetFilter<Tube3DType, PointSet3DType>;
  SpatialObjectToPointSet3DFilterType::Pointer pointSetFilter3D = SpatialObjectToPointSet3DFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(pointSetFilter3D, SpatialObjectToPointSetFilter, MeshSource);

  childrenDepth = 999999;
  pointSetFilter3D->SetChildrenDepth(childrenDepth);
  ITK_TEST_SET_GET_VALUE(childrenDepth, pointSetFilter3D->GetChildrenDepth());

  pointSetFilter3D->SetInput(group3D);

  pointSetFilter3D->Update();

  PointSet3DType::Pointer pointSet3D = pointSetFilter3D->GetOutput();

  std::cout << "Testing pointSet3D exists : ";
  if (!pointSet3D.GetPointer())
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet3D size : ";
  if (pointSet3D->GetPoints()->Size() != 20)
  {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  std::cout << "Testing pointSet3D validity : ";

  using PointIterator3D = PointSet3DType::PointsContainer::ConstIterator;
  PointIterator3D pointItr2 = pointSet3D->GetPoints()->Begin();
  PointIterator3D pointEnd2 = pointSet3D->GetPoints()->End();

  val = 0;
  while (pointItr2 != pointEnd2)
  {
    if ((itk::Math::NotExactlyEquals(pointItr2.Value()[0], val)) ||
        (itk::Math::NotExactlyEquals(pointItr2.Value()[1], val + 1)) ||
        (itk::Math::NotExactlyEquals(pointItr2.Value()[2], val + 2)))
    {
      std::cout << pointItr2.Value()[0] << " :" << pointItr2.Value()[1] << " : " << pointItr2.Value()[2] << std::endl;
      std::cout << val << " :" << val + 1 << " : " << val + 2 << std::endl;
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
