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

#include "itkImage.h"

#include <iostream>
#include <fstream>

template <unsigned int TDimension>
void TestTransform()
{
  typedef itk::Image<float,TDimension>         ImageType;

  typename ImageType::Pointer image =         ImageType::New();
  typename ImageType::Pointer orientedImage = ImageType::New();

  typename ImageType::PointType origin;

  for (unsigned int i=0; i < TDimension; i++)
    {
    origin[i] = static_cast<double>(i*100);
    }
  image->SetOrigin(origin);
  orientedImage->SetOrigin(origin);

  typedef itk::ImageRegion< TDimension > RegionType;
  typedef typename RegionType::IndexType IndexType;
  typedef typename RegionType::SizeType  SizeType;

  typename ImageType::PointType point;
  RegionType region;

  SizeType size;
  size.Fill(10);
  region.SetSize( size );

  IndexType index;
  index.Fill(5);

  std::cout << "TransformIndexToPhysicalPoint..." << std::endl;
  orientedImage->TransformIndexToPhysicalPoint(index, point);
  std::cout << "    Image: " << index << " -> " << point << std::endl;

  image->TransformIndexToPhysicalPoint(index, point);
  std::cout << "    Image:         " << index << " -> " << point << std::endl;

  std::cout << "TransformPhysicalPointToIndex..." << std::endl;
  orientedImage->TransformPhysicalPointToIndex(point, index);
  std::cout << "    Image: " << point << " -> " << index << std::endl;

  image->TransformPhysicalPointToIndex(point, index);
  std::cout << "    Image:         " << point << " -> " << index << std::endl;
}

int itkImageTransformTest(int, char* [] )
{
  TestTransform<8>();
  TestTransform<3>();
  TestTransform<2>();
  TestTransform<1>();

  return EXIT_SUCCESS;

}
