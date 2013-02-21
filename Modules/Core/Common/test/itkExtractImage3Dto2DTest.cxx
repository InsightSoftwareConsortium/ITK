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
#include "itkExtractImageFilter.h"
#include "itkRandomImageSource.h"

int itkExtractImage3Dto2DTest(int, char* [] )
{
  typedef itk::Image<uint8_t,3>                      Image3DType;
  typedef itk::Image<uint8_t,2>                      Image2DType;
  typedef itk::ExtractImageFilter<Image3DType,Image2DType> ExtractType;
  typedef itk::RandomImageSource<Image3DType>              RandomImageSourceType;

  RandomImageSourceType::Pointer src =
    RandomImageSourceType::New();
  src->SetMin(0);
  src->SetMax(255);
  Image3DType::SizeType size = {{16,16,16}};
  src->SetSize(size);
  src->Update();
  Image3DType::Pointer im3d(src->GetOutput());
  Image3DType::DirectionType dir = im3d->GetDirection();
  dir[1][1] = 0.0;
  dir[1][2] = 1.0;
  dir[2][1] = 1.0;
  dir[2][2] = 0.0;
  // change directions to
  // 1 0 0
  // 0 0 1
  // 0 1 0

  im3d->SetDirection(dir);

  ExtractType::Pointer extract = ExtractType::New();
  extract->SetDirectionCollapseToIdentity();
  Image3DType::RegionType extractRegion = im3d->GetLargestPossibleRegion();
  Image3DType::SizeType extractSize = extractRegion.GetSize();

  extractSize[2] = 0;
  Image3DType::IndexType extractIndex;
  extractIndex[2] = extractIndex[1] = extractIndex[0] = 0;

  extract->SetInput(im3d);
  extractRegion.SetSize(extractSize);
  extractRegion.SetIndex(extractIndex);
  extract->SetExtractionRegion(extractRegion);
  extract->Update();

  Image2DType::Pointer extractedImage = extract->GetOutput();
  std::cout << "Directions from extracted 2D Image"
            << extractedImage->GetDirection()
            << std::endl;
  return EXIT_SUCCESS;
}
