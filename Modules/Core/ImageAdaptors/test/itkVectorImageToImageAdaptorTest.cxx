/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkVectorImageToImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

// This test tests the basic functionality of
// VectorImageToImageAdaptor, especially the Set/GetPixel() methods.

int
itkVectorImageToImageAdaptorTest(int, char *[])
{

  // image type alias
  constexpr unsigned int Dimension = 3;
  constexpr unsigned int VectorLength = 4;
  constexpr unsigned int componentToExtract = 3;
  using PixelType = float;

  using VectorImageType = itk::VectorImage<PixelType, Dimension>;

  using VectorImageToImageAdaptorType = itk::VectorImageToImageAdaptor<PixelType, Dimension>;

  // initialize a vector image
  auto                                 vectorImage = VectorImageType::New();
  VectorImageType::IndexType           start;
  itk::VariableLengthVector<PixelType> f(VectorLength);
  VectorImageType::SizeType            size;
  for (unsigned int i = 0; i < VectorLength; ++i)
  {
    f[i] = static_cast<PixelType>(i);
  }
  start.Fill(0);
  size.Fill(50);

  const VectorImageType::RegionType region{ start, size };
  vectorImage->SetVectorLength(VectorLength);
  vectorImage->SetRegions(region);
  vectorImage->Allocate();
  vectorImage->FillBuffer(f);


  // run the adaptor
  auto vectorImageToImageAdaptor = VectorImageToImageAdaptorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(vectorImageToImageAdaptor, VectorImageToImageAdaptor, ImageAdaptor);


  vectorImageToImageAdaptor->SetExtractComponentIndex(componentToExtract);

  vectorImageToImageAdaptor->SetImage(vectorImage);
  vectorImageToImageAdaptor->Update();

  // test adaptor with const iterator
  itk::ImageRegionConstIteratorWithIndex<VectorImageToImageAdaptorType> adaptIt(vectorImageToImageAdaptor, region);
  adaptIt.GoToBegin();
  while (!adaptIt.IsAtEnd())
  {
    const PixelType pixelV = adaptIt.Get();
    if (itk::Math::NotAlmostEquals(pixelV, static_cast<PixelType>(componentToExtract)))
    {
      std::cout << "Wrong Pixel Value: adaptIt(" << adaptIt.GetIndex() << ") = " << adaptIt.Get() << std::endl;

      return EXIT_FAILURE;
    }
    ++adaptIt;
  }

  // test Get/SetPixel() methods
  auto index = VectorImageToImageAdaptorType::IndexType::Filled(10);
  ITK_TEST_EXPECT_EQUAL(PixelType(componentToExtract), vectorImageToImageAdaptor->GetPixel(index));

  constexpr PixelType v = 4.4f;
  vectorImageToImageAdaptor->SetPixel(index, v);
  ITK_TEST_EXPECT_EQUAL(v, vectorImageToImageAdaptor->GetPixel(index));

  return EXIT_SUCCESS;
}
