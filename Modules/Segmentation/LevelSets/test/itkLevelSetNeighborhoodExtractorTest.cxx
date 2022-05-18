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

#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkFastMarchingImageFilter.h"

int
itkLevelSetNeighborhoodExtractorTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;

  // Create an input image using fastmarching
  using SourceType = itk::FastMarchingImageFilter<ImageType>;
  auto source = SourceType::New();

  ImageType::SizeType size;
  size.Fill(17);

  source->SetOutputSize(size);

  SourceType::NodeType node;
  ImageType::IndexType index;
  index.Fill(8);

  node.SetIndex(index);
  node.SetValue(-4.0);

  using NodeContainerType = SourceType::NodeContainer;
  auto container = NodeContainerType::New();

  container->InsertElement(0, node);

  source->SetTrialPoints(container);
  source->CollectPointsOn();
  source->Update();

  using ExtractorType = itk::LevelSetNeighborhoodExtractor<ImageType>;
  auto extractor = ExtractorType::New();

  extractor->SetInputLevelSet(source->GetOutput());
  extractor->SetLevelSetValue(0.0);
  extractor->NarrowBandingOff();

  extractor->Locate();

  using Iterator = NodeContainerType::ConstIterator;
  Iterator iter;
  Iterator iterEnd;

  std::cout << "Inside Points" << std::endl;
  iter = extractor->GetInsidePoints()->Begin();
  iterEnd = extractor->GetInsidePoints()->End();
  for (; iter != iterEnd; ++iter)
  {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << std::endl;
  }

  std::cout << "Outside Points" << std::endl;
  iter = extractor->GetOutsidePoints()->Begin();
  iterEnd = extractor->GetOutsidePoints()->End();
  for (; iter != iterEnd; ++iter)
  {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << std::endl;
  }

  // exercise Print
  extractor->Print(std::cout);

  // exercise Get methods
  std::cout << "InputLevelSet: " << extractor->GetInputLevelSet() << std::endl;
  std::cout << "LevelSetValue: " << extractor->GetLevelSetValue() << std::endl;
  std::cout << "NarrowBandwidth: " << extractor->GetNarrowBandwidth() << std::endl;
  std::cout << "NarrowBanding: " << extractor->GetNarrowBanding() << std::endl;
  std::cout << "InputNarrowBand: " << extractor->GetInputNarrowBand() << std::endl;

  // exercise error handling
  bool passed;
  std::cout << "Testing nullptr inputs" << std::endl;

  try
  {
    passed = false;
    extractor->SetInputLevelSet(nullptr);
    extractor->Locate();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    passed = true;
    extractor->SetInputLevelSet(source->GetOutput());
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    passed = false;
    extractor->NarrowBandingOn();
    extractor->SetInputNarrowBand(nullptr);
    extractor->Locate();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    passed = true;
    extractor->NarrowBandingOff();
  }

  if (!passed)
  {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
