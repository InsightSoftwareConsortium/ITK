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

#include "itkLevelSetVelocityNeighborhoodExtractor.h"
#include "itkFastMarchingImageFilter.h"

int
itkLevelSetVelocityNeighborhoodExtractorTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using AuxValueType = double;

  // Create an input image using fastmarching
  using SourceType = itk::FastMarchingImageFilter<ImageType>;
  SourceType::Pointer source = SourceType::New();

  ImageType::SizeType size;
  size.Fill(17);

  source->SetOutputSize(size);

  SourceType::NodeType node;
  ImageType::IndexType index;
  index.Fill(8);

  node.SetIndex(index);
  node.SetValue(-4.0);

  using NodeContainerType = SourceType::NodeContainer;
  NodeContainerType::Pointer container = NodeContainerType::New();

  container->InsertElement(0, node);

  source->SetTrialPoints(container);
  source->CollectPointsOn();
  source->Update();

  using ExtractorType = itk::LevelSetVelocityNeighborhoodExtractor<ImageType, AuxValueType, 2>;
  ExtractorType::Pointer extractor = ExtractorType::New();

  extractor->SetInputLevelSet(source->GetOutput());
  extractor->SetLevelSetValue(0.0);
  extractor->NarrowBandingOff();

  // create some dummy auxiliary variable images
  using AuxImageType = ExtractorType::AuxImageType;

  AuxImageType::Pointer aux0 = AuxImageType::New();
  aux0->SetRegions(source->GetOutput()->GetBufferedRegion());
  aux0->Allocate();
  aux0->FillBuffer(1.0);

  extractor->SetAuxImage(aux0, 0);

  AuxImageType::Pointer aux1 = AuxImageType::New();
  aux1->SetRegions(source->GetOutput()->GetBufferedRegion());
  aux1->Allocate();
  aux1->FillBuffer(5.0);

  extractor->SetAuxImage(aux1, 1);

  extractor->Locate();

  using Iterator = NodeContainerType::ConstIterator;
  Iterator iter;
  Iterator iterEnd;

  using AuxValueContainer = ExtractorType::AuxValueContainer;
  using AuxIterator = AuxValueContainer::ConstIterator;
  AuxIterator aIter;
  AuxIterator aIterEnd;

  std::cout << "Inside Points" << std::endl;
  iter = extractor->GetInsidePoints()->Begin();
  iterEnd = extractor->GetInsidePoints()->End();
  aIter = extractor->GetAuxInsideValues()->Begin();
  aIterEnd = extractor->GetAuxInsideValues()->End();
  for (; iter != iterEnd; iter++, aIter++)
  {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << " ";
    std::cout << aIter.Value() << std::endl;
  }

  std::cout << "Outside Points" << std::endl;
  iter = extractor->GetOutsidePoints()->Begin();
  iterEnd = extractor->GetOutsidePoints()->End();
  aIter = extractor->GetAuxOutsideValues()->Begin();
  aIterEnd = extractor->GetAuxOutsideValues()->End();

  for (; iter != iterEnd; iter++, aIter++)
  {
    std::cout << iter.Value().GetIndex() << " ";
    std::cout << iter.Value().GetValue() << " ";
    std::cout << aIter.Value() << std::endl;
  }

  // exercise Print
  extractor->Print(std::cout);

  std::cout << "AuxImage0: " << extractor->GetAuxImage(0).GetPointer() << std::endl;

  // exercise error handling
  extractor->SetAuxImage(aux0, 2);

  if (extractor->GetAuxImage(2))
  {
    std::cout << "Out of range index should return nullptr pointer" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
