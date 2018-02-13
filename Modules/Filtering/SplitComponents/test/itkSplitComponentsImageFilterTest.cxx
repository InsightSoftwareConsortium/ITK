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
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVector.h"

#include <sstream>

#include "itkSplitComponentsImageFilter.h"

int
itkSplitComponentsImageFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImagePrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = signed short;
  using OutputImageType = itk::Image<PixelType, Dimension>;
  using VectorType = itk::Vector<PixelType, Dimension>;
  using InputImageType = itk::Image<VectorType, Dimension>;

  // Size in every dimension of the output image.
  constexpr unsigned int sizes = 100;

  InputImageType::Pointer input = InputImageType::New();

  using RegionType = InputImageType::RegionType;
  RegionType            region;
  RegionType::IndexType index;
  index.Fill(0);
  region.SetIndex(index);
  RegionType::SizeType size;
  for (unsigned int i = 0; i < Dimension; i++)
  {
    size[i] = sizes;
  }
  region.SetSize(size);

  input->SetRegions(region);
  input->Allocate();

  itk::ImageRegionIteratorWithIndex<InputImageType> it(input, region);
  VectorType                                        vector;
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    index = it.GetIndex();
    vector[0] = index[0];
    vector[1] = index[1];
    it.Set(vector);
  }

  using FilterType = itk::SplitComponentsImageFilter<InputImageType, OutputImageType, Dimension>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(input);

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();

  std::ostringstream ostr;
  try
  {
    ostr << argv[1] << 0 << ".mha";
    writer->SetFileName(ostr.str());
    writer->SetInput(filter->GetOutput());
    writer->Update();

    ostr.str("");
    ostr << argv[1] << 1 << ".mha";
    writer->SetFileName(ostr.str());
    writer->SetInput(filter->GetOutput(1));
    writer->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  using ComponentsMaskType = FilterType::ComponentsMaskType;
  ComponentsMaskType componentsMask(false);
  componentsMask[1] = true;
  const ComponentsMaskType oldComponents = filter->GetComponentsMask();
  if (oldComponents[0] != true)
  {
    std::cerr << "Did not get the expected default ComponentsMask." << std::endl;
    return EXIT_FAILURE;
  }
  filter->SetComponentsMask(componentsMask);
  const ComponentsMaskType newComponents = filter->GetComponentsMask();
  if (newComponents[0] != false)
  {
    std::cerr << "Did not get the expected modified ComponentsMask." << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
