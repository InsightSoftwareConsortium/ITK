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

#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkTestingMacros.h"

int
itkMaskConnectedComponentImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage  outputImage threshold_low threshold_hi [fully_connected] [minimum_object_size]"
              << std::endl;
    return EXIT_FAILURE;
  }

  using InternalPixelType = unsigned short;
  using MaskPixelType = bool;
  constexpr unsigned int Dimension = 2;

  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using MaskImageType = itk::Image<MaskPixelType, Dimension>;
  using OutputImageType = itk::Image<unsigned short, Dimension>;

  using RGBPixelType = itk::RGBPixel<unsigned char>;
  using RGBImageType = itk::Image<RGBPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InternalImageType>;
  using WriterType = itk::ImageFileWriter<RGBImageType>;


  using ThresholdFilterType = itk::BinaryThresholdImageFilter<InternalImageType, InternalImageType>;
  using FilterType = itk::ConnectedComponentImageFilter<InternalImageType, OutputImageType, MaskImageType>;
  using RelabelType = itk::RelabelComponentImageFilter<OutputImageType, OutputImageType>;


  auto reader = ReaderType::New();
  auto writer = WriterType::New();
  auto threshold = ThresholdFilterType::New();
  auto filter = FilterType::New();
  auto relabel = RelabelType::New();

  itk::SimpleFilterWatcher watcher(filter);
  watcher.QuietOn();

  reader->SetFileName(argv[1]);

  const InternalPixelType threshold_low = std::stoi(argv[3]);
  const InternalPixelType threshold_hi = std::stoi(argv[4]);

  threshold->SetInput(reader->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::OneValue());
  threshold->SetOutsideValue(InternalPixelType{});
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();

  // create a mask containing the upper left hand corner and
  // a chunk out of the middle
  auto mask = MaskImageType::New();
  mask->SetRegions(threshold->GetOutput()->GetLargestPossibleRegion());
  mask->CopyInformation(threshold->GetOutput());
  mask->Allocate();
  mask->FillBuffer(MaskPixelType{});

  const MaskImageType::RegionType maskRegion = mask->GetLargestPossibleRegion();
  MaskImageType::SizeType         maskSize = maskRegion.GetSize();

  // use upper left corner
  MaskImageType::SizeType size;
  for (unsigned int i = 0; i < MaskImageType::ImageDimension; ++i)
  {
    size[i] = static_cast<unsigned long>(0.375 * maskSize[i]);
  }

  MaskImageType::IndexType  index{};
  MaskImageType::RegionType region;
  region.SetIndex(index);
  region.SetSize(size);

  itk::ImageRegionIterator<MaskImageType> mit(mask, region);
  while (!mit.IsAtEnd())
  {
    mit.Set(itk::NumericTraits<MaskPixelType>::max());
    ++mit;
  }

  // use middle section
  for (unsigned int i = 0; i < MaskImageType::ImageDimension; ++i)
  {
    index[i] = static_cast<long>(0.375 * maskSize[i]);
    size[i] = static_cast<unsigned long>(0.25 * maskSize[i]);
  }
  region.SetIndex(index);
  region.SetSize(size);

  itk::ImageRegionIterator<MaskImageType> mit2(mask, region);
  while (!mit2.IsAtEnd())
  {
    mit2.Set(itk::NumericTraits<MaskPixelType>::max());
    ++mit2;
  }

  filter->SetInput(threshold->GetOutput());
  filter->SetMaskImage(mask);

  if (argc > 5)
  {
    const int fullyConnected = std::stoi(argv[5]);
    filter->SetFullyConnected(fullyConnected);
  }
  relabel->SetInput(filter->GetOutput());
  if (argc > 6)
  {
    const int minSize = std::stoi(argv[6]);
    relabel->SetMinimumObjectSize(minSize);
    std::cerr << "minSize: " << minSize << std::endl;
  }

  try
  {
    relabel->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Relabel: exception caught !" << std::endl;
    std::cerr << excep << std::endl;
  }

  // Remap the labels to viewable colors
  auto colored = RGBImageType::New();
  colored->SetRegions(filter->GetOutput()->GetBufferedRegion());
  colored->Allocate();

  const unsigned short numObjects = relabel->GetNumberOfObjects();

  std::vector<RGBPixelType> colormap;
  colormap.resize(numObjects + 1);
  const itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer rvgen =
    itk::Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();
  rvgen->SetSeed(1031571);
  RGBPixelType px;
  for (auto & i : colormap)
  {
    px.SetRed(static_cast<unsigned char>(255 * rvgen->GetUniformVariate(0.3333, 1.0)));
    px.SetGreen(static_cast<unsigned char>(255 * rvgen->GetUniformVariate(0.3333, 1.0)));
    px.SetBlue(static_cast<unsigned char>(255 * rvgen->GetUniformVariate(0.3333, 1.0)));

    i = px;
  }

  itk::ImageRegionIterator<OutputImageType> it(relabel->GetOutput(), relabel->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<RGBImageType>    cit(colored, colored->GetBufferedRegion());

  while (!it.IsAtEnd())
  {
    if (it.Get() == 0)
    {
      cit.Set(RGBPixelType());
    }
    else
    {
      cit.Set(colormap[it.Get()]);
    }
    ++it;
    ++cit;
  }

  try
  {
    writer->SetInput(colored);
    writer->SetFileName(argv[2]);
    writer->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
  }

  return EXIT_SUCCESS;
}
