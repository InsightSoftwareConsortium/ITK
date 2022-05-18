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
#include "itkTestingMacros.h"
#include "vnl/vnl_sample.h"

int
itkConnectedComponentImageFilterTestRGB(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage  outputImage threshold_low threshold_hi [fully_connected] [minimum_object_size]"
              << std::endl;
    return EXIT_FAILURE;
  }

  using InternalPixelType = unsigned short;
  constexpr unsigned int Dimension = 2;

  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using OutputImageType = itk::Image<unsigned short, Dimension>;

  using RGBPixelType = itk::RGBPixel<unsigned char>;
  using RGBImageType = itk::Image<RGBPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InternalImageType>;
  using WriterType = itk::ImageFileWriter<RGBImageType>;


  using ThresholdFilterType = itk::BinaryThresholdImageFilter<InternalImageType, RGBImageType>;
  using FilterType = itk::ConnectedComponentImageFilter<RGBImageType, OutputImageType>;
  using RelabelType = itk::RelabelComponentImageFilter<OutputImageType, OutputImageType>;


  auto reader = ReaderType::New();
  auto writer = WriterType::New();
  auto threshold = ThresholdFilterType::New();
  auto filter = FilterType::New();
  auto relabel = RelabelType::New();

  itk::SimpleFilterWatcher watcher(filter);
  watcher.QuietOn();

  reader->SetFileName(argv[1]);

  InternalPixelType threshold_low, threshold_hi;
  threshold_low = std::stoi(argv[3]);
  threshold_hi = std::stoi(argv[4]);

  threshold->SetInput(reader->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<RGBPixelType>::OneValue());
  threshold->SetOutsideValue(itk::NumericTraits<RGBPixelType>::ZeroValue());
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();

  filter->SetInput(threshold->GetOutput());
  if (argc > 5)
  {
    int fullyConnected = std::stoi(argv[5]);
    filter->SetFullyConnected(fullyConnected);
  }
  relabel->SetInput(filter->GetOutput());
  if (argc > 6)
  {
    int minSize = std::stoi(argv[6]);
    relabel->SetMinimumObjectSize(minSize);
    std::cerr << "minSize: " << minSize << std::endl;
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(relabel->Update());


  // Remap the labels to viewable colors
  auto colored = RGBImageType::New();
  colored->SetRegions(filter->GetOutput()->GetBufferedRegion());
  colored->Allocate();

  unsigned short numObjects = relabel->GetNumberOfObjects();

  std::vector<RGBPixelType> colormap;
  RGBPixelType              px;
  colormap.resize(numObjects + 1);
  vnl_sample_reseed(1031571);
  for (auto & i : colormap)
  {
    px.SetRed(static_cast<unsigned char>(255 * vnl_sample_uniform(0.3333, 1.0)));
    px.SetGreen(static_cast<unsigned char>(255 * vnl_sample_uniform(0.3333, 1.0)));
    px.SetBlue(static_cast<unsigned char>(255 * vnl_sample_uniform(0.3333, 1.0)));

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

  writer->SetInput(colored);
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
