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

#include "itkImageToHistogramFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkComposeImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"

template <typename TVectorImage>
int
itkImageToHistogramFilterTest4Templated(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  " << argv[0] << " inputImageFileName inputImageFileName outputHistogramFile" << std::endl;
    return EXIT_FAILURE;
  }


  using PixelComponentType = unsigned char;
  constexpr unsigned int Dimension = 3;
  using VectorPixelType = itk::Vector<PixelComponentType, 2>;

  using ImageType = itk::Image<unsigned char, Dimension>;
  using VectorImageType = TVectorImage;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using ComposeType = itk::ComposeImageFilter<ImageType, VectorImageType>;
  typename ComposeType::Pointer compose = ComposeType::New();
  compose->SetInput1(reader->GetOutput());
  compose->SetInput2(reader2->GetOutput());

  using HistogramFilterType = itk::Statistics::ImageToHistogramFilter<VectorImageType>;
  typename HistogramFilterType::Pointer histogramFilter = HistogramFilterType::New();
  histogramFilter->SetInput(compose->GetOutput());
  itk::SimpleFilterWatcher watcher(histogramFilter, "histogramFilter");

  using HistogramType = typename HistogramFilterType::HistogramType;
  using SizeType = typename HistogramFilterType::HistogramSizeType;

  // use a 3D image to check the behavior of HistogramToImageFilter when the image
  // is of greater dimension than the histogram
  using FloatImageType = itk::Image<float, 3>;
  using ImageFilterType = itk::HistogramToLogProbabilityImageFilter<HistogramType, FloatImageType>;
  typename ImageFilterType::Pointer imageFilter = ImageFilterType::New();
  imageFilter->SetInput(histogramFilter->GetOutput());
  itk::SimpleFilterWatcher watcher2(imageFilter, "imageFilter");

  using RescaleType = itk::RescaleIntensityImageFilter<FloatImageType, ImageType>;
  typename RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput(imageFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(rescale->GetOutput());
  writer->SetFileName(argv[3]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // print the image produced by HistogramToLogProbabilityImageFilter for visual inspection
  imageFilter->GetOutput()->Print(std::cout);

  return EXIT_SUCCESS;
}


int
itkImageToHistogramFilterTest4(int argc, char * argv[])
{
  std::string command = argv[1];
  argc--;
  argv++;
  if (command == "Vector")
  {
    using VectorImageType = itk::Image<itk::Vector<float, 2>, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  else if (command == "CovarianVector")
  {
    using VectorImageType = itk::Image<itk::CovariantVector<float, 2>, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  else if (command == "RGBPixel")
  {
    using VectorImageType = itk::Image<itk::RGBPixel<unsigned char>, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  else if (command == "RGBAPixel")
  {
    using VectorImageType = itk::Image<itk::RGBAPixel<unsigned char>, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  else if (command == "FixedArray")
  {
    using VectorImageType = itk::Image<itk::FixedArray<unsigned char, 2>, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  else if (command == "complex")
  {
    using VectorImageType = itk::Image<std::complex<float>, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  else if (command == "VectorImage")
  {
    using VectorImageType = itk::VectorImage<unsigned char, 3>;
    return itkImageToHistogramFilterTest4Templated<VectorImageType>(argc, argv);
  }
  return EXIT_FAILURE;
}
