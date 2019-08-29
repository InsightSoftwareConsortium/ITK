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

#include "itkTestingHashImageFilter.h"
#include "itkTestingMacros.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkCustomColormapFunction.h"

#include "itkScalarToRGBColormapImageFilter.h"


int
itkScalarToRGBColormapImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputImage outputImage colormap [customColormapFile]" << std::endl;
    std::cout << "  Possible colormaps: grey, red, green, blue, copper, jet, hsv, ";
    std::cout << "spring, summer, autumn, winter, hot, cool, custom" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ImageDimension = 2;

  using PixelType = unsigned int;
  using RGBPixelType = itk::RGBPixel<unsigned char>;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using RGBImageType = itk::Image<RGBPixelType, ImageDimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  std::string colormapString(argv[3]);

  using VectorImageType = itk::VectorImage<unsigned char, ImageDimension>;
  using VectorFilterType = itk::ScalarToRGBColormapImageFilter<ImageType, VectorImageType>;
  VectorFilterType::Pointer vfilter = VectorFilterType::New();

  using RGBFilterType = itk::ScalarToRGBColormapImageFilter<ImageType, RGBImageType>;
  RGBFilterType::Pointer rgbfilter = RGBFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(rgbfilter, ScalarToRGBColormapImageFilter, ImageToImageFilter);

  rgbfilter->SetInput(reader->GetOutput());
  vfilter->SetInput(reader->GetOutput());

  if (colormapString == "red")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Red);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Red);
  }
  else if (colormapString == "green")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Green);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Green);
  }
  else if (colormapString == "blue")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Blue);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Blue);
  }
  else if (colormapString == "grey")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Grey);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Grey);
  }
  else if (colormapString == "cool")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Cool);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Cool);
  }
  else if (colormapString == "hot")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Hot);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Hot);
  }
  else if (colormapString == "spring")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Spring);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Spring);
  }
  else if (colormapString == "autumn")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Autumn);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Autumn);
  }
  else if (colormapString == "winter")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Winter);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Winter);
  }
  else if (colormapString == "copper")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Copper);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Copper);
  }
  else if (colormapString == "summer")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Summer);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Summer);
  }
  else if (colormapString == "jet")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::Jet);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::Jet);
  }
  else if (colormapString == "hsv")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::HSV);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::HSV);
  }
  else if (colormapString == "overunder")
  {
    rgbfilter->SetColormap(itk::RGBColormapFilterEnumType::OverUnder);
    vfilter->SetColormap(itk::RGBColormapFilterEnumType::OverUnder);
  }
  else if (colormapString == "custom")
  {
    using ColormapType = itk::Function::CustomColormapFunction<ImageType::PixelType, RGBImageType::PixelType>;

    ColormapType::Pointer colormap = ColormapType::New();

    using VectorColormapType = itk::Function::CustomColormapFunction<ImageType::PixelType, VectorImageType::PixelType>;

    VectorColormapType::Pointer vcolormap = VectorColormapType::New();

    std::ifstream str(argv[4]);
    std::string   line;

    float                     value;
    ColormapType::ChannelType channel;

    // Get red values
    std::getline(str, line);
    std::istringstream issr(line);
    while (issr >> value)
    {
      channel.push_back(value);
    }
    colormap->SetRedChannel(channel);
    vcolormap->SetRedChannel(channel);

    // Get green values
    std::getline(str, line);
    std::istringstream issg(line);
    while (issg >> value)
    {
      channel.push_back(value);
    }
    colormap->SetGreenChannel(channel);
    vcolormap->SetGreenChannel(channel);

    // Get blue values
    std::getline(str, line);
    std::istringstream issb(line);
    colormap->SetMinimumRGBComponentValue(0);
    colormap->SetMaximumRGBComponentValue(255);
    while (issb >> value)
    {
      channel.push_back(value);
    }
    colormap->SetBlueChannel(channel);
    vcolormap->SetBlueChannel(channel);
    rgbfilter->SetColormap(colormap);
    vfilter->SetColormap(vcolormap);
  }

  using RGBHasher = itk::Testing::HashImageFilter<RGBImageType>;
  RGBHasher::Pointer rgbhasher = RGBHasher::New();
  rgbhasher->SetInput(rgbfilter->GetOutput());
  rgbhasher->InPlaceOff();

  using VectorHasher = itk::Testing::HashImageFilter<VectorImageType>;
  VectorHasher::Pointer vhasher = VectorHasher::New();
  vhasher->SetInput(vfilter->GetOutput());

  try
  {
    rgbfilter->Update();
    vfilter->Update();

    rgbhasher->Update();
    vhasher->Update();
  }
  catch (...)
  {
    return EXIT_FAILURE;
  }

  using WriterType = itk::ImageFileWriter<RGBImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(rgbfilter->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
