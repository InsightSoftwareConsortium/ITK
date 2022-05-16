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

#include "itkTestingHashImageFilter.h"
#include "itkTestingMacros.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkCustomColormapFunction.h"

#include "itkScalarToRGBColormapImageFilter.h"


int
itkScalarToRGBColormapImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputImage outputImage colormap useInputImageExtremaForScaling [customColormapFile]" << std::endl;
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

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  std::string colormapString(argv[3]);

  using VectorImageType = itk::VectorImage<unsigned char, ImageDimension>;
  using VectorFilterType = itk::ScalarToRGBColormapImageFilter<ImageType, VectorImageType>;
  auto vfilter = VectorFilterType::New();

  using RGBFilterType = itk::ScalarToRGBColormapImageFilter<ImageType, RGBImageType>;
  auto rgbfilter = RGBFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(rgbfilter, ScalarToRGBColormapImageFilter, ImageToImageFilter);


  auto useInputImageExtremaForScaling = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(rgbfilter, UseInputImageExtremaForScaling, useInputImageExtremaForScaling);
  ITK_TEST_SET_GET_BOOLEAN(vfilter, UseInputImageExtremaForScaling, useInputImageExtremaForScaling);

  rgbfilter->SetInput(reader->GetOutput());
  vfilter->SetInput(reader->GetOutput());

  if (colormapString == "red")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Red;

    using RGBSpecificColormapType =
      itk::Function::RedColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType =
      itk::Function::RedColormapFunction<VectorFilterType::InputImagePixelType, VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "green")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Green;

    using RGBSpecificColormapType =
      itk::Function::GreenColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::GreenColormapFunction<VectorFilterType::InputImagePixelType,
                                                                            VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "blue")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Blue;

    using RGBSpecificColormapType =
      itk::Function::BlueColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::BlueColormapFunction<VectorFilterType::InputImagePixelType,
                                                                           VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "grey")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Grey;

    using RGBSpecificColormapType =
      itk::Function::GreyColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::GreyColormapFunction<VectorFilterType::InputImagePixelType,
                                                                           VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "cool")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Cool;

    using RGBSpecificColormapType =
      itk::Function::CoolColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::CoolColormapFunction<VectorFilterType::InputImagePixelType,
                                                                           VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "hot")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Hot;

    using RGBSpecificColormapType =
      itk::Function::HotColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType =
      itk::Function::HotColormapFunction<VectorFilterType::InputImagePixelType, VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "spring")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Spring;

    using RGBSpecificColormapType =
      itk::Function::SpringColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::SpringColormapFunction<VectorFilterType::InputImagePixelType,
                                                                             VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "autumn")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Autumn;

    using RGBSpecificColormapType =
      itk::Function::WinterColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::AutumnColormapFunction<VectorFilterType::InputImagePixelType,
                                                                             VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "winter")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Winter;

    using RGBSpecificColormapType =
      itk::Function::WinterColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::WinterColormapFunction<VectorFilterType::InputImagePixelType,
                                                                             VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "copper")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Copper;

    using RGBSpecificColormapType =
      itk::Function::CopperColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::CopperColormapFunction<VectorFilterType::InputImagePixelType,
                                                                             VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "summer")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Summer;

    using RGBSpecificColormapType =
      itk::Function::SummerColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::SummerColormapFunction<VectorFilterType::InputImagePixelType,
                                                                             VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "jet")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Jet;

    using RGBSpecificColormapType =
      itk::Function::JetColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType =
      itk::Function::JetColormapFunction<VectorFilterType::InputImagePixelType, VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "hsv")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::HSV;

    using RGBSpecificColormapType =
      itk::Function::HSVColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType =
      itk::Function::HSVColormapFunction<VectorFilterType::InputImagePixelType, VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "overunder")
  {
    auto colormapEnumVal = itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::OverUnder;

    using RGBSpecificColormapType =
      itk::Function::OverUnderColormapFunction<RGBFilterType::InputImagePixelType, RGBFilterType::OutputImagePixelType>;
    auto rgbColormap = RGBSpecificColormapType::New();

    rgbfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(rgbColormap->GetNameOfClass(), rgbfilter->GetColormap()->GetNameOfClass());

    using VectorSpecificColormapType = itk::Function::OverUnderColormapFunction<VectorFilterType::InputImagePixelType,
                                                                                VectorFilterType::OutputImagePixelType>;
    auto vColormap = VectorSpecificColormapType::New();

    vfilter->SetColormap(colormapEnumVal);
    ITK_TEST_EXPECT_EQUAL(vColormap->GetNameOfClass(), vfilter->GetColormap()->GetNameOfClass());
  }
  else if (colormapString == "custom")
  {
    using ColormapType = itk::Function::CustomColormapFunction<ImageType::PixelType, RGBImageType::PixelType>;

    auto colormap = ColormapType::New();

    using VectorColormapType = itk::Function::CustomColormapFunction<ImageType::PixelType, VectorImageType::PixelType>;

    auto vcolormap = VectorColormapType::New();

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

    ITK_TEST_SET_GET_VALUE(colormap, rgbfilter->GetColormap());
    ITK_TEST_SET_GET_VALUE(vcolormap, vfilter->GetColormap());
  }

  using RGBHasher = itk::Testing::HashImageFilter<RGBImageType>;
  auto rgbhasher = RGBHasher::New();
  rgbhasher->SetInput(rgbfilter->GetOutput());
  rgbhasher->InPlaceOff();

  using VectorHasher = itk::Testing::HashImageFilter<VectorImageType>;
  auto vhasher = VectorHasher::New();
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
  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(rgbfilter->GetOutput());
  writer->Update();

  // Test streaming enumeration for ScalarToRGBColormapImageFilterEnums::RGBColormapFilter elements
  const std::set<itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter> allRGBColormapFilter{
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Red,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Green,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Blue,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Grey,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Hot,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Cool,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Spring,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Summer,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Autumn,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Winter,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Copper,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Jet,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::HSV,
    itk::ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::OverUnder
  };
  for (const auto & ee : allRGBColormapFilter)
  {
    std::cout << "STREAMED ENUM VALUE ScalarToRGBColormapImageFilterEnums::RGBColormapFilter: " << ee << std::endl;
  }
  return EXIT_SUCCESS;
}
