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

#include "itkTestingMacros.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVotingBinaryImageFilter.h"


namespace
{

template <typename TInputImageType>
int
itkVotingBinaryImageFilterTestImp(const std::string & infname,
                                  const std::string & outfname,
                                  itk::SizeValueType  radius,
                                  long                foregroundValue,
                                  long                backgroundValue,
                                  unsigned int        birthThreshold = 1,
                                  unsigned int        survivalThreshold = 1)
{
  using InputImageType = TInputImageType;
  using OutputImageType = TInputImageType;

  using InputPixelType = typename TInputImageType::PixelType;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  using FilterType = itk::VotingBinaryImageFilter<InputImageType, OutputImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(infname);

  auto filter = FilterType::New();

  filter->SetInput(reader->GetOutput());

  auto R = FilterType::InputSizeType::Filled(itk::Math::CastWithRangeCheck<itk::SizeValueType>(radius));
  filter->SetRadius(R);

  filter->SetForegroundValue(itk::Math::CastWithRangeCheck<InputPixelType>(foregroundValue));
  filter->SetBackgroundValue(itk::Math::CastWithRangeCheck<InputPixelType>(backgroundValue));
  filter->SetBirthThreshold(birthThreshold);
  filter->SetSurvivalThreshold(survivalThreshold);

  ITK_TEST_SET_GET_VALUE(R, filter->GetRadius());
  ITK_TEST_SET_GET_VALUE(itk::Math::CastWithRangeCheck<InputPixelType>(foregroundValue), filter->GetForegroundValue());
  ITK_TEST_SET_GET_VALUE(itk::Math::CastWithRangeCheck<InputPixelType>(backgroundValue), filter->GetBackgroundValue());
  ITK_TEST_SET_GET_VALUE(birthThreshold, filter->GetBirthThreshold());
  ITK_TEST_SET_GET_VALUE(survivalThreshold, filter->GetSurvivalThreshold());

  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(outfname);
  writer->SetNumberOfStreamDivisions(5);
  writer->Update();

  std::cout << filter;

  return EXIT_SUCCESS;
}

} // namespace


int
itkVotingBinaryImageFilterTest(int argc, char * argv[])
{

  if (argc < 6)
  {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " Inputimage OutputImage radius ForegroundValue BackgroundValue" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string  infname = argv[1];
  const std::string  outfname = argv[2];
  const unsigned int radius = std::stoi(argv[3]);
  const long         foregroundValue = atol(argv[4]);
  const long         backgroundValue = atol(argv[5]);


  const itk::ImageIOBase::Pointer iobase =
    itk::ImageIOFactory::CreateImageIO(infname.c_str(), itk::ImageIOFactory::IOFileModeEnum::ReadMode);

  if (iobase.IsNull())
  {
    itkGenericExceptionMacro("Unable to determine ImageIO reader for \"" << infname << '"');
  }


  // const itk::IOPixelEnum pixelType = iobase->GetPixelType();
  const itk::IOComponentEnum componentType = iobase->GetComponentType();
  const unsigned int         dimension = iobase->GetNumberOfDimensions();

  using TestImageType = itk::Image<short, 3>;
  using FilterType = itk::VotingBinaryImageFilter<TestImageType, TestImageType>;

  auto filter = FilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, VotingBinaryImageFilter, ImageToImageFilter);

  switch (componentType)
  {
    case itk::IOComponentEnum::CHAR:
    case itk::IOComponentEnum::UCHAR:
    case itk::IOComponentEnum::SHORT:
      if (dimension == 2)
        return itkVotingBinaryImageFilterTestImp<itk::Image<short, 2>>(
          infname, outfname, radius, foregroundValue, backgroundValue);
      else if (dimension == 3)
        return itkVotingBinaryImageFilterTestImp<itk::Image<short, 3>>(
          infname, outfname, radius, foregroundValue, backgroundValue);
      break;
    case itk::IOComponentEnum::USHORT:
    case itk::IOComponentEnum::INT:
      if (dimension == 2)
        return itkVotingBinaryImageFilterTestImp<itk::Image<int, 2>>(
          infname, outfname, radius, foregroundValue, backgroundValue);
      else if (dimension == 3)
        return itkVotingBinaryImageFilterTestImp<itk::Image<int, 3>>(
          infname, outfname, radius, foregroundValue, backgroundValue);
      break;
    case itk::IOComponentEnum::UINT:
      if (dimension == 2)
        return itkVotingBinaryImageFilterTestImp<itk::Image<unsigned int, 2>>(
          infname, outfname, radius, foregroundValue, backgroundValue);
      else if (dimension == 3)
        return itkVotingBinaryImageFilterTestImp<itk::Image<unsigned int, 3>>(
          infname, outfname, radius, foregroundValue, backgroundValue);
      break;
    case itk::IOComponentEnum::ULONG:
    case itk::IOComponentEnum::LONG:
    case itk::IOComponentEnum::ULONGLONG:
    case itk::IOComponentEnum::LONGLONG:
    case itk::IOComponentEnum::FLOAT:
    case itk::IOComponentEnum::DOUBLE:
    case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
    default:
      itkGenericExceptionMacro("Input image is a real, long, long long, or an unknown component type");
  }

  std::cerr << "Unexpected program flow!" << std::endl;
  return EXIT_FAILURE;
}
