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

#include "itkGDCMImageIO.h"
#include "itkImageSeriesReader.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkImageSeriesWriterTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " DicomDirectory OutputDirectory FileSuffix"
              << std::endl;
    return EXIT_FAILURE;
  }

  using ImageNDType = itk::Image<short, 3>;
  using ReaderType = itk::ImageSeriesReader<ImageNDType>;

  itk::GDCMImageIO::Pointer io = itk::GDCMImageIO::New();

  // Get the DICOM filenames from the directory
  itk::GDCMSeriesFileNames::Pointer nameGenerator = itk::GDCMSeriesFileNames::New();
  nameGenerator->SetDirectory(argv[1]);

  using SeriesIdContainer = std::vector<std::string>;
  const SeriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();
  std::string               seriesIdentifier = seriesUID.begin()->c_str();

  auto reader = ReaderType::New();
  reader->SetFileNames(nameGenerator->GetFileNames(seriesIdentifier));
  reader->SetImageIO(io);

  itk::SimpleFilterWatcher watcher(reader);


  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  reader->GetOutput()->Print(std::cout);


  using WritePixelType = unsigned char;
  using RescaleImageType = itk::Image<WritePixelType, 3>;
  using OutputImageType = itk::Image<WritePixelType, 2>;

  using RescaleFilterType = itk::RescaleIntensityImageFilter<ImageNDType, RescaleImageType>;
  auto rescaler = RescaleFilterType::New();
  rescaler->SetInput(reader->GetOutput());
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  rescaler->UpdateLargestPossibleRegion();


  { // This API is being deprecated. Please use NumericSeriesFileNames in the future
    // for generating the list of filenames.  This API will be removed after ITK 1.8
    using WriterType = itk::ImageSeriesWriter<RescaleImageType, OutputImageType>;

    auto writer = WriterType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(writer, ImageSeriesWriter, ProcessObject);


    itk::SimpleFilterWatcher watcher2(writer);

    writer->SetInput(rescaler->GetOutput());

    itk::SizeValueType startIndex = 1;
    writer->SetStartIndex(startIndex);
    ITK_TEST_SET_GET_VALUE(startIndex, writer->GetStartIndex());

    itk::SizeValueType incrementIndex = 1;
    writer->SetIncrementIndex(incrementIndex);
    ITK_TEST_SET_GET_VALUE(incrementIndex, writer->GetIncrementIndex());

    char format[4096];
    snprintf(format, sizeof(format), "%s/series.%%d.%s", argv[2], argv[3]);
    writer->SetSeriesFormat(format);
    ITK_TEST_SET_GET_VALUE(std::string(format), std::string(writer->GetSeriesFormat()));

    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


    // Verify that attempting to use a MetaDataDictionary without setting the ImageIO
    // should throw an exception.
    writer->SetMetaDataDictionaryArray(reader->GetMetaDataDictionaryArray());

    ITK_TRY_EXPECT_EXCEPTION(writer->Update());

    writer->SetImageIO(io);
    ITK_TEST_SET_GET_VALUE(io, writer->GetImageIO());


    std::cout << "Old API PASSED !" << std::endl;
  }

  { // This is the new API, using the NumericSeriesFileNames (or any other filename generator).
    itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

    using WriterType = itk::ImageSeriesWriter<RescaleImageType, OutputImageType>;

    auto writer = WriterType::New();


    char format[4096];
    snprintf(format, sizeof(format), "%s/series.%%d.%s", argv[2], argv[3]);

    std::cout << "Format = " << format << std::endl;

    ImageNDType::RegionType region = reader->GetOutput()->GetBufferedRegion();
    ImageNDType::SizeType   size = region.GetSize();

    itk::SizeValueType startIndex = 0;
    fit->SetStartIndex(startIndex);
    ITK_TEST_SET_GET_VALUE(startIndex, fit->GetStartIndex());

    itk::SizeValueType endIndex = size[2] - 1;
    fit->SetEndIndex(endIndex); // The number of slices to write
    ITK_TEST_SET_GET_VALUE(endIndex, fit->GetEndIndex());

    itk::SizeValueType incrementIndex = 1;
    fit->SetIncrementIndex(incrementIndex);
    ITK_TEST_SET_GET_VALUE(incrementIndex, fit->GetIncrementIndex());

    fit->SetSeriesFormat(format);
    ITK_TEST_SET_GET_VALUE(std::string(format), std::string(fit->GetSeriesFormat()));

    writer->SetInput(rescaler->GetOutput());
    writer->SetFileNames(fit->GetFileNames());

    // experiment the UseCompression methods and values
    if (writer->GetUseCompression())
    {
      std::cerr << "Wrong default use compression value" << std::endl;
      return EXIT_FAILURE;
    }
    bool useCompression = false;
    ITK_TEST_SET_GET_BOOLEAN(writer, UseCompression, useCompression);

    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


    // Verify that attempting to use a MetaDataDictionary without setting the ImageIO
    // should throw an exception.
    writer->SetMetaDataDictionaryArray(reader->GetMetaDataDictionaryArray());

    ITK_TRY_EXPECT_EXCEPTION(writer->Update());


    std::cout << "Test with NumericSeriesFileNames PASSED !" << std::endl;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
