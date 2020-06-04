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

#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"

int
main(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " DicomDirectory  outputFileName sigma [seriesName] "
              << std::endl;
    return EXIT_FAILURE;
  }


  using PixelType = signed short;
  constexpr unsigned int Dimension = 3;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageSeriesReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();


  using ImageIOType = itk::GDCMImageIO;
  ImageIOType::Pointer dicomIO = ImageIOType::New();

  reader->SetImageIO(dicomIO);


  using NamesGeneratorType = itk::GDCMSeriesFileNames;
  NamesGeneratorType::Pointer nameGenerator = NamesGeneratorType::New();

  nameGenerator->SetUseSeriesDetails(true);
  nameGenerator->AddSeriesRestriction("0008|0021");

  nameGenerator->SetDirectory(argv[1]);


  try
  {
    std::cout << std::endl << "The directory: " << std::endl;
    std::cout << std::endl << argv[1] << std::endl << std::endl;
    std::cout << "Contains the following DICOM Series: ";
    std::cout << std::endl << std::endl;


    using SeriesIdContainer = std::vector<std::string>;

    const SeriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();

    auto seriesItr = seriesUID.begin();
    auto seriesEnd = seriesUID.end();
    while (seriesItr != seriesEnd)
    {
      std::cout << seriesItr->c_str() << std::endl;
      ++seriesItr;
    }


    std::string seriesIdentifier;

    if (argc > 4) // If no optional series identifier
    {
      seriesIdentifier = argv[4];
    }
    else
    {
      seriesIdentifier = seriesUID.begin()->c_str();
    }


    std::cout << std::endl << std::endl;
    std::cout << "Now reading series: " << std::endl << std::endl;
    std::cout << seriesIdentifier << std::endl;
    std::cout << std::endl << std::endl;


    using FileNamesContainer = std::vector<std::string>;
    FileNamesContainer fileNames;

    fileNames = nameGenerator->GetFileNames(seriesIdentifier);


    reader->SetFileNames(fileNames);


    try
    {
      reader->Update();
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cout << ex << std::endl;
      return EXIT_FAILURE;
    }


    using FilterType =
      itk::SmoothingRecursiveGaussianImageFilter<ImageType, ImageType>;

    FilterType::Pointer filter = FilterType::New();

    filter->SetInput(reader->GetOutput());

    const double sigma = std::stod(argv[3]);
    filter->SetSigma(sigma);

    using WriterType = itk::ImageFileWriter<ImageType>;
    WriterType::Pointer writer = WriterType::New();

    writer->SetFileName(argv[2]);

    writer->SetInput(filter->GetOutput());

    std::cout << "Writing the image as " << std::endl << std::endl;
    std::cout << argv[2] << std::endl << std::endl;


    try
    {
      writer->Update();
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cout << ex << std::endl;
      return EXIT_FAILURE;
    }
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
