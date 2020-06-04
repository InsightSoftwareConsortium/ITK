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
#include "itkNumericSeriesFileNames.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkMetaDataObject.h"

#include <vector>
#include "itksys/SystemTools.hxx"

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a 3D image from a non DICOM file and
//  write it as a series of DICOM slices. with some changed header
//  information. Header
//
//  Please note that modifying the content of a DICOM header is a very risky
//  operation. The Header contains fundamental information about the patient
//  and therefore its consistency must be protected from any data corruption.
//  Before attempting to modify the DICOM headers of your files, you must make
//  sure that you have a very good reason for doing so, and that you can
//  ensure that this information change will not result in a lower quality of
//  health care to be delivered to the patient.
//
//  \index{DICOM!Writing Series}
//
//  Software Guide : EndLatex


int
main(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  OutputDicomDirectory" << std::endl;
    return EXIT_FAILURE;
  }


  using PixelType = signed short;
  constexpr unsigned int Dimension = 3;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  using ImageIOType = itk::GDCMImageIO;
  using NamesGeneratorType = itk::NumericSeriesFileNames;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();

  const char * outputDirectory = argv[2];

  itksys::SystemTools::MakeDirectory(outputDirectory);


  using OutputPixelType = signed short;
  constexpr unsigned int OutputDimension = 2;

  using Image2DType = itk::Image<OutputPixelType, OutputDimension>;

  using SeriesWriterType = itk::ImageSeriesWriter<ImageType, Image2DType>;

  NamesGeneratorType::Pointer namesGenerator = NamesGeneratorType::New();

  itk::MetaDataDictionary & dict = gdcmIO->GetMetaDataDictionary();
  std::string               tagkey, value;
  tagkey = "0008|0060"; // Modality
  value = "MR";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0008|0008"; // Image Type
  value = "DERIVED\\SECONDARY";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0008|0064"; // Conversion Type
  value = "DV";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);


  SeriesWriterType::Pointer seriesWriter = SeriesWriterType::New();

  seriesWriter->SetInput(reader->GetOutput());
  seriesWriter->SetImageIO(gdcmIO);


  ImageType::RegionType region =
    reader->GetOutput()->GetLargestPossibleRegion();

  ImageType::IndexType start = region.GetIndex();
  ImageType::SizeType  size = region.GetSize();


  std::string format = outputDirectory;

  format += "/image%03d.dcm";

  namesGenerator->SetSeriesFormat(format.c_str());

  namesGenerator->SetStartIndex(start[2]);
  namesGenerator->SetEndIndex(start[2] + size[2] - 1);
  namesGenerator->SetIncrementIndex(1);


  seriesWriter->SetFileNames(namesGenerator->GetFileNames());


  try
  {
    seriesWriter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown while writing the series " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
