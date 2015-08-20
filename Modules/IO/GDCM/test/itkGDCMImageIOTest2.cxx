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
#include "itkGDCMImageIO.h"
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"

#include <sstream>

#define SPECIFIC_IMAGEIO_MODULE_TEST

// Take as input any kind of image (typically RAW) and will create a valid
// DICOM MR image out of the raw image.
int itkGDCMImageIOTest2(int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " InputFile OutputDicomRoot\n";
    return EXIT_FAILURE;
    }
  const char *input = argv[1];
  const char *output = argv[2];
  typedef itk::Image<unsigned char,3>     ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typedef itk::ImageFileWriter<ImageType> WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(input);
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  itk::GDCMImageIO::Pointer dicomIO = itk::GDCMImageIO::New();
  //reader->GetOutput()->Print(std::cout);

  itk::MetaDataDictionary & dict = dicomIO->GetMetaDataDictionary();
  std::string tagkey, value;
  tagkey = "0002|0002";
  value = "1.2.840.10008.5.1.4.1.1.4"; // Media Storage SOP Class UID
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value );
  tagkey = "0008|0060"; // Modality
  value = "MR";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value );
  tagkey = "0008|0008"; // Image Type
  value = "DERIVED\\SECONDARY";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0008|0064"; // Conversion Type
  value = "DV";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|5100"; // Patient Position
  value = "HFS";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0020|1040"; // Position Reference Indicator
  value = "";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0020"; // Scanning Sequence
  value = "GR";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0021"; // Sequence Variant
  value = "SS\\SP";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0022"; // Scan Options
  value = "";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0023"; // MRA Acquisition Type
  value = "";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0080"; // Repetition Time
  value = "";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0081"; // Echo Time
  value = "";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0091"; // Echo Train Length
  value = "";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0018|0050"; // Slice Thickness
  std::ostringstream os;
  os << reader->GetImageIO()->GetSpacing(2);
  value = os.str();
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);

  // Construct different filename based on the root given
  std::string output_j2k = output;
  output_j2k += "-j2k.dcm";
  std::string output_jpll = output;
  output_jpll += "-jpll.dcm";
  std::string output_raw = output;
  output_raw += "-raw.dcm";

  WriterType::Pointer writer = WriterType::New();
  writer->SetImageIO(dicomIO);
  writer->SetInput(reader->GetOutput());
  writer->UseInputMetaDataDictionaryOff();

  // Save as JPEG 2000 Lossless
  // Explicitely specify which compression type to use
  dicomIO->SetCompressionType(itk::GDCMImageIO::JPEG2000);
  // Request compression of the ImageIO
  writer->UseCompressionOn();
  writer->SetFileName(output_j2k.c_str());
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Save as JPEG Lossless
  dicomIO->SetCompressionType(itk::GDCMImageIO::JPEG);
  writer->SetFileName(output_jpll.c_str());
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Save as raw
  writer->UseCompressionOff();
  writer->SetFileName(output_raw.c_str());
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
