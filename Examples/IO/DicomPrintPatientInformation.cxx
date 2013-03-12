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

//  Software Guide : BeginLatex
//
//  This example illustrates how to print out some patient information from the
//  header of the DICOM file.
//
//  Software Guide : EndLatex

#include "itkGDCMImageIO.h"
#include "itkImageFileReader.h"
#include "itkMetaDataObject.h"

std::string FindDicomTag( const std::string & entryId, const itk::GDCMImageIO::Pointer dicomIO )
{
  std::string tagvalue;
  bool found = dicomIO->GetValueFromTag(entryId, tagvalue);
  if ( !found )
    {
    tagvalue = "NOT FOUND";
    }
  return tagvalue;
}


int main( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomFile " << std::endl;
    return EXIT_FAILURE;
    }

  typedef signed short       PixelType;
  const unsigned int         Dimension = 2;

  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  itk::GDCMImageIO::Pointer dicomIO = itk::GDCMImageIO::New();

  reader->SetFileName( argv[1] );
  reader->SetImageIO( dicomIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }


  std::string patientName  = FindDicomTag("0010|0010", dicomIO);
  std::string patientID    = FindDicomTag("0010|0020", dicomIO);
  std::string patientSex   = FindDicomTag("0010|0040", dicomIO);
  std::string patientAge   = FindDicomTag("0010|1010", dicomIO);
  std::string studyDate    = FindDicomTag("0008|0020", dicomIO);
  std::string modality     = FindDicomTag("0008|0060", dicomIO);
  std::string manufacturer = FindDicomTag("0008|0070", dicomIO);
  std::string institution  = FindDicomTag("0008|0080", dicomIO);
  std::string model        = FindDicomTag("0008|1090", dicomIO);


  std::cout << "Patient Name : " << patientName  << std::endl;
  std::cout << "Patient ID   : " << patientID    << std::endl;
  std::cout << "Patient Sex  : " << patientSex   << std::endl;
  std::cout << "Patient Age  : " << patientAge   << std::endl;
  std::cout << "Study Date   : " << studyDate    << std::endl;
  std::cout << "Modality     : " << modality     << std::endl;
  std::cout << "Manufacturer : " << manufacturer << std::endl;
  std::cout << "Institution  : " << institution  << std::endl;
  std::cout << "Model        : " << model        << std::endl;

  return EXIT_SUCCESS;

}
