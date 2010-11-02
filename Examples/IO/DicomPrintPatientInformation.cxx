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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

//  Software Guide : BeginLatex
//
//  This example illustrates how to print out some patient information from the
//  header of the DICOM file.
//
//  Software Guide : EndLatex


#include "itkDICOMImageIO2.h"
#include "itkImageFileReader.h"

int main( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomFile " << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<signed short,2>              ImageType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;

  itk::DICOMImageIO2::Pointer dicomIO = itk::DICOMImageIO2::New();

  ReaderType::Pointer reader = ReaderType::New();

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


  const unsigned int length = 2048;

  char patientName[  length ];
  char patientID[    length ];
  char patientSex[   length ];
  char patientAge[   length ];
  char studyDate[    length ];
  char modality[     length ];
  char manufacturer[ length ];
  char institution[  length ];
  char model[        length ];

  dicomIO->GetPatientName(  patientName  );
  dicomIO->GetPatientID(    patientID    );
  dicomIO->GetPatientSex(   patientSex   );
  dicomIO->GetPatientAge(   patientAge   );
  dicomIO->GetStudyDate(    studyDate    );
  dicomIO->GetModality(     modality     );
  dicomIO->GetManufacturer( manufacturer );
  dicomIO->GetInstitution(  institution  );
  dicomIO->GetModel(        model        );

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
