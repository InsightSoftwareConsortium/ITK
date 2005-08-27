/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomPrintPatientInformation.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
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
