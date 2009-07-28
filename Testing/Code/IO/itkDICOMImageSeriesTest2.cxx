/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMImageSeriesTest2.cxx
  Language:  C++
  Date:      $Date$xgoto-l

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
#include "itkDICOMImageIO2Factory.h"
#include "itkDICOMImageIO2.h"
#include "itkImageSeriesReader.h"
#include "itkDICOMSeriesFileNames.h"
#include "../BasicFilters/itkFilterWatcher.h"

int itkDICOMImageSeriesTest2(int ac, char* av[])
{

  if( ac < 2 )
    {
    std::cerr << "Usage: " << av[0] << " DicomDirectory \n";
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,3> ImageNDType;
  typedef itk::ImageSeriesReader<ImageNDType> ReaderType;

  itk::DICOMImageIO2::Pointer io = itk::DICOMImageIO2::New();

  // Get the DICOM filenames from the directory
  itk::DICOMSeriesFileNames::Pointer names = itk::DICOMSeriesFileNames::New();
  names->SetDirectory(av[1]);
  
  ReaderType::Pointer reader = ReaderType::New();

  typedef itk::DICOMSeriesFileNames::FileNamesArrayType  FileNamesArrayType;

  FileNamesArrayType filenames = names->GetFileNames();

  FileNamesArrayType selectedFilenames;

  // Take only one filename
  selectedFilenames.push_back( filenames[0] );

  std::cout << "Reading only " << selectedFilenames[0] << std::endl;

  reader->SetFileNames( selectedFilenames );
  reader->SetImageIO(io);
  std::cout << names;

  FilterWatcher watcher(reader);

  try
    {
    reader->Update();
    reader->GetOutput()->Print(std::cout);
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
