/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDICOMImageSeriesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDICOMImageIO2Factory.h"
#include "itkImageSeriesReader.h"
#include "itkDICOMSeriesFileNames.h"
#include "../BasicFilters/itkFilterWatcher.h"

int itkDICOMImageSeriesTest(int ac, char* av[])
{

  if(ac < 3)
  {
    std::cerr << "Usage: " << av[0] << " DicomDirectory ReverseOrder(0/1)\n";
    return EXIT_FAILURE;
  }

  typedef itk::Image<unsigned short,3> Image3DType;
  typedef itk::ImageSeriesReader<Image3DType> ReaderType;

  // Register on factory capable of creating DicomImage readers
  itk::DICOMImageIO2Factory::RegisterOneFactory();

  // Get the DICOM filenames from the directory
  itk::DICOMSeriesFileNames::Pointer names = itk::DICOMSeriesFileNames::New();
  names->SetDirectory(av[1]);
  
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileNames(names->GetFileNames());
  std::cout << names;

  FilterWatcher watcher(reader);

  try
    {
    if (atoi(av[2]))
      {
      reader->ReverseOrderOn();
      }
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
