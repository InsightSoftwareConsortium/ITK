/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesWriterTest.cxx
  Language:  C++
  Date:      $Date$xgoto-l

  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageSeriesWriter.h"
#include "../BasicFilters/itkFilterWatcher.h"

int itkImageSeriesWriterTest(int ac, char* av[])
{

  if(ac < 4)
  {
    std::cerr << "Usage: " << av[0] << " DicomDirectory OutputDirectory FileSuffix\n";
    return EXIT_FAILURE;
  }

  typedef itk::Image<short,3> ImageNDType;
  typedef itk::ImageSeriesReader<ImageNDType> ReaderType;

  itk::DICOMImageIO2::Pointer io = itk::DICOMImageIO2::New();

  // Get the DICOM filenames from the directory
  itk::DICOMSeriesFileNames::Pointer names = itk::DICOMSeriesFileNames::New();
  names->SetDirectory(av[1]);
  names->SetFileNameSortingOrderToSortByImageNumber();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileNames(names->GetFileNames());
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

  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, 3 > RescaleImageType;
  typedef itk::Image< WritePixelType, 2 > OutputImageType;

  typedef itk::RescaleIntensityImageFilter< 
               ImageNDType, RescaleImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
    rescaler->SetInput(reader->GetOutput());
    rescaler->SetOutputMinimum(   0 );
    rescaler->SetOutputMaximum( 255 );
    rescaler->UpdateLargestPossibleRegion();

  typedef  itk::ImageSeriesWriter<RescaleImageType,OutputImageType> WriterType; 

  WriterType::Pointer writer = WriterType::New();
  FilterWatcher watcher2(writer);
  
  writer->SetInput(rescaler->GetOutput());
  char format[4096];
  sprintf (format, "%s/series.%%d.%s", av[2], av[3]); 
  writer->SetSeriesFormat(format);
  writer->Update();
 
  return EXIT_SUCCESS;

}
