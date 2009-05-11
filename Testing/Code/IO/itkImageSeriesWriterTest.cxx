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
#include "itkNumericSeriesFileNames.h"
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


  { // This API is being deprecated. Please use NumericSeriesFileNames in the future
    // for generating the list of filenames.  This API will be removed after ITK 1.8
  typedef  itk::ImageSeriesWriter<RescaleImageType,OutputImageType> WriterType; 

  WriterType::Pointer writer = WriterType::New();

  FilterWatcher watcher2(writer);

  writer->SetInput(rescaler->GetOutput());
  char format[4096];
  sprintf (format, "%s/series.%%d.%s", av[2], av[3]); 
  writer->SetSeriesFormat(format);

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with old API" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Verify that attempting to use a MetaDataDictionary without setting the ImageIO
  // should throw an exception.
  writer->SetMetaDataDictionaryArray( reader->GetMetaDataDictionaryArray() );
  try
    {
    writer->Update();
    std::cerr << "Failed to throw expected exception of using MetaDataDictionary without ImageIO" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Exercised expected exception" << std::endl;
    }

  std::cout << "Old API PASSED !" << std::endl;
  }
 
  { // This is the new API, using the NumericSeriesFileNames (or any other filename generator).
  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  typedef  itk::ImageSeriesWriter<RescaleImageType,OutputImageType> WriterType; 

  WriterType::Pointer writer = WriterType::New();


  char format[4096];
  sprintf (format, "%s/series.%%d.%s", av[2], av[3]); 

  std::cout << "Format = " << format << std::endl;

  ImageNDType::RegionType region = reader->GetOutput()->GetBufferedRegion();
  ImageNDType::SizeType   size = region.GetSize();

  fit->SetStartIndex(0);
  fit->SetEndIndex(size[2]-1);  // The number of slices to write
  fit->SetIncrementIndex(1);
  fit->SetSeriesFormat (format);

  writer->SetInput(rescaler->GetOutput());
  writer->SetFileNames(  fit->GetFileNames() );

  // experiment the UseCompression methods and values
  if( writer->GetUseCompression() )
    {
    std::cerr << "Wrong default use compression value" << std::endl;
    return EXIT_FAILURE;
    }
  writer->SetUseCompression( true );
  writer->UseCompressionOn();
  writer->UseCompressionOff();

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with SeriesFileNames generator" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Verify that attempting to use a MetaDataDictionary without setting the ImageIO
  // should throw an exception.
  writer->SetMetaDataDictionaryArray( reader->GetMetaDataDictionaryArray() );
  try
    {
    writer->Update();
    std::cerr << "Failed to throw expected exception of using MetaDataDictionary without ImageIO" << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Exercised expected exception" << std::endl;
    }

  std::cout << "Test with NumericSeriesFileNames PASSED !" << std::endl;
  }
  
  return EXIT_SUCCESS;

}
