/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMSeriesStreamReadImageWrite.cxx
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

#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkImageSeriesWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"

int main( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << 
      " DicomDirectory  outputFile " << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  typedef itk::GDCMImageIO                        ImageIOType;
  typedef itk::GDCMSeriesFileNames                SeriesFileNames;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();
  SeriesFileNames::Pointer it = SeriesFileNames::New();

  // Get the DICOM filenames from the directory
  // First add a restriction *before* selecting the input directory
  // since SetInputDirectory has a side effect of executing
  gdcm::SerieHelper *sh = it->GetSeriesHelper( );
  it->SetInputDirectory( argv[1] );

  ReaderType::Pointer reader = ReaderType::New();

  const ReaderType::FileNamesContainer & filenames = it->GetInputFileNames();
  unsigned int numberOfFilenames =  filenames.size();
  std::cout << numberOfFilenames << std::endl; 
  for(unsigned int fni = 0; fni<numberOfFilenames; fni++)
    {
    std::cout << "filename # " << fni << " = ";
    std::cout << filenames[fni] << std::endl;
    }
  
  reader->SetFileNames( filenames );
  reader->SetImageIO( gdcmIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;

    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->SetInput( reader->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;

    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

