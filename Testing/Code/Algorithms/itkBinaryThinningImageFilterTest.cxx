/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThinningImageFilterTest.cxx
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

#include "itkImageFileReader.h"
#include "itkBinaryThinningImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

int itkBinaryThinningImageFilterTest(int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile";  
    std::cerr << std::endl;  
    return EXIT_FAILURE;
    }

  typedef  short InputPixelType;
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::BinaryThinningImageFilter< InputImageType, InputImageType >  ThinningType;
  typedef itk::RescaleIntensityImageFilter< InputImageType, OutputImageType > RescaleType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  ThinningType::Pointer thinning = ThinningType::New();
  RescaleType::Pointer rescaler = RescaleType::New();
  WriterType::Pointer writer = WriterType::New();

  // Set up the reader
  reader->SetFileName( argv[1] );

  // Set up the filter parameters.
  thinning->SetInput( reader->GetOutput() );

  // Rescale the image so that it can be seen.
  rescaler->SetInput( thinning->GetOutput() );
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  // Write out the test image
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
