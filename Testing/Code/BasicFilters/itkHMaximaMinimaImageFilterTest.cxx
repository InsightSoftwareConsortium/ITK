/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHMaximaMinimaImageFilterTest.cxx
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

//  

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkHMaximaImageFilter.h"
#include "itkHMinimaImageFilter.h"

int itkHMaximaMinimaImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile  height" << std::endl;
    return EXIT_FAILURE;
    }

  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;
  
  typedef unsigned short   InputPixelType;
  typedef short            InternalPixelType;
  typedef unsigned char    OutputPixelType;
  typedef unsigned char    WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< InternalPixelType,  Dimension > InternalImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >     WriteImageType;


  // readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >   WriterType;

  // define the hmaxima filter
  typedef itk::HMaximaImageFilter<
                            InputImageType, 
                            InternalImageType >  HmaximaFilterType;
  // define the hminima filter
  typedef itk::HMinimaImageFilter<
                            InternalImageType, 
                            OutputImageType >  HminimaFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  
  // Create the filters
  HmaximaFilterType::Pointer  hmaxima = HmaximaFilterType::New();
  FilterWatcher watchHmaxima(hmaxima,"hmaxima");
  HminimaFilterType::Pointer  hminima = HminimaFilterType::New();

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );
  
  // Setup the hmaxima method
  hmaxima->SetInput(  reader->GetOutput() );
  hmaxima->SetHeight( static_cast<InputPixelType>(atof(argv[3])) );

  // Setup the hminima method
  hminima->SetInput(  hmaxima->GetOutput() );
  hminima->SetHeight( static_cast<InputPixelType>(atof(argv[3])) );

  // Run the filter
  writer->SetInput( hminima->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;

}

