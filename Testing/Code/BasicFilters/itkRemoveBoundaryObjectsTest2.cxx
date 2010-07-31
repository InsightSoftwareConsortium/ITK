/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRemoveBoundaryObjectsTest2.cxx
  Language:  C++
  Date:      $Date$
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

//  

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkGrayscaleGrindPeakImageFilter.h"
#include "itkXorImageFilter.h"

int itkRemoveBoundaryObjectsTest2( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile  " << std::endl;
    return EXIT_FAILURE;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;
  
  typedef unsigned char   InputPixelType;
  typedef unsigned char   OutputPixelType;
  typedef unsigned char   WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >    WriteImageType;


  // readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  // define the fillhole filter
  typedef itk::GrayscaleGrindPeakImageFilter<
                            InputImageType, 
                            OutputImageType >  GrindPeakFilterType;

  // define the xor and not filters
  typedef itk::XorImageFilter<InputImageType, InputImageType, OutputImageType>
    XorFilterType;

  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  
  // Create the filter
  GrindPeakFilterType::Pointer  grindpeak = GrindPeakFilterType::New();

  // Create the xor and not filter
  XorFilterType::Pointer xorfilter = XorFilterType::New();
  
  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Setup the grindpeak method
  grindpeak->SetInput( reader->GetOutput() );

  // Setup the xor and not
  xorfilter->SetInput1( grindpeak->GetOutput() );
  xorfilter->SetInput2( reader->GetOutput() );

  // Run the filter
  writer->SetInput( xorfilter->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}

