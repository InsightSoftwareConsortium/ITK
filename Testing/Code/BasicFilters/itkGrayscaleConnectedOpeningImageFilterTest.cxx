/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleConnectedOpeningImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

#include "itkGrayscaleConnectedOpeningImageFilter.h"


int itkGrayscaleConnectedOpeningImageFilterTest( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile seedX seedY " << std::endl;
    return 1;
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
  typedef itk::GrayscaleConnectedOpeningImageFilter<
                            InputImageType, 
                            OutputImageType >  ConnectedOpeningFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  
  // Create the filter
  ConnectedOpeningFilterType::Pointer  connectedOpening = ConnectedOpeningFilterType::New();
  FilterWatcher watcher(connectedOpening, "Opening"); watcher.QuietOn();

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );
  
  // Setup the connected opening method
  connectedOpening->SetInput(  reader->GetOutput() );

  InputImageType::IndexType seed;
  seed[0] = atoi(argv[3]);
  seed[1] = atoi(argv[4]);
  connectedOpening->SetSeed(seed);
  
  // Run the filter
  writer->SetInput( connectedOpening->GetOutput() );
  writer->Update();

  // Output the number of iterations used
  std::cout << "ConnectedOpening took " << connectedOpening->GetNumberOfIterationsUsed() << " iterations." << std::endl;
  std::cout << "<DartMeasurement name=\"NumberOfIterations\" type=\"numeric/integer\">" << connectedOpening->GetNumberOfIterationsUsed() << "</DartMeasurement>" << std::endl;
  

  return 0;

}

