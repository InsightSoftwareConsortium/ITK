/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleConnectedClosingImageFilterTest.cxx
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

#include "itkGrayscaleConnectedClosingImageFilter.h"


int itkGrayscaleConnectedClosingImageFilterTest( int argc, char * argv[] )
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

  // define the connected closing filter
  typedef itk::GrayscaleConnectedClosingImageFilter<
                            InputImageType, 
                            OutputImageType >  ConnectedClosingFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  
  // Create the filter
  ConnectedClosingFilterType::Pointer  connectedClosing = ConnectedClosingFilterType::New();
  FilterWatcher watcher(connectedClosing, "connectedClosing");

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );
  
  // Setup the connectedopening method
  connectedClosing->SetInput(  reader->GetOutput() );

  InputImageType::IndexType seed;
  seed[0] = atoi(argv[3]);
  seed[1] = atoi(argv[4]);
  connectedClosing->SetSeed(seed);
  
  // Run the filter
  writer->SetInput( connectedClosing->GetOutput() );
  writer->Update();

  // Output the number of iterations used
  std::cout << "ConnectedClosing took " << connectedClosing->GetNumberOfIterationsUsed() << " iterations." << std::endl;
  std::cout << "<DartMeasurement name=\"NumberOfIterations\" type=\"numeric/integer\">" << connectedClosing->GetNumberOfIterationsUsed() << "</DartMeasurement>" << std::endl;
  

  return 0;

}

