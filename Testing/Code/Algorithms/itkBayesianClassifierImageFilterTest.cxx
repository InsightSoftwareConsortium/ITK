/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBayesianClassifierImageFilterTest.cxx
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

#include "itkBayesianClassifierImageFilter.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"


int itkBayesianClassifierImageFilterTest(int argc, char* argv[] )
{

  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile numberOfClasses" << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned char InputComponentType;
  typedef itk::Vector<InputComponentType, 3> InputPixelType;

  typedef unsigned long OutputPixelType;


  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType, Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType >     ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >    WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  const unsigned int numberOfClasses = atoi( argv[3] );



  typedef itk::BayesianClassifierImageFilter< 
                                 InputImageType,
                                 OutputImageType >  ClassifierFilterType;


  ClassifierFilterType::Pointer filter = ClassifierFilterType::New();

  filter->SetInput( reader->GetOutput() );





  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
