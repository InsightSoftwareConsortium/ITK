/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageKmeansImageFilterTest.cxx
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
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarImageKmeansImageFilter.h"
#include "itkRelabelComponentImageFilter.h"


int itkScalarImageKmeansImageFilterTest(int argc, char* argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << " inputScalarImage outputLabeledImage numberOfClasses mean1 mean2... meanN " << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned char       PixelType;
  const unsigned int          Dimension = 2;

  typedef itk::Image<PixelType, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::ScalarImageKmeansImageFilter< ImageType > KMeansFilterType;

  KMeansFilterType::Pointer kmeansFilter = KMeansFilterType::New();

  kmeansFilter->SetInput( reader->GetOutput() );

  kmeansFilter->SetUseNonContiguousLabels( argv[3] );

  const unsigned int numberOfInitialClasses = atoi( argv[4] );

  const unsigned int numberOfArgumentsBeforeMeans = 5;
  if( static_cast<unsigned int>(argc) < numberOfInitialClasses + numberOfArgumentsBeforeMeans )
    {
    std::cerr << "Error: " << std::endl;
    std::cerr << numberOfInitialClasses << " classes has been specified ";
    std::cerr << "but no enough means have been provided in the command ";
    std::cerr << "line arguments " << std::endl;
    return EXIT_FAILURE;
    }

  for(unsigned k=0; k<numberOfInitialClasses; k++)
    {
    kmeansFilter->AddClassWithInitialMean( atof( argv[k+numberOfArgumentsBeforeMeans] ) );
    }


  try
    {
    kmeansFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while classifying the image " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  KMeansFilterType::ParametersType estimatedMeans = kmeansFilter->GetFinalMeans();

  const unsigned int numberOfClasses = estimatedMeans.Size();

  for ( unsigned int i = 0 ; i < numberOfClasses ; ++i )
    {
    std::cout << "cluster[" << i << "] ";
    std::cout << "    estimated mean : " << estimatedMeans[i] << std::endl;
    }

  typedef KMeansFilterType::OutputImageType  OutputImageType;

  typedef itk::RelabelComponentImageFilter< 
                                OutputImageType,
                                OutputImageType > RelabelFilterType;
                        
    
  RelabelFilterType::Pointer relabeler = RelabelFilterType::New();

  relabeler->SetInput( kmeansFilter->GetOutput() );




  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  
  writer->SetInput( relabeler->GetOutput() );

  writer->SetFileName( argv[2] );


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while writing image file : " << argv[2] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typedef std::vector< unsigned long > SizesType;
  
  const SizesType &  sizes = relabeler->GetSizeOfObjectsInPixels();

  SizesType::const_iterator sizeItr = sizes.begin();
  SizesType::const_iterator sizeEnd = sizes.end();

  std::cout << "Number of pixels per class " << std::endl;
  unsigned int kclass = 0;
  while( sizeItr != sizeEnd )
    {
    std::cout << "Class " << kclass << " = " << *sizeItr << std::endl;
    ++kclass;
    ++sizeItr;
    }
  
  return EXIT_SUCCESS;
}



