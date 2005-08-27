/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    LaplacianSharpeningImageFilter.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkLaplacianSharpeningImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

int main(int argc, char* argv[])
{
  if( argc < 3)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
    }
   
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];

  typedef unsigned char    CharPixelType; 
  const    unsigned int    Dimension = 2;

  typedef itk::Image<CharPixelType, Dimension>    CharImageType;
  
  typedef itk::ImageFileReader< CharImageType >  ReaderType;
  typedef itk::ImageFileWriter< CharImageType >  WriterType;

  typedef itk::RescaleIntensityImageFilter<CharImageType, CharImageType> RescaleFilter;

  typedef itk::LaplacianSharpeningImageFilter< 
                              CharImageType, 
                              CharImageType >    LaplacianSharpeningFilter;


  //Setting the IO
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  RescaleFilter::Pointer rescale = RescaleFilter::New();

  //Setting the ITK pipeline filter
  
  LaplacianSharpeningFilter::Pointer lapFilter = LaplacianSharpeningFilter::New();
  
  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );

  //Sharpen with the laplacian
  lapFilter->SetInput( reader->GetOutput() );

  // Rescale and cast to unsigned char
  rescale->SetInput( lapFilter->GetOutput() );
  writer->SetInput( rescale->GetOutput() );

  rescale->SetOutputMinimum(   0 );
  rescale->SetOutputMaximum( 255 );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return EXIT_FAILURE;
    } 

  return EXIT_SUCCESS;

}
