/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    AntiAliasBinaryImageFilter.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

//  Software Guide : BeginLatex
//
//  This example introduces the use of the \doxygen{AntiAliasBinaryImageFilter}. This
//  filter expect a binary mask as input, and using Level Sets it smooths the
//  image by keeping the edge of the structure within 1 pixel distance from the
//  original location. It is usually desirable to run this filter before
//  extracting isocontour with surface extraction methods. 
//  
//  \index{itk::AntiAliasBinaryImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::AntiAliasBinaryImageFilter!header}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkAntiAliasBinaryImageFilter.h"
// Software Guide : EndCodeSnippet


int main(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage [RMS] [numberOfIterations]" << std::endl;
    return EXIT_FAILURE;
    }
   
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];
  double maximumRMSError = 0.01;
  unsigned int numberOfIterations = 50;

  if( argc > 3 )
    {
    maximumRMSError = atof( argv[3] );
    }

  if( argc > 4 )
    {
    numberOfIterations = atoi( argv[4] );
    }


  typedef unsigned char    CharPixelType;  //  IO
  typedef double           RealPixelType;  //  Operations
  const   unsigned int     Dimension = 3;

  typedef itk::Image<CharPixelType, Dimension>    CharImageType;
  typedef itk::Image<RealPixelType, Dimension>    RealImageType;

  typedef itk::ImageFileReader< CharImageType >  ReaderType;
  typedef itk::ImageFileWriter< CharImageType >  WriterType;




  //  Software Guide : BeginLatex
  //
  //  This filter operates on image of pixel type float. It is then necessary
  //  to cast the type of the input images that are usually of integer type.
  //  The \doxygen{CastImageFilter} is used here for that purpose. Its image 
  //  template parameters are defined for casting from the input type to the
  //  float type using for processing.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< CharImageType, RealImageType> CastToRealFilterType;
  // Software Guide : EndCodeSnippet



  typedef itk::RescaleIntensityImageFilter<RealImageType, CharImageType > RescaleFilter;


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{AntiAliasBinaryImageFilter} is instantiated using the float image type.
  //
  //  \index{itk::AntiAliasBinaryImageFilter|textbf}
  //
  //  Software Guide : EndLatex


  typedef itk::AntiAliasBinaryImageFilter<RealImageType, RealImageType> AntiAliasFilterType;

  //Setting the IO

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  CastToRealFilterType::Pointer toReal = CastToRealFilterType::New();
  RescaleFilter::Pointer rescale = RescaleFilter::New();

  //Setting the ITK pipeline filter

  // Software Guide : BeginCodeSnippet
  AntiAliasFilterType::Pointer antiAliasFilter = AntiAliasFilterType::New();

  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );

  //The output of an edge filter is 0 or 1
  rescale->SetOutputMinimum(   0 );
  rescale->SetOutputMaximum( 255 );

  toReal->SetInput( reader->GetOutput() );

  antiAliasFilter->SetInput( toReal->GetOutput() );
  
  antiAliasFilter->SetMaximumRMSError( maximumRMSError );
  antiAliasFilter->SetNumberOfIterations( numberOfIterations );
  antiAliasFilter->SetNumberOfLayers( 2 );
  
  rescale->SetInput( antiAliasFilter->GetOutput() );
  writer->SetInput( rescale->GetOutput() );
  try 
    {
    writer->Update();
    std::cout << "Completed in " << antiAliasFilter->GetNumberOfIterations() << std::endl;
    }
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return EXIT_FAILURE;
    } 

  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}
