/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RGBToGrayscale.cxx
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

//  Software Guide : BeginLatex
//
//  This example illustrates how to convert an RGB image into a grayscale one.
//  The \doxygen{RGBToLuminanceImageFilter} is the central piece of this example.
//
//  \index{itk::RGBToLuminanceImageFilter!RGB Images}
//
//  Software Guide : EndLatex 


//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::RGBToLuminanceImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkRGBToLuminanceImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkRGBPixel.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char * argv[] )
{
  if( argc < 3 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputRGBImageFile  outputGrayscaleImageFile " << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef   itk::RGBPixel< unsigned char >            InputPixelType;
  typedef   itk::Image< InputPixelType, Dimension >   InputImageType;
  typedef   itk::Image< unsigned char,  Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );


  typedef itk::RGBToLuminanceImageFilter< 
                                 InputImageType, 
                                 OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( reader->GetOutput() );



  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );

  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception Thrown" << std::endl;
    std::cerr << excp << std::endl;
    }

  return EXIT_SUCCESS;
}

