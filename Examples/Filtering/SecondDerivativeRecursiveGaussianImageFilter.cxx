/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SecondDerivativeRecursiveGaussianImageFilter.cxx
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
//  This example illustrates how to compute second derivatives of
//  a 3D image using the \doxygen{RecursiveGaussianImageFilter}.
//
//  In this example, all the second derivatives are computed independently in
//  the same way as if they were intended to be used for building the Hessian
//  matrix of the image.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkRecursiveGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageDuplicator.h"
#include "itkImage.h"
#include <string>


int main(int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputPrefix  [sigma] " << std::endl;
    return EXIT_FAILURE;
    }

  typedef float            PixelType;
  typedef float            OutputPixelType;

  const unsigned int  Dimension = 3;

  typedef itk::Image< PixelType,       Dimension >  ImageType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;
 
  typedef itk::ImageFileReader< ImageType       >   ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >   WriterType;

  typedef itk::ImageDuplicator< OutputImageType >   DuplicatorType;

  typedef itk::RecursiveGaussianImageFilter< 
                                      ImageType, 
                                      ImageType >  FilterType;

  ReaderType::Pointer  reader  = ReaderType::New();
  WriterType::Pointer  writer  = WriterType::New();

  DuplicatorType::Pointer duplicator  = DuplicatorType::New();

  reader->SetFileName( argv[1] );
  
  std::string outputPrefix = argv[2];
  std::string outputFileName;

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem reading the input file" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  FilterType::Pointer ga = FilterType::New();
  FilterType::Pointer gb = FilterType::New();
  FilterType::Pointer gc = FilterType::New();

  ga->SetDirection( 0 );
  gb->SetDirection( 1 );
  gc->SetDirection( 2 );

  if( argc > 3 )
    {
    const float sigma = atof( argv[3] );
    ga->SetSigma( sigma );
    gb->SetSigma( sigma );
    gc->SetSigma( sigma );
    }

  ga->SetZeroOrder();
  gb->SetZeroOrder();
  gc->SetSecondOrder();

  ImageType::Pointer inputImage = reader->GetOutput();

  ga->SetInput( inputImage );
  gb->SetInput( ga->GetOutput() );
  gc->SetInput( gb->GetOutput() );

  duplicator->SetInputImage( gc->GetOutput() );


  gc->Update(); 
  duplicator->Update();

  ImageType::Pointer Izz = duplicator->GetOutput();

  writer->SetInput( Izz );
  outputFileName = outputPrefix + "-Izz.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  gc->SetDirection( 1 );  // gc now works along Y
  gb->SetDirection( 2 );  // gb now works along Z

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Iyy = duplicator->GetOutput();

  writer->SetInput( Iyy );
  outputFileName = outputPrefix + "-Iyy.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();


  gc->SetDirection( 0 );  // gc now works along X
  ga->SetDirection( 1 );  // ga now works along Y

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Ixx = duplicator->GetOutput();

  writer->SetInput( Ixx );
  outputFileName = outputPrefix + "-Ixx.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();


  ga->SetDirection( 0 );
  gb->SetDirection( 1 );
  gc->SetDirection( 2 );

  ga->SetZeroOrder();
  gb->SetFirstOrder();
  gc->SetFirstOrder();

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Iyz = duplicator->GetOutput();

  writer->SetInput( Iyz );
  outputFileName = outputPrefix + "-Iyz.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();


  ga->SetDirection( 1 );
  gb->SetDirection( 0 );
  gc->SetDirection( 2 );

  ga->SetZeroOrder();
  gb->SetFirstOrder();
  gc->SetFirstOrder();

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Ixz = duplicator->GetOutput();

  writer->SetInput( Ixz );
  outputFileName = outputPrefix + "-Ixz.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  ga->SetDirection( 2 );
  gb->SetDirection( 0 );
  gc->SetDirection( 1 );

  ga->SetZeroOrder();
  gb->SetFirstOrder();
  gc->SetFirstOrder();

  gc->Update();
  duplicator->Update();

  ImageType::Pointer Ixy = duplicator->GetOutput();

  writer->SetInput( Ixy );
  outputFileName = outputPrefix + "-Ixy.mhd";
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();

  // Software Guide : EndCodeSnippet
  
return EXIT_SUCCESS;
}


