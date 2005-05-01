/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SubsampleVolume.cxx
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

//  Software Guide : BeginLatex
//
//  This example illustrates how to perform subsampling of a volume using ITK
//  classes.  In order to avoid aliasing artifacts, the volume must be
//  processed by a low-pass filter before resampling.  Here we use the
//  \doxygen{RecursiveGaussianImageFilter} as low-pass filter.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkCastImageFilter.h"


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] 
      << "  inputImageFile  outputImageFile factorX factorY factorZ" 
      << std::endl; 
    return EXIT_FAILURE;
    }

  const     unsigned int    Dimension = 3;

  typedef   unsigned char   InputPixelType;

  typedef   float           InternalPixelType;
  typedef   unsigned char   OutputPixelType;

  typedef itk::Image< InputPixelType,    Dimension >   InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >   InternalImageType;
  typedef itk::Image< OutputPixelType,   Dimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  const double factorX = atof( argv[3] );
  const double factorY = atof( argv[4] );
  const double factorZ = atof( argv[5] );

  try 
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  InputImageType::ConstPointer inputImage = reader->GetOutput();


  typedef itk::CastImageFilter< InputImageType,
                                InternalImageType >   CastFilterType;

  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput( inputImage );

  typedef itk::RecursiveGaussianImageFilter< 
                                  InternalImageType,
                                  InternalImageType > GaussianFilterType;

  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherY = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherZ = GaussianFilterType::New();

  smootherX->SetInput( caster->GetOutput() );
  smootherY->SetInput( smootherX->GetOutput() );
  smootherZ->SetInput( smootherY->GetOutput() );

  const InputImageType::SpacingType& inputSpacing = inputImage->GetSpacing();

  const double sigmaX = inputSpacing[0] * factorX;
  const double sigmaY = inputSpacing[1] * factorY;
  const double sigmaZ = inputSpacing[2] * factorZ;

  smootherX->SetSigma( sigmaX );
  smootherY->SetSigma( sigmaY );
  smootherZ->SetSigma( sigmaZ );

  smootherX->SetDirection( 0 );
  smootherY->SetDirection( 1 );
  smootherZ->SetDirection( 2 );

  smootherX->SetNormalizeAcrossScale( false );
  smootherY->SetNormalizeAcrossScale( false );
  smootherZ->SetNormalizeAcrossScale( false );

  try 
    {
    smootherZ->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }

  std::cout << "Image Smoothed" << std::endl;

  InternalImageType::ConstPointer smoothedImage = smootherZ->GetOutput();

  typedef itk::ResampleImageFilter<
                  InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  typedef itk::IdentityTransform< double, Dimension >  TransformType;

  typedef itk::LinearInterpolateImageFunction< 
                                   InternalImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  resampler->SetDefaultPixelValue( 0 ); // value for regions without source


  OutputImageType::SpacingType spacing;

  spacing[0] = inputSpacing[0] * factorX;
  spacing[1] = inputSpacing[1] * factorY;
  spacing[2] = inputSpacing[2] * factorZ;

  resampler->SetOutputSpacing( spacing );

  // Use the same origin
  resampler->SetOutputOrigin( inputImage->GetOrigin() );


  InputImageType::SizeType   inputSize = 
              inputImage->GetLargestPossibleRegion().GetSize();
  typedef InputImageType::SizeType::SizeValueType SizeValueType;
  InputImageType::SizeType   size;

  size[0] = static_cast< SizeValueType >( inputSize[0] / factorX );
  size[1] = static_cast< SizeValueType >( inputSize[1] / factorY );
  size[2] = static_cast< SizeValueType >( inputSize[2] / factorZ );

  resampler->SetSize( size );

  resampler->SetInput( smoothedImage );

  writer->SetInput( resampler->GetOutput() );

  TransformType::Pointer transform = TransformType::New();

  transform->SetIdentity();

  resampler->SetTransform( transform );

  
  try 
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }

  std::cout << "Resampling Done !" << std::endl;

// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}

