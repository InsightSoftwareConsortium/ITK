/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleVolumesToBeIsotropic.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkIntensityWindowingImageFilter.h"


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  lower upper " << std::endl; 
    return 1;
    }

  const     unsigned int    Dimension = 3;

  typedef   unsigned short  InputPixelType;
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

  const double * inputSpacing = inputImage->GetSpacing();

  const double isoSpacing = sqrt( inputSpacing[2] * inputSpacing[0] );

  typedef itk::IntensityWindowingImageFilter< 
                                  InputImageType, 
                                  InternalImageType >  IntensityFilterType;

  IntensityFilterType::Pointer intensityWindowing = IntensityFilterType::New();

  intensityWindowing->SetWindowMinimum( atoi( argv[3] ) );
  intensityWindowing->SetWindowMaximum( atoi( argv[4] ) );

  intensityWindowing->SetOutputMinimum(   0.0 );
  intensityWindowing->SetOutputMaximum( 255.0 ); // floats but in the range of chars.


  typedef itk::RecursiveGaussianImageFilter< 
                                  InternalImageType,
                                  InternalImageType > GaussianFilterType;

  GaussianFilterType::Pointer smootherX = GaussianFilterType::New();
  GaussianFilterType::Pointer smootherY = GaussianFilterType::New();

  intensityWindowing->SetInput( reader->GetOutput() );
  smootherX->SetInput( intensityWindowing->GetOutput() );
  smootherY->SetInput( smootherX->GetOutput() );

  smootherX->SetSigma( isoSpacing );
  smootherY->SetSigma( isoSpacing );

  smootherX->SetDirection( 0 );
  smootherY->SetDirection( 1 );

  smootherX->SetNormalizeAcrossScale( true );
  smootherY->SetNormalizeAcrossScale( true );

  try 
    {
    smootherY->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  InternalImageType::ConstPointer smoothedImage = smootherY->GetOutput();

  typedef itk::ResampleImageFilter<
                  InternalImageType, OutputImageType >  ResampleFilterType;

  ResampleFilterType::Pointer resampler = ResampleFilterType::New();

  typedef itk::IdentityTransform< double, Dimension >  TransformType;

  typedef itk::LinearInterpolateImageFunction< 
                                   InternalImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  resampler->SetDefaultPixelValue( 1000 ); // highlight regions without source


  double spacing[ Dimension ];

  spacing[0] = isoSpacing;
  spacing[1] = isoSpacing;
  spacing[2] = isoSpacing;

  resampler->SetOutputSpacing( spacing );

  // Use the same origin
  resampler->SetOutputOrigin( inputImage->GetOrigin() );


  InputImageType::SizeType   inputSize = inputImage->GetLargestPossibleRegion().GetSize();
  InputImageType::SizeType   size;

  size[0] =   inputSize[0]       * inputSpacing[0] / isoSpacing;
  size[1] =   inputSize[1]       * inputSpacing[1] / isoSpacing;
  size[2] = ( inputSize[2] - 1 ) * inputSpacing[2] / isoSpacing;

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


  return 0;
}

