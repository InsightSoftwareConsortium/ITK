/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    BSplineWarping1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


//  Software Guide : BeginLatex
//
//  This example illustrates how to deform an image using a BSplineTransform.
// 
//  \index{BSplineDeformableTransform}
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h" 
#include "itkImageFileWriter.h" 

#include "itkImage.h"
#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"

#include "itkBSplineDeformableTransform.h"

#include <fstream>


int main( int argc, char * argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coefficientsFile fixedImage ";
    std::cerr << "movingImage deformedMovingImage" << std::endl;
    return 1;
    }

  const     unsigned int   ImageDimension = 2;

  typedef   unsigned char  PixelType;
  typedef   itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef   itk::Image< PixelType, ImageDimension >  MovingImageType;

  typedef   itk::ImageFileReader< FixedImageType  >  FixedReaderType;
  typedef   itk::ImageFileReader< MovingImageType >  MovingReaderType;

  typedef   itk::ImageFileWriter< MovingImageType >  MovingWriterType;


  FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[2] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  MovingReaderType::Pointer movingReader = MovingReaderType::New();
  MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName( argv[3] );
  movingWriter->SetFileName( argv[4] );


  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();


  typedef itk::ResampleImageFilter< MovingImageType, 
                                    FixedImageType  >  FilterType;

  FilterType::Pointer resampler = FilterType::New();

  typedef itk::LinearInterpolateImageFunction< 
                       MovingImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );


  resampler->SetOutputSpacing( fixedImage->GetSpacing() );
  resampler->SetOutputOrigin(  fixedImage->GetOrigin() );

  
  FixedImageType::RegionType region = fixedImage->GetBufferedRegion();
  resampler->SetSize(  region.GetSize() );
  resampler->SetOutputStartIndex(  region.GetIndex() );


  resampler->SetInput( movingReader->GetOutput() );
  
  movingWriter->SetInput( resampler->GetOutput() );

  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineDeformableTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     TransformType;
   

  
  TransformType::Pointer bsplineTransform = TransformType::New();

  resampler->SetTransform( bsplineTransform );


  /**
   * Define the deformable grid region, spacing and origin
   */
  typedef TransformType::RegionType RegionType;
  RegionType bsplineRegion;
  RegionType::SizeType   size;

  size.Fill( 10 );
  bsplineRegion.SetSize( size );

  typedef TransformType::SpacingType SpacingType;
  SpacingType spacing;
  spacing.Fill( 32.0 );

  typedef TransformType::OriginType OriginType;
  OriginType origin;
  origin.Fill( -32.0 );


  bsplineTransform->SetGridSpacing( spacing );
  bsplineTransform->SetGridOrigin( origin );
  bsplineTransform->SetGridRegion( bsplineRegion );
  

  typedef TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters =
               bsplineTransform->GetNumberOfParameters();
  

  const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

  ParametersType parameters( numberOfParameters );


  //  Read the coefficients from an input file
  std::ifstream infile;

  infile.open( argv[1] );

  for( unsigned int n=0; n < numberOfNodes; n++ )
    {
    infile >>  parameters[n]; 
    infile >>  parameters[n+numberOfNodes]; 
    } 

  infile.close();


  bsplineTransform->SetParameters( parameters );

  bsplineTransform->Print( std::cout );

  try
    {
    movingWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

//  Software Guide : EndLatex

}

