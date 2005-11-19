/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DeformableRegistration6.cxx
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



// Software Guide : BeginLatex
//
// This example illustrates the use of the \doxygen{BSplineDeformableTransform}
// class in a manually controlled multi-resolution scheme. Here we define two
// transforms at two different resolution levels. A first registration is
// performed with the spline grid of low resolution, and the results are then
// used for initializing a higher resolution grid. Since this example is quite
// similar to the previous example on the use of the
// \code{BSplineDeformableTransform} we omit here most of the details already
// discussed and will focus on the aspects related to the multi-resolution
// approach.
//
// \index{itk::BSplineDeformableTransform}
// \index{itk::BSplineDeformableTransform!DeformableRegistration}
// \index{itk::LBFGSOptimizer}
//
//
// Software Guide : EndLatex 

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImage.h"

//  Software Guide : BeginLatex
//  
//  We include the header files for the transform and the optimizer.
//
//  \index{itk::BSplineDeformableTransform!header}
//  \index{itk::LBFGSOptimizer!header}
// 
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBSplineDeformableTransform.h"
#include "itkLBFGSOptimizer.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"

#include "itkBSplineResampleImageFunction.h"
#include "itkIdentityTransform.h"
#include "itkBSplineDecompositionImageFilter.h"

// NOTE: the LBFGSOptimizer does not invoke events


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile  ";
    std::cerr << " [differenceOutputfile] [differenceBeforeRegistration] ";
    std::cerr << " [deformationField] ";
    return 1;
    }
  
  const    unsigned int    ImageDimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, ImageDimension >  FixedImageType;
  typedef itk::Image< PixelType, ImageDimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //
  //  We instantiate now the type of the \code{BSplineDeformableTransform} using
  //  as template parameters the type for coordinates representation, the
  //  dimension of the space, and the order of the BSpline. 
  // 
  //  \index{BSplineDeformableTransform!New}
  //  \index{BSplineDeformableTransform!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineDeformableTransform<
                            CoordinateRepType,
                            SpaceDimension,
                            SplineOrder >     TransformType;
  // Software Guide : EndCodeSnippet


  typedef itk::LBFGSOptimizer       OptimizerType;


  typedef itk::MeanSquaresImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;

  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  

  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );


  //  Software Guide : BeginLatex
  //
  //  We construct two transform objects, each one will be configured for a resolution level.
  //  Notice than in this multi-resolution scheme we are not modifying the
  //  resolution of the image, but rather the flexibility of the deformable
  //  transform itself.
  //
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer  transformLow = TransformType::New();
  registration->SetTransform( transformLow );
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  FixedImageType::ConstPointer fixedImage = fixedImageReader->GetOutput();

  registration->SetFixedImage(  fixedImage   );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update();

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  
 registration->SetFixedImageRegion( fixedRegion );

  //  Software Guide : BeginLatex
  //
  //  Here we define the parameters of the BSplineDeformableTransform grid.  We
  //  arbitrarily decide to use a grid with $5 \times 5$ nodes within the image. 
  //  The reader should note that the BSpline computation requires a
  //  finite support region ( 1 grid node at the lower borders and 2
  //  grid nodes at upper borders). Therefore in this example, we set
  //  the grid size to be $8 \times 8$ and place the grid origin such that
  //  grid node (1,1) coincides with the first pixel in the fixed image.
  // 
  //  \index{BSplineDeformableTransform}
  //
  //  Software Guide : EndLatex 


  typedef TransformType::RegionType RegionType;
  RegionType bsplineRegionLow;
  RegionType::SizeType   gridBorderSize;
  RegionType::SizeType   totalGridSize;

  gridBorderSize.Fill( 3 );    // Border for spline order = 3 ( 1 lower, 2 upper )

   //  Software Guide : BeginLatex
  //
  //  Here we define the parameters of the BSpline transform at low resolution
  // 
  //  \index{BSplineDeformableTransform}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  RegionType::SizeType   gridLowSizeOnImage;
  gridLowSizeOnImage.Fill( 5 );
  totalGridSize = gridLowSizeOnImage + gridBorderSize;

  RegionType bsplineRegion;
  bsplineRegion.SetSize( totalGridSize );

  typedef TransformType::SpacingType SpacingType;
  SpacingType spacingLow = fixedImage->GetSpacing();

  typedef TransformType::OriginType OriginType;
  OriginType originLow = fixedImage->GetOrigin();;

  FixedImageType::SizeType fixedImageSize = fixedRegion.GetSize();

  for(unsigned int r=0; r<ImageDimension; r++)
    {
    spacingLow[r] *= floor( static_cast<double>(fixedImageSize[r] - 1)  / 
                            static_cast<double>(gridLowSizeOnImage[r] - 1) );
    originLow[r]  -=  spacingLow[r]; 
    }

  transformLow->SetGridSpacing( spacingLow );
  transformLow->SetGridOrigin( originLow );
  transformLow->SetGridRegion( bsplineRegion );

  typedef TransformType::ParametersType     ParametersType;

  const unsigned int numberOfParameters =
               transformLow->GetNumberOfParameters();
  
  ParametersType parametersLow( numberOfParameters );

  parametersLow.Fill( 0.0 );

  transformLow->SetParameters( parametersLow );
  //  Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  We now pass the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( transformLow->GetParameters() );


  optimizer->SetGradientConvergenceTolerance( 0.05 );
  optimizer->SetLineSearchAccuracy( 0.9 );
  optimizer->SetDefaultStepLength( 1.5 );
  optimizer->TraceOn();
  optimizer->SetMaximumNumberOfFunctionEvaluations( 1000 );


  std::cout << "Starting Registration with low resolution transform" << std::endl;

  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    } 
  // Software Guide : EndCodeSnippet
  

  //  Software Guide : BeginLatex
  //  
  //  Once the registration has finished with the low resolution grid, we
  //  proceed to instantiate a higher resolution
  //  \code{BSplineDeformableTransform}.
  //  
  //  Software Guide : EndLatex 

  TransformType::Pointer  transformHigh = TransformType::New();

  RegionType::SizeType   gridHighSizeOnImage;
  gridHighSizeOnImage.Fill( 10 );
  totalGridSize = gridHighSizeOnImage + gridBorderSize;

  bsplineRegion.SetSize( totalGridSize );

  SpacingType spacingHigh = fixedImage->GetSpacing();
  OriginType  originHigh  = fixedImage->GetOrigin();;

  for(unsigned int rh=0; rh<ImageDimension; rh++)
    {
    spacingHigh[rh] *= floor( static_cast<double>(fixedImageSize[rh] - 1)  / 
                            static_cast<double>(gridHighSizeOnImage[rh] - 1) );
    originHigh[rh]  -=  spacingHigh[rh]; 
    }

  transformHigh->SetGridSpacing( spacingHigh );
  transformHigh->SetGridOrigin( originHigh );
  transformHigh->SetGridRegion( bsplineRegion );

  ParametersType parametersHigh( transformHigh->GetNumberOfParameters() );
  parametersHigh.Fill( 0.0 );

  //  Software Guide : BeginLatex
  //  
  //  Now we need to initialize the BSpline coefficients of the higher resolution
  //  transform. This is done by first computing the actual deformation field 
  //  at the higher resolution from the lower resolution BSpline coefficients. 
  //  Then a BSpline decomposition is done to obtain the BSpline coefficient of 
  //  the higher resolution transform.
  //  
  //  Software Guide : EndLatex 

  unsigned int counter = 0;

  for ( unsigned int k = 0; k < SpaceDimension; k++ )
    {
    typedef TransformType::ImageType ParametersImageType;
    typedef itk::ResampleImageFilter<ParametersImageType,ParametersImageType> ResamplerType;
    ResamplerType::Pointer upsampler = ResamplerType::New();

    typedef itk::BSplineResampleImageFunction<ParametersImageType,double> FunctionType;
    FunctionType::Pointer function = FunctionType::New();

    typedef itk::IdentityTransform<double,SpaceDimension> IdentityTransformType;
    IdentityTransformType::Pointer identity = IdentityTransformType::New();

    upsampler->SetInput( transformLow->GetCoefficientImage()[k] );
    upsampler->SetInterpolator( function );
    upsampler->SetTransform( identity );
    upsampler->SetSize( transformHigh->GetGridRegion().GetSize() );
    upsampler->SetOutputSpacing( transformHigh->GetGridSpacing() );
    upsampler->SetOutputOrigin( transformHigh->GetGridOrigin() );

    typedef itk::BSplineDecompositionImageFilter<ParametersImageType,ParametersImageType>
      DecompositionType;
    DecompositionType::Pointer decomposition = DecompositionType::New();

    decomposition->SetSplineOrder( SplineOrder );
    decomposition->SetInput( upsampler->GetOutput() );
    decomposition->Update();

    ParametersImageType::Pointer newCoefficients = decomposition->GetOutput();

    // copy the coefficients into the parameter array
    typedef itk::ImageRegionIterator<ParametersImageType> Iterator;
    Iterator it( newCoefficients, transformHigh->GetGridRegion() );
    while ( !it.IsAtEnd() )
      {
      parametersHigh[ counter++ ] = it.Get();
      ++it;
      }

    }
  
  transformHigh->SetParameters( parametersHigh );

  //  Software Guide : BeginLatex
  //  
  //  We now pass the parameters of the high resolution transform as the initial
  //  parameters to be used in a second stage of the registration process.
  //
  //  Software Guide : EndLatex 

  std::cout << "Starting Registration with high resolution transform" << std::endl;

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( transformHigh->GetParameters() );
  registration->SetTransform( transformHigh );

  //  Software Guide : BeginLatex
  //  
  //  Typically, we will also want to tighten the optimizer parameters
  //  when we move from lower to higher resolution grid.
  //
  //  Software Guide : EndLatex 
  optimizer->SetGradientConvergenceTolerance( 0.01 );
  optimizer->SetDefaultStepLength( 0.25 );


  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    } 
  // Software Guide : EndCodeSnippet



  // Finally we use the last transform parameters in order to resample the image.
  //
  transformHigh->SetParameters( registration->GetLastTransformParameters() );

  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( transformHigh );
  resample->SetInput( movingImageReader->GetOutput() );

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetDefaultPixelValue( 100 );
  
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, ImageDimension > OutputImageType;
  
  typedef itk::CastImageFilter< 
                        FixedImageType,
                        OutputImageType > CastFilterType;
                    
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );


  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err ) 
    { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    } 
 


  typedef itk::SquaredDifferenceImageFilter< 
                                  FixedImageType, 
                                  FixedImageType, 
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );  
  

  // Compute the difference image between the 
  // fixed and resampled moving image.
  if( argc >= 5 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );
    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err ) 
      { 
      std::cerr << "ExceptionObject caught !" << std::endl; 
      std::cerr << err << std::endl; 
      return -1;
      } 
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc >= 6 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
    try
      {
      writer2->Update();
      }
    catch( itk::ExceptionObject & err ) 
      { 
      std::cerr << "ExceptionObject caught !" << std::endl; 
      std::cerr << err << std::endl; 
      return -1;
      } 
    }



  // Generate the explicit deformation field resulting from 
  // the registration.

  typedef itk::Vector< float, ImageDimension >  VectorType;
  typedef itk::Image< VectorType, ImageDimension >  DeformationFieldType;

  DeformationFieldType::Pointer field = DeformationFieldType::New();
  field->SetRegions( fixedRegion );
  field->SetOrigin( fixedImage->GetOrigin() );
  field->SetSpacing( fixedImage->GetSpacing() );
  field->Allocate();

  typedef itk::ImageRegionIterator< DeformationFieldType > FieldIterator;
  FieldIterator fi( field, fixedRegion );

  fi.GoToBegin();

  TransformType::InputPointType  fixedPoint;
  TransformType::OutputPointType movingPoint;
  DeformationFieldType::IndexType index;

  VectorType displacement;

  while( ! fi.IsAtEnd() )
    {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint( index, fixedPoint );
    movingPoint = transformHigh->TransformPoint( fixedPoint );
    displacement[0] = movingPoint[0] - fixedPoint[0];
    displacement[1] = movingPoint[1] - fixedPoint[1];
    fi.Set( displacement );
    ++fi;
    }



  typedef itk::ImageFileWriter< DeformationFieldType >  FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( field );

  if( argc >= 7 )
    {
    fieldWriter->SetFileName( argv[6] );
    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }

  return 0;
}

