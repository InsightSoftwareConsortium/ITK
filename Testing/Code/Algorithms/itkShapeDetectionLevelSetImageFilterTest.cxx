/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkShapeDetectionLevelSetImageFilter.h"

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkCastImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"

/* Uncomment to write out image files */
/*
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"
*/

int itkShapeDetectionLevelSetImageFilterTest(int, char* [] )
{

  const   unsigned int    ImageDimension = 2;
  typedef unsigned char   PixelType;
  typedef float           InternalPixelType;

  {
  /**
   *  Test out some vnl functions
   */
  InternalPixelType value = 12.3456;
  InternalPixelType value2 = -9.8765;
  InternalPixelType results;
  results = vcl_sqrt( value );
  std::cout << "vcl_sqrt( " << value << " ) = " << results << std::endl;
  results = vnl_math_abs( value2 );
  std::cout << "vnl_math_abs( " << value2 << " ) = " << results << std::endl;
  results = vnl_math_min( value, value2 );
  std::cout << "vnl_math_min( " << value << ", " << value2 
    << " ) = " << results << std::endl;
  results = vnl_math_max( value, value2 );
  std::cout << "vnl_math_max( " << value << ", " << value2 
    << " ) = " << results << std::endl;
  }

  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<InternalPixelType,ImageDimension> InternalImageType;

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize( imageSize );

  /**
   * Create an input image.
   * A light square on a dark background.
   */
  PixelType background = 0;
  PixelType foreground = 190;

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions( imageRegion );
  inputImage->Allocate();
  inputImage->FillBuffer( background );

  ImageType::IndexType squareStart;
  squareStart.Fill( 20 );
  ImageType::SizeType squareSize;
  squareSize.Fill( 60 );
  ImageType::RegionType squareRegion;
  squareRegion.SetIndex( squareStart );
  squareRegion.SetSize( squareSize );

  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator it( inputImage, squareRegion );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( foreground );
    ++it;
    }

  /**
   * Create an edge potential map.
   * First compute the image gradient magnitude using a derivative of gaussian filter.
   * Then apply a sigmoid function to the gradient magnitude.
   */
  typedef itk::CastImageFilter< ImageType, InternalImageType > CastFilterType;
  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput( inputImage );

  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter< InternalImageType,
    InternalImageType > GradientImageType;

  GradientImageType::Pointer gradMagnitude = GradientImageType::New();
  gradMagnitude->SetInput( caster->GetOutput() );
  gradMagnitude->SetSigma( 1.0 );

  typedef itk::SigmoidImageFilter< InternalImageType, InternalImageType >
    SigmoidFilterType;
  SigmoidFilterType::Pointer sigmoid = SigmoidFilterType::New();
  sigmoid->SetOutputMinimum( 0.0 );
  sigmoid->SetOutputMaximum( 1.0 );
  sigmoid->SetAlpha( -0.4 );
  sigmoid->SetBeta( 2.5 );
  sigmoid->SetInput( gradMagnitude->GetOutput() );

  /**
   * Create an initial level.
   * Use fast marching to create an signed distance from a seed point.
   */
  typedef itk::FastMarchingImageFilter<InternalImageType> FastMarchingFilterType;
  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  typedef FastMarchingFilterType::NodeContainer NodeContainer;
  typedef FastMarchingFilterType::NodeType      NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();

  // Choose an initial contour that is wholly within the square to be segmented.
  InternalImageType::IndexType seedPosition;
  seedPosition[0] = 47;
  seedPosition[1] = 47;

  NodeType node;
  node.SetValue( -5.0 );
  node.SetIndex( seedPosition );

  seeds->Initialize();
  seeds->InsertElement( 0, node );

  fastMarching->SetTrialPoints( seeds );
  fastMarching->SetSpeedConstant( 1.0 );
  fastMarching->SetOutputSize( imageSize );

  /**
   * Set up and run the shape detection filter
   */
  typedef itk::ShapeDetectionLevelSetImageFilter<
    InternalImageType, InternalImageType > ShapeDetectionFilterType;
  
  ShapeDetectionFilterType::Pointer shapeDetection = ShapeDetectionFilterType::New();
  
  // set the initial level set
  shapeDetection->SetInput( fastMarching->GetOutput() );

  // set the edge potential image
  shapeDetection->SetFeatureImage( sigmoid->GetOutput() );

  // set the weights between the propagation and curvature terms
  shapeDetection->SetPropagationScaling( 1.0 );
  shapeDetection->SetCurvatureScaling( 0.1 );

  // set the convergence criteria
  shapeDetection->SetMaximumRMSError( 0.02 );
  shapeDetection->SetMaximumIterations( 200 );

  /**
   * Threshold the output level set to display the final contour.
   */
  typedef itk::BinaryThresholdImageFilter< InternalImageType, ImageType >
    ThresholdFilterType;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput( shapeDetection->GetOutput() );
  thresholder->SetLowerThreshold( -1e+10 );
  thresholder->SetUpperThreshold( 0.0 );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 255 );

  /**
   * Compute overlap between the true shape and the segmented shape
   */
  typedef itk::SimilarityIndexImageFilter< ImageType, ImageType >
    OverlapCalculatorType;
  OverlapCalculatorType::Pointer overlap = OverlapCalculatorType::New();

  overlap->SetInput1( inputImage );
  overlap->SetInput2( thresholder->GetOutput() );
  overlap->Update();
  
  /** Printout useful information from the shape detection filter. */
  std::cout << "Max. no. iterations: " << shapeDetection->GetMaximumIterations() << std::endl;
  std::cout << "Max. RMS error: " << shapeDetection->GetMaximumRMSError() << std::endl;
  std::cout << "No. elpased iterations: " << shapeDetection->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << shapeDetection->GetRMSChange() << std::endl;
  std::cout << "Overlap: " << overlap->GetSimilarityIndex() << std::endl;

  if ( overlap->GetSimilarityIndex() > 0.90 )
    {
    std::cout << "Overlap exceed threshold." << std::endl;
    }
  else
    {
    std::cout << "Overlap below threshold." << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * Uncomment to write out image files.
   */
  /*
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  typedef itk::RescaleIntensityImageFilter< InternalImageType,
    ImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  writer->SetFileName( "inputImage.png" );
  writer->SetInput( inputImage );
  writer->Update();

  rescaler->SetInput( gradMagnitude->GetOutput() );
  rescaler->SetOutputMinimum( 0 );
  rescaler->SetOutputMaximum( 255 );
  writer->SetFileName( "gradMagnitude.png" );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  rescaler->SetInput( sigmoid->GetOutput() );
  writer->SetFileName( "edgePotential.png" );
  writer->Update();

  rescaler->SetInput( fastMarching->GetOutput() );
  writer->SetFileName( "initialLevelSet.png" );
  writer->Update();

  writer->SetInput( thresholder->GetOutput() );
  writer->SetFileName( "outputLevelSet.png" );
  writer->Update();
  */

  std::cout << "Test Passed. " << std::endl;
  return EXIT_SUCCESS;

}
