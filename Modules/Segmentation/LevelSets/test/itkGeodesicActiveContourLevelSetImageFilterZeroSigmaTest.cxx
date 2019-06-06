/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkGeodesicActiveContourLevelSetImageFilter.h"

#include "itkCastImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"
#include "itkTestingMacros.h"


int itkGeodesicActiveContourLevelSetImageFilterZeroSigmaTest( int, char* [] )
{

  constexpr unsigned int ImageDimension = 2;
  using PixelType = unsigned char;
  using InternalPixelType = float;

  using ImageType = itk::Image<PixelType, ImageDimension>;
  using InternalImageType = itk::Image<InternalPixelType, ImageDimension>;

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize( imageSize );

  //
  // Create an input image: a light square on a dark background.
  //
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

  using Iterator = itk::ImageRegionIterator<ImageType>;
  Iterator it( inputImage, squareRegion );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( foreground );
    ++it;
    }

  //
  // Create an edge potential map.
  // First compute the image gradient magnitude using a derivative of gaussian filter.
  // Then apply a sigmoid function to the gradient magnitude.
  //
  using CastFilterType = itk::CastImageFilter< ImageType, InternalImageType >;
  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput( inputImage );

  using GradientImageType = itk::GradientMagnitudeRecursiveGaussianImageFilter< InternalImageType,
    InternalImageType >;

  GradientImageType::Pointer gradMagnitude = GradientImageType::New();
  gradMagnitude->SetInput( caster->GetOutput() );
  gradMagnitude->SetSigma( 1.0 );

  using SigmoidFilterType =
      itk::SigmoidImageFilter< InternalImageType, InternalImageType >;
  SigmoidFilterType::Pointer sigmoid = SigmoidFilterType::New();
  sigmoid->SetOutputMinimum( 0.0 );
  sigmoid->SetOutputMaximum( 1.0 );
  sigmoid->SetAlpha( -0.4 );
  sigmoid->SetBeta( 2.5 );
  sigmoid->SetInput( gradMagnitude->GetOutput() );

  //
  // Create an initial level.
  // Use fast marching to create an signed distance from a seed point.
  //
  using FastMarchingFilterType = itk::FastMarchingImageFilter<InternalImageType>;
  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  using NodeContainer = FastMarchingFilterType::NodeContainer;
  using NodeType = FastMarchingFilterType::NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();

  // Choose an initial contour that overlaps the square to be segmented.
  InternalImageType::IndexType seedPosition;
  seedPosition[0] = 47;
  seedPosition[1] = 47;

  NodeType node;
  node.SetValue( -29.5 );
  node.SetIndex( seedPosition );

  seeds->Initialize();
  seeds->InsertElement( 0, node );

  fastMarching->SetTrialPoints( seeds );
  fastMarching->SetSpeedConstant( 1.0 );
  fastMarching->SetOutputSize( imageSize );

  //
  // Set up and run the shape detection filter.
  //
  using ShapeDetectionFilterType = itk::GeodesicActiveContourLevelSetImageFilter<
    InternalImageType, InternalImageType >;

  ShapeDetectionFilterType::Pointer shapeDetection = ShapeDetectionFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( shapeDetection, GeodesicActiveContourLevelSetImageFilter,
    SegmentationLevelSetImageFilter );

  // set the initial level set
  shapeDetection->SetInput( fastMarching->GetOutput() );

  // Set the edge potential image
  shapeDetection->SetFeatureImage( sigmoid->GetOutput() );

  // Set the weights between the propagation, curvature and advection terms
  shapeDetection->SetPropagationScaling( 1.0 );
  shapeDetection->SetCurvatureScaling( 0.1 );
  shapeDetection->SetAdvectionScaling( 0.5 );

  // Use finite differences instead of derivative of Gaussian to build advection
  shapeDetection->SetDerivativeSigma( 0.0 );

  // Set the convergence criteria
  shapeDetection->SetMaximumRMSError( 0.03 );
  shapeDetection->SetNumberOfIterations( 200 );

  //
  // Threshold the output level set to display the final contour.
  //
  using ThresholdFilterType =
      itk::BinaryThresholdImageFilter< InternalImageType, ImageType >;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput( shapeDetection->GetOutput() );
  thresholder->SetLowerThreshold( -1e+10 );
  thresholder->SetUpperThreshold( 0.0 );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 255 );

  //
  // Compute overlap between the true shape and the segmented shape.
  //
  using OverlapCalculatorType =
      itk::SimilarityIndexImageFilter< ImageType, ImageType >;
  OverlapCalculatorType::Pointer overlap = OverlapCalculatorType::New();

  overlap->SetInput1( inputImage );
  overlap->SetInput2( thresholder->GetOutput() );
  overlap->Update();

  // Print useful information from the shape detection filter
  std::cout << "Max. no. iterations: " << shapeDetection->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << shapeDetection->GetMaximumRMSError() << std::endl;
  std::cout << "No. elpased iterations: " << shapeDetection->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << shapeDetection->GetRMSChange() << std::endl;
  std::cout << "Overlap: " << overlap->GetSimilarityIndex() << std::endl;


  // Uncomment to write out image files
/*
  using WriterType = itk::ImageFileWriter< ImageType >;
  WriterType::Pointer writer = WriterType::New();

  using RescaleFilterType = itk::RescaleIntensityImageFilter< InternalImageType,
    ImageType >;
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

  writer->SetInput( thresholder->GetOutput() );
  writer->SetFileName( "outputLevelSet.png" );
  writer->Update();

  thresholder->SetInput( fastMarching->GetOutput() );
  writer->SetInput( thresholder->GetOutput() );
  writer->SetFileName( "initialLevelSet.png" );
  writer->Update();
*/

  // Check if overlap is above threshold
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

  // Test case when PropagationScaling is zero
  shapeDetection->SetPropagationScaling( 0.0 );
  shapeDetection->SetCurvatureScaling( 1.0 );
  shapeDetection->SetAdvectionScaling( 0.0 );
  shapeDetection->Update();

  std::cout << "Test Passed." << std::endl;
  return EXIT_SUCCESS;

}
