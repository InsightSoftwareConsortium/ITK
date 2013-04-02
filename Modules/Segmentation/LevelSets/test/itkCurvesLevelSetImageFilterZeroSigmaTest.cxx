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

#include "itkCurvesLevelSetImageFilter.h"

#include "itkCastImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"

/* Uncomment to write out image files */
/*
*/

int itkCurvesLevelSetImageFilterZeroSigmaTest(int, char* [] )
{

  const   unsigned int    ImageDimension = 2;
  typedef unsigned char   PixelType;
  typedef float           InternalPixelType;

  typedef itk::Image<PixelType,ImageDimension>         ImageType;
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

  /**
   * Set up and run the shape detection filter
   */
  typedef itk::CurvesLevelSetImageFilter<
    InternalImageType, InternalImageType > CurvesFilterType;

  CurvesFilterType::Pointer curvesFilter = CurvesFilterType::New();

  // set the initial level set
  curvesFilter->SetInput( fastMarching->GetOutput() );

  // set the edge potential image
  curvesFilter->SetFeatureImage( sigmoid->GetOutput() );

  // set the weights between the propagation, curvature and advection terms
  curvesFilter->SetPropagationScaling( 1.0 );
  curvesFilter->SetCurvatureScaling( 0.1 );
  curvesFilter->SetAdvectionScaling( 0.5 );

  // use finite differences instead of derivative of Gaussian to build advection
  curvesFilter->SetDerivativeSigma( 0.0 );

  // set the convergence criteria
  curvesFilter->SetMaximumRMSError( 0.03 );
  curvesFilter->SetNumberOfIterations( 200 );

  /**
   * Threshold the output level set to display the final contour.
   */
  typedef itk::BinaryThresholdImageFilter< InternalImageType, ImageType >
    ThresholdFilterType;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput( curvesFilter->GetOutput() );
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
  std::cout << "Max. no. iterations: " << curvesFilter->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << curvesFilter->GetMaximumRMSError() << std::endl;
  std::cout << "No. elpased iterations: " << curvesFilter->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << curvesFilter->GetRMSChange() << std::endl;
  std::cout << "Overlap: " << overlap->GetSimilarityIndex() << std::endl;

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
  curvesFilter->SetPropagationScaling( 0.0 );
  curvesFilter->SetCurvatureScaling( 1.0 );
  curvesFilter->SetAdvectionScaling( 0.0 );
  curvesFilter->Update();

  std::cout << "Test Passed. " << std::endl;
  return EXIT_SUCCESS;

}
