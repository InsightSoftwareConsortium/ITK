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

#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.h"
#include "itkPCAShapeSignedDistanceFunction.h"
#include "itkAmoebaOptimizer.h"

#include "itkSphereSignedDistanceFunction.h"
#include "itkCastImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"

#include "itkFastMarchingImageFilter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"
#include "itkTestingMacros.h"

/** \class ShowIterationObject
 * This class is used to support callbacks
 * on the segmentation filter in this test.
 */
namespace {
template<typename TFilter>
class ShowIterationObject
{
public:
  ShowIterationObject( TFilter * filter )
    { m_Filter = filter; }
  void ShowIteration()
    {
    std::cout << m_Filter->GetElapsedIterations() << ": ";
    std::cout << m_Filter->GetCurrentParameters() << " ";
    std::cout << m_Filter->GetRMSChange() << std::endl;
    }

  typename TFilter::Pointer m_Filter;
};
}

int itkGeodesicActiveContourShapePriorLevelSetImageFilterTest_2( int, char *[] )
{
  // Typedefs of components
  const unsigned int    ImageDimension = 2;
  typedef unsigned char PixelType;
  typedef float         InternalPixelType;

  typedef itk::Image<PixelType, ImageDimension>         ImageType;
  typedef itk::Image<InternalPixelType, ImageDimension> InternalImageType;

  typedef itk::GeodesicActiveContourShapePriorLevelSetImageFilter<InternalImageType, InternalImageType>
                                     FilterType;
  typedef itk::PCAShapeSignedDistanceFunction<double, ImageDimension>
                                     ShapeFunctionType;
  typedef itk::ShapePriorMAPCostFunction<InternalImageType, InternalPixelType>
                                     CostFunctionType;
  typedef itk::AmoebaOptimizer       OptimizerType;
  typedef FilterType::ParametersType ParametersType;


  typedef itk::SphereSignedDistanceFunction<double, ImageDimension> SphereFunctionType;

  FilterType::Pointer filter             = FilterType::New();
  ShapeFunctionType::Pointer shape       = ShapeFunctionType::New();
  CostFunctionType::Pointer costFunction = CostFunctionType::New();
  OptimizerType::Pointer optimizer       = OptimizerType::New();

  SphereFunctionType::Pointer sphere     = SphereFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, GeodesicActiveContourShapePriorLevelSetImageFilter,
    ShapePriorSegmentationLevelSetImageFilter );

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize( imageSize );

  //
  // Create an input image.
  // A light circle with a rectangle through it on a dark background.
  // The circle is centered at {50,57} with radius 30.
  // The rectangle starts at {10,50} with size {80,10}.
  //
  // The true shape is just the circle.
  //

  PixelType background = 0;
  PixelType foreground = 190;

  // Fill in the background
  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions( imageRegion );
  inputImage->Allocate();
  inputImage->FillBuffer( background );

  ImageType::Pointer trueShape = ImageType::New();
  trueShape->SetRegions( imageRegion );
  trueShape->Allocate();
  trueShape->FillBuffer( background );

  // Draw in the rectangle
  ImageType::IndexType rectStart;
  rectStart[0] = 10;
  rectStart[1] = 50;
  ImageType::SizeType rectSize;
  rectSize[0] = 80;
  rectSize[1] = 10;
  ImageType::RegionType rectRegion;
  rectRegion.SetIndex( rectStart );
  rectRegion.SetSize( rectSize );

  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator it( inputImage, rectRegion );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( foreground );
    ++it;
    }

  // Draw in the circle
  sphere->Initialize();
  ParametersType trueParameters( sphere->GetNumberOfParameters() );
  trueParameters[0] = 30.0;
  trueParameters[1] = 50.0;
  trueParameters[2] = 57.0;
  sphere->SetParameters( trueParameters );

  it = Iterator( inputImage, imageRegion );
  it.GoToBegin();

  Iterator it2( trueShape, imageRegion );
  it2.GoToBegin();

  while( !it.IsAtEnd() )
  {
  ImageType::IndexType index = it.GetIndex();
  SphereFunctionType::PointType point;
  inputImage->TransformIndexToPhysicalPoint( index, point );
  if( sphere->Evaluate( point ) <= 0.0 )
    {
    it.Set( foreground );
    it2.Set( foreground );
    }
  ++it;
  ++it2;
  }

  //
  // Create an edge potential map.
  // First compute the image gradient magnitude using a derivative of gaussian filter.
  // Then apply a sigmoid function to the gradient magnitude.
  //
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

  //
  // Create an initial level.
  // Use fast marching to create an signed distance from a seed point.
  //
  typedef itk::FastMarchingImageFilter<InternalImageType> FastMarchingFilterType;
  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  typedef FastMarchingFilterType::NodeContainer NodeContainer;
  typedef FastMarchingFilterType::NodeType      NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();

  // Choose an initial contour that is within the shape to be segmented
  // The initial contour is a circle centered at {47,47} with radius 10.0
  InternalImageType::IndexType seedPosition;
  seedPosition[0] = 47;
  seedPosition[1] = 47;

  NodeType node;
  node.SetValue( -10.0 );
  node.SetIndex( seedPosition );

  seeds->Initialize();
  seeds->InsertElement( 0, node );

  fastMarching->SetTrialPoints( seeds );
  fastMarching->SetSpeedConstant( 1.0 );
  fastMarching->SetOutputSize( imageSize );


  //
  // Set up the components of the shape prior segmentation filter.
  //

  // Set up the shape function.
  //
  // Use the sphere function to create the mean image with center
  // at the center of the image and radius of 0.
  //
  typedef ShapeFunctionType::ImageType ComponentImageType;
  ComponentImageType::Pointer meanImage = ComponentImageType::New();
  meanImage->SetRegions( imageRegion );
  meanImage->Allocate();

  trueParameters[0] = 10.0;
  trueParameters[1] = 64.0;
  trueParameters[2] = 64.0;

  sphere->SetParameters( trueParameters );

  typedef itk::ImageRegionIterator<ComponentImageType> ComponentIterator;
  ComponentIterator citer( meanImage, imageRegion );
  citer.GoToBegin();

  while( !citer.IsAtEnd() )
    {
    ComponentImageType::IndexType index = citer.GetIndex();
    SphereFunctionType::PointType point;
    meanImage->TransformIndexToPhysicalPoint( index, point );

    citer.Set( sphere->Evaluate( point ) );

    ++citer;
    }

  //
  // There is 1 PCA component image(s).
  //
  // Component[0] is a image of all ones representing scale.
  //
  typedef ShapeFunctionType::ImagePointerVector ImageVectorType;
  ImageVectorType pca;

  unsigned int numberOfPCA = 1;
  pca.resize( numberOfPCA );

  pca[0] = ComponentImageType::New();
  pca[0]->SetRegions( imageRegion );
  pca[0]->Allocate();
  pca[0]->FillBuffer( 1.0 );


  //
  // Set up a translation transform
  //
  typedef itk::TranslationTransform<double,ImageDimension> TransformType;
  TransformType::Pointer transform = TransformType::New();

  //
  // Set up the standard deviations
  // TODO: this parameter is not in Leventon's paper
  TransformType::ParametersType pcaStdDev( numberOfPCA );
  pcaStdDev.Fill( 1.0 );

  shape->SetNumberOfPrincipalComponents( numberOfPCA );
  shape->SetMeanImage( meanImage );
  shape->SetPrincipalComponentImages( pca );
  shape->SetTransform( transform );
  shape->SetPrincipalComponentStandardDeviations( pcaStdDev );
  shape->Initialize();

  // Set up the cost function
  CostFunctionType::ArrayType mean( shape->GetNumberOfShapeParameters() );
  CostFunctionType::ArrayType stddev( shape->GetNumberOfShapeParameters() );

  // Assume the pca component has a mean value of -15.0 and std dev of 3
  mean[0]   = -15.0;
  stddev[0] = 3.0;

  costFunction->SetShapeParameterMeans( mean );
  costFunction->SetShapeParameterStandardDeviations( stddev );

  CostFunctionType::WeightsType weights;
  weights.Fill( 1.0 );
  weights[1] = 10.0;
  costFunction->SetWeights( weights );

  // Set up the optimizer
  optimizer->SetFunctionConvergenceTolerance( 0.1 );
  optimizer->SetParametersConvergenceTolerance( 0.5 );
  optimizer->SetMaximumNumberOfIterations( 50 );


  // Set up the initial parameters
  ParametersType parameters( shape->GetNumberOfParameters() );

  parameters[0] = mean[0]; // mean pca value
  parameters[1] = 1.0; // center of model already located at center of image
  parameters[2] = 1.0; // center of model already located at center of image

  // Set up the scaling between the level set terms
  filter->SetPropagationScaling( 0.5 );
  filter->SetAdvectionScaling( 1.00 );
  filter->SetCurvatureScaling( 1.00 );
  filter->SetShapePriorScaling( 0.1 );

  // Hook up components to the filter
  filter->SetInput( fastMarching->GetOutput() );  // initial level set
  filter->SetFeatureImage( sigmoid->GetOutput() );  // edge potential map
  filter->SetShapeFunction( shape );
  filter->SetCostFunction( costFunction );
  filter->SetOptimizer( optimizer );
  filter->SetInitialParameters( parameters );

  filter->SetNumberOfLayers( 4 );
  filter->SetMaximumRMSError( 0.01 );
  filter->SetNumberOfIterations( 400 );

  //
  // Connect an observer to the filter.
  //
  typedef ShowIterationObject<FilterType> WatcherType;
  WatcherType iterationWatcher(filter);
  itk::SimpleMemberCommand<WatcherType>::Pointer command =
    itk::SimpleMemberCommand<WatcherType>::New();
  command->SetCallbackFunction( &iterationWatcher,
                                &WatcherType::ShowIteration );
  filter->AddObserver( itk::IterationEvent(), command );

  //
  // Threshold the output level set to display the final contour.
  //
  typedef itk::BinaryThresholdImageFilter< InternalImageType, ImageType >
    ThresholdFilterType;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput( filter->GetOutput() );
  thresholder->SetLowerThreshold( -1e+10 );
  thresholder->SetUpperThreshold( 0.0 );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 255 );
  thresholder->Update();

  //
  // Compute overlap between the true shape and the segmented shape.
  //
  typedef itk::SimilarityIndexImageFilter< ImageType, ImageType >
    OverlapCalculatorType;
  OverlapCalculatorType::Pointer overlap = OverlapCalculatorType::New();

  overlap->SetInput1( trueShape );
  overlap->SetInput2( thresholder->GetOutput() );

  // Run the pipeline
  try
    {
    overlap->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    std::cout << "Caught unexpected exception." << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // Print useful information from the shape detection filter
  std::cout << "Max. no. iterations: " << filter->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << filter->GetMaximumRMSError() << std::endl;
  std::cout << "No. elpased iterations: " << filter->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << filter->GetRMSChange() << std::endl;
  std::cout << "Overlap: " << overlap->GetSimilarityIndex() << std::endl;

  // Uncomment to write out images
/*
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  typedef itk::RescaleIntensityImageFilter< InternalImageType,
    ImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  typedef itk::CastImageFilter< ComponentImageType,
    InternalImageType > ComponentCasterType;
  ComponentCasterType::Pointer ccaster = ComponentCasterType::New();

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

  ccaster->SetInput( meanImage );
  rescaler->SetInput( ccaster->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( "mean.png" );
  writer->Update();
*/

  // Check if overlap is above threshold
  if ( overlap->GetSimilarityIndex() > 0.93 )
    {
    std::cout << "Overlap exceed threshold." << std::endl;
    }
  else
    {
    std::cout << "Overlap below threshold." << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
