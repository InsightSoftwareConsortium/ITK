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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

//  Software Guide : BeginCommandLineArgs
//    INPUTS: {Circle.png}
//    OUTPUTS: {CircleOutput.mha}
//    49 49 10
//  Software Guide : EndCommandLineArgs

// The use of the \doxygen{ScalarChanAndVeseSparseLevelSetImageFilter} is
// illustrated in the following example. The implementation of this filter in
// ITK is based on the paper by Chan And Vese.  This
// implementation extends the functionality of the
// level-set filters in ITK by using region-based variational techniques. These methods
// do not rely on the presence of edges in the images.
//
// ScalarChanAndVeseSparseLevelSetImageFilter expects two inputs.  The first is
// an initial level set in the form of an \doxygen{Image}. The second input
// is a feature image. For this algorithm, the feature image is the original
// raw or preprocessed image. Several parameters are required by the algorithm
// for regularization and weights of different energy terms. The user is encouraged to
// change different parameter settings to optimize the code example on their images.
//
// Let's start by including the headers of the main filters involved in the
// preprocessing.
//
// Software Guide : EndLatex

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"
#include "itkScalarChanAndVeseLevelSetFunctionData.h"
#include "itkConstrainedRegionBasedLevelSetFunctionSharedData.h"
#include "itkFastMarchingImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"


int main(int argc, char**argv)
{

  if( argc < 6 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " featureImage outputImage";
    std::cerr << " startx starty seedValue" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int nb_iteration = 500;
  double rms = 0.;
  double epsilon = 1.;
  double curvature_weight = 0.;
  double area_weight = 0.;
  double reinitialization_weight = 0.;
  double volume_weight = 0.;
  double volume = 0.;
  double l1 = 1.;
  double l2 = 1.;

  //
  //  We now define the image type using a particular pixel type and
  //  dimension. In this case the \code{float} type is used for the pixels
  //  due to the requirements of the smoothing filter.
  //
  const unsigned int Dimension = 2;
  typedef float ScalarPixelType;
  typedef itk::Image< ScalarPixelType, Dimension > InternalImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< InternalImageType,
    InternalImageType > DataHelperType;

  typedef itk::ConstrainedRegionBasedLevelSetFunctionSharedData<
    InternalImageType, InternalImageType, DataHelperType > SharedDataHelperType;

  typedef itk::ScalarChanAndVeseLevelSetFunction< InternalImageType,
    InternalImageType, SharedDataHelperType > LevelSetFunctionType;


  //  We declare now the type of the numerically discretized Step and Delta functions that
  //  will be used in the level-set computations for foreground and background regions
  //
  typedef itk::AtanRegularizedHeavisideStepFunction< ScalarPixelType,
    ScalarPixelType >  DomainFunctionType;

  DomainFunctionType::Pointer domainFunction = DomainFunctionType::New();
  domainFunction->SetEpsilon( epsilon );

  // We instantiate reader and writer types in the following lines.
  //
  typedef itk::ImageFileReader< InternalImageType > ReaderType;
  typedef itk::ImageFileWriter< InternalImageType > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  reader->Update();

  writer->SetFileName( argv[2] );

  InternalImageType::Pointer featureImage = reader->GetOutput();

  //  We declare now the type of the FastMarchingImageFilter that
  //  will be used to generate the initial level set in the form of a distance
  //  map.
  //
  typedef  itk::FastMarchingImageFilter<
    InternalImageType,
    InternalImageType >    FastMarchingFilterType;

  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();


  //  The FastMarchingImageFilter requires the user to provide a seed
  //  point from which the level set will be generated. The user can actually
  //  pass not only one seed point but a set of them. Note the the
  //  FastMarchingImageFilter is used here only as a helper in the
  //  determination of an initial level set. We could have used the
  //  \doxygen{DanielssonDistanceMapImageFilter} in the same way.
  //
  //  The seeds are passed stored in a container. The type of this
  //  container is defined as \code{NodeContainer} among the
  //  FastMarchingImageFilter traits.
  //
  typedef FastMarchingFilterType::NodeContainer  NodeContainer;
  typedef FastMarchingFilterType::NodeType       NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();

  InternalImageType::IndexType  seedPosition;

  seedPosition[0] = atoi( argv[3] );
  seedPosition[1] = atoi( argv[4] );

  const double initialDistance = atof( argv[5] );

  NodeType node;

  const double seedValue = - initialDistance;

  node.SetValue( seedValue );
  node.SetIndex( seedPosition );

  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.
  //
  seeds->Initialize();
  seeds->InsertElement( 0, node );


  //  The set of seed nodes is passed now to the
  //  FastMarchingImageFilter with the method
  //  \code{SetTrialPoints()}.
  //
  fastMarching->SetTrialPoints(  seeds  );


  //  Since the FastMarchingImageFilter is used here just as a
  //  Distance Map generator. It does not require a speed image as input.
  //  Instead the constant value $1.0$ is passed using the
  //  \code{SetSpeedConstant()} method.
  //
  fastMarching->SetSpeedConstant( 1.0 );

  //  The FastMarchingImageFilter requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //
  fastMarching->SetOutputSize(
    featureImage->GetBufferedRegion().GetSize() );
  fastMarching->Update();

  //  We declare now the type of the ScalarChanAndVeseDenseLevelSetImageFilter that
  //  will be used to generate a segmentation.
  //

  typedef itk::ScalarChanAndVeseDenseLevelSetImageFilter< InternalImageType,
    InternalImageType, InternalImageType, LevelSetFunctionType,
    SharedDataHelperType > MultiLevelSetType;

  MultiLevelSetType::Pointer levelSetFilter = MultiLevelSetType::New();

  //  We set the function count to 1 since a single level-set is being evolved.
  //
  levelSetFilter->SetFunctionCount( 1 );

  //  Set the feature image and initial level-set image as output of the
  //  fast marching image filter.
  //
  levelSetFilter->SetFeatureImage( featureImage );
  levelSetFilter->SetLevelSet( 0, fastMarching->GetOutput() );

  //  Once activiated the level set evolution will stop if the convergence
  //  criteria or if the maximum number of iterations is reached.  The
  //  convergence criteria is defined in terms of the root mean squared (RMS)
  //  change in the level set function. The evolution is said to have
  //  converged if the RMS change is below a user specified threshold.  In a
  //  real application is desirable to couple the evolution of the zero set
  //  to a visualization module allowing the user to follow the evolution of
  //  the zero set. With this feedback, the user may decide when to stop the
  //  algorithm before the zero set leaks through the regions of low gradient
  //  in the contour of the anatomical structure to be segmented.
  //
  levelSetFilter->SetNumberOfIterations( nb_iteration );
  levelSetFilter->SetMaximumRMSError( rms );

  //  Often, in real applications, images have different pixel resolutions. In such
  //  cases, it is best to use the native spacings to compute derivatives etc rather
  //  than sampling the images.
  //
  levelSetFilter->SetUseImageSpacing( 1 );

  //  For large images, we may want to compute the level-set over the initial supplied
  //  level-set image. This saves a lot of memory.
  //
  levelSetFilter->SetInPlace( false );

  //  For the level set with phase 0, set different parameters and weights. This may
  //  to be set in a loop for the case of multiple level-sets evolving simultaneously.
  //
  levelSetFilter->GetDifferenceFunction(0)->SetDomainFunction( domainFunction );
  levelSetFilter->GetDifferenceFunction(0)->SetCurvatureWeight( curvature_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetAreaWeight( area_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetReinitializationSmoothingWeight( reinitialization_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetVolumeMatchingWeight( volume_weight );
  levelSetFilter->GetDifferenceFunction(0)->SetVolume( volume );
  levelSetFilter->GetDifferenceFunction(0)->SetLambda1( l1 );
  levelSetFilter->GetDifferenceFunction(0)->SetLambda2( l2 );

  levelSetFilter->Update();


  writer->SetInput( levelSetFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return -1;
    }

  return EXIT_SUCCESS;
}
