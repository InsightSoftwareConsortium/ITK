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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput5.png}
//    ARGUMENTS:    81 114 5 1.0  -0.5  3.0   .05 1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput6.png}
//    ARGUMENTS:    99 114 5 1.0  -0.5  3.0   .05 1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput7.png}
//    ARGUMENTS:    56 92 5 1.0  -0.3  2.0   .05 1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput8.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput1.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput2.png}
//    OUTPUTS: {ShapeDetectionLevelSetFilterOutput3.png}
//    ARGUMENTS:    40 90 5 0.5  -0.3  2.0   .05 1
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// The use of the \doxygen{ShapeDetectionLevelSetImageFilter} is illustrated
// in the following example.  The implementation of this filter in ITK is
// based on the paper by Malladi et al \cite{Malladi1995}.  In this
// implementation, the governing differential equation has an additional
// curvature-based term. This term acts as a smoothing term where areas of
// high curvature, assumed to be due to noise, are smoothed out. Scaling
// parameters are used to control the tradeoff between the expansion term and
// the smoothing term.  One consequence of this additional curvature term is
// that the fast marching algorithm is no longer applicable, because the
// contour is no longer guaranteed to always be expanding. Instead, the level
// set function is updated iteratively.
//
// The ShapeDetectionLevelSetImageFilter expects two inputs,
// the first being an initial Level Set in the form of an
// \doxygen{Image}, and the second being a feature image. For this algorithm,
// the feature image is an edge potential image that basically
// follows the same rules applicable to the speed image used for the
// FastMarchingImageFilter discussed in
// Section~\ref{sec:FastMarchingImageFilter}.
//
// In this example we use an FastMarchingImageFilter to produce the initial
// level set as the distance function to a set of user-provided seeds. The
// FastMarchingImageFilter is run with a constant speed value which enables
// us to employ this filter as a distance map calculator.
//
// \begin{figure} \center
// \includegraphics[width=\textwidth]{ShapeDetectionCollaborationDiagram1}
// \itkcaption[ShapeDetectionLevelSetImageFilter collaboration diagram]{Collaboration
// diagram for the ShapeDetectionLevelSetImageFilter applied to a segmentation task.}
// \label{fig:ShapeDetectionCollaborationDiagram}
// \end{figure}
//
// Figure~\ref{fig:ShapeDetectionCollaborationDiagram} shows the major
// components involved in the application of the
// ShapeDetectionLevelSetImageFilter to a segmentation task. The first stage
// involves smoothing using the
// \doxygen{CurvatureAnisotropicDiffusionImageFilter}. The smoothed image is
// passed as the input for the
// \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and then to the
// \doxygen{SigmoidImageFilter} in order to produce the edge potential image.
// A set of user-provided seeds is passed to an FastMarchingImageFilter in
// order to compute the distance map. A constant value is subtracted from
// this map in order to obtain a level set in which the \emph{zero set}
// represents the initial contour. This level set is also passed as input to
// the ShapeDetectionLevelSetImageFilter.
//
// Finally, the level set at the output of the
// ShapeDetectionLevelSetImageFilter is passed to an
// BinaryThresholdImageFilter in order to produce a binary mask representing
// the segmented object.
//
// Let's start by including the headers of the main filters involved in the
// preprocessing.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
// The edge potential map is generated using these filters as in the previous
// example.
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  We will need the Image class, the FastMarchingImageFilter class and the
//  ShapeDetectionLevelSetImageFilter class. Hence we include their headers
//  here.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkFastMarchingImageFilter.h"
#include "itkShapeDetectionLevelSetImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The level set resulting from the ShapeDetectionLevelSetImageFilter will
//  be thresholded at the zero level in order to get a binary image
//  representing the segmented object. The BinaryThresholdImageFilter is used
//  for this purpose.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBinaryThresholdImageFilter.h"
// Software Guide : EndCodeSnippet


//  Reading and writing images will be done with the \doxygen{ImageFileReader}
//  and \doxygen{ImageFileWriter}.

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


//  The RescaleIntensityImageFilter is used to renormailize the output
//  of filters before sending them to files.
#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char *argv[] )
{
  if( argc < 11 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage";
    std::cerr << " seedX seedY InitialDistance";
    std::cerr << " Sigma SigmoidAlpha SigmoidBeta ";
    std::cerr << " curvatureScaling propagationScaling" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We now define the image type using a particular pixel type and a
  //  dimension. In this case the \code{float} type is used for the pixels
  //  due to the requirements of the smoothing filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The output image, on the other hand, is declared to be binary.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The type of the BinaryThresholdImageFilter filter is instantiated below
  //  using the internal image type and the output image type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryThresholdImageFilter< InternalImageType, OutputImageType >
    ThresholdingFilterType;
  ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The upper threshold of the BinaryThresholdImageFilter is set
  //  to $0.0$ in order to display the zero set of the resulting level
  //  set. The lower threshold is set to a large negative number in order to
  //  ensure that the interior of the segmented object will appear
  //  inside the binary region.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  thresholder->SetLowerThreshold( -1000.0 );
  thresholder->SetUpperThreshold(     0.0 );

  thresholder->SetOutsideValue(  0  );
  thresholder->SetInsideValue(  255 );
  // Software Guide : EndCodeSnippet


  // We instantiate reader and writer types in the following lines.
  //
  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  The RescaleIntensityImageFilter type is declared below. This filter will
  //  renormalize image before sending them to writers.
  //
  typedef itk::RescaleIntensityImageFilter<InternalImageType, OutputImageType>
    CastFilterType;

  //  Software Guide : BeginLatex
  //
  //  The CurvatureAnisotropicDiffusionImageFilter type is instantiated using
  //  the internal image type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   itk::CurvatureAnisotropicDiffusionImageFilter<
                               InternalImageType,
                               InternalImageType >  SmoothingFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter is instantiated by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SmoothingFilterType::Pointer smoothing = SmoothingFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The types of the GradientMagnitudeRecursiveGaussianImageFilter and
  //  SigmoidImageFilter are instantiated using the internal
  //  image type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   itk::GradientMagnitudeRecursiveGaussianImageFilter<
                               InternalImageType,
                               InternalImageType >  GradientFilterType;

  typedef   itk::SigmoidImageFilter<
                               InternalImageType,
                               InternalImageType >  SigmoidFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The corresponding filter objects are created with the method
  //  \code{New()}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  GradientFilterType::Pointer  gradientMagnitude = GradientFilterType::New();
  SigmoidFilterType::Pointer sigmoid = SigmoidFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The minimum and maximum values of the SigmoidImageFilter output are
  //  defined with the methods \code{SetOutputMinimum()} and
  //  \code{SetOutputMaximum()}. In our case, we want these two values to be
  //  $0.0$ and $1.0$ respectively in order to get a nice speed image to feed
  //  to the FastMarchingImageFilter. Additional details on the use of the
  //  SigmoidImageFilter are presented in
  //  Section~\ref{sec:IntensityNonLinearMapping}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sigmoid->SetOutputMinimum(  0.0  );
  sigmoid->SetOutputMaximum(  1.0  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We now declare the type of the FastMarchingImageFilter that
  //  will be used to generate the initial level set in the form of a distance
  //  map.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  itk::FastMarchingImageFilter< InternalImageType, InternalImageType >
    FastMarchingFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Next we construct one filter of this class using the \code{New()}
  //  method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FastMarchingFilterType::Pointer  fastMarching
                                              = FastMarchingFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In the following lines we instantiate the type of the
  //  ShapeDetectionLevelSetImageFilter and create an object of this type
  //  using the \code{New()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  itk::ShapeDetectionLevelSetImageFilter< InternalImageType,
                              InternalImageType >    ShapeDetectionFilterType;
  ShapeDetectionFilterType::Pointer
    shapeDetection = ShapeDetectionFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:ShapeDetectionCollaborationDiagram} with the following
  //  code.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );
  gradientMagnitude->SetInput( smoothing->GetOutput() );
  sigmoid->SetInput( gradientMagnitude->GetOutput() );

  shapeDetection->SetInput( fastMarching->GetOutput() );
  shapeDetection->SetFeatureImage( sigmoid->GetOutput() );

  thresholder->SetInput( shapeDetection->GetOutput() );

  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The CurvatureAnisotropicDiffusionImageFilter requires a couple
  //  of parameters to be defined. The following are typical values for $2D$
  //  images. However they may have to be adjusted depending on the amount of
  //  noise present in the input image. This filter has been discussed in
  //  Section~\ref{sec:GradientAnisotropicDiffusionImageFilter}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  smoothing->SetTimeStep( 0.125 );
  smoothing->SetNumberOfIterations(  5 );
  smoothing->SetConductanceParameter( 9.0 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The GradientMagnitudeRecursiveGaussianImageFilter performs the
  //  equivalent of a convolution with a Gaussian kernel followed by a
  //  derivative operator. The sigma of this Gaussian can be used to control
  //  the range of influence of the image edges. This filter has been discussed
  //  in Section~\ref{sec:GradientMagnitudeRecursiveGaussianImageFilter}.
  //
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!SetSigma()}
  //
  //  Software Guide : EndLatex

  const double sigma = atof( argv[6] );

  // Software Guide : BeginCodeSnippet
  gradientMagnitude->SetSigma(  sigma  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The SigmoidImageFilter requires two parameters that define the linear
  //  transformation to be applied to the sigmoid argument. These parameters
  //  have been discussed in Sections~\ref{sec:IntensityNonLinearMapping} and
  //  \ref{sec:FastMarchingImageFilter}.
  //
  //  Software Guide : EndLatex

  const double alpha =  atof( argv[7] );
  const double beta  =  atof( argv[8] );

  // Software Guide : BeginCodeSnippet
  sigmoid->SetAlpha( alpha );
  sigmoid->SetBeta(  beta  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The FastMarchingImageFilter requires the user to provide a seed
  //  point from which the level set will be generated. The user can actually
  //  pass not only one seed point but a set of them. Note the
  //  FastMarchingImageFilter is used here only as a helper in the
  //  determination of an initial level set. We could have used the
  //  \doxygen{DanielssonDistanceMapImageFilter} in the same way.
  //
  //  \index{itk::FastMarchingImageFilter!Multiple seeds}
  //
  //  The seeds are stored in a container. The type of this
  //  container is defined as \code{NodeContainer} among the
  //  FastMarchingImageFilter traits.
  //
  //  \index{itk::FastMarchingImageFilter!Nodes}
  //  \index{itk::FastMarchingImageFilter!NodeContainer}
  //  \index{itk::FastMarchingImageFilter!NodeType}
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  typedef FastMarchingFilterType::NodeContainer           NodeContainer;
  typedef FastMarchingFilterType::NodeType                NodeType;
  NodeContainer::Pointer seeds = NodeContainer::New();
  //  Software Guide : EndCodeSnippet


  InternalImageType::IndexType  seedPosition;

  seedPosition[0] = atoi( argv[3] );
  seedPosition[1] = atoi( argv[4] );


  //  Software Guide : BeginLatex
  //
  //  Nodes are created as stack variables and initialized with a value and
  //  an \doxygen{Index} position. Note that we assign the negative of the
  //  value of the user-provided distance to the unique node of the seeds
  //  passed to the FastMarchingImageFilter. In this way, the value
  //  will increment as the front is propagated, until it reaches the zero
  //  value corresponding to the contour. After this, the front will continue
  //  propagating until it fills up the entire image. The initial distance is
  //  taken from the command line arguments. The rule of thumb for the user
  //  is to select this value as the distance from the seed points at which
  //  the initial contour should be.
  //
  //  \index{itk::FastMarchingImageFilter!Seed initialization}
  //
  //  Software Guide : EndLatex

  const double initialDistance = atof( argv[5] );

  // Software Guide : BeginCodeSnippet
  NodeType node;
  const double seedValue = - initialDistance;

  node.SetValue( seedValue );
  node.SetIndex( seedPosition );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The list of nodes is initialized and then every node is inserted using
  //  \code{InsertElement()}.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  seeds->Initialize();
  seeds->InsertElement( 0, node );
  //  Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The set of seed nodes is now passed to the FastMarchingImageFilter with
  //  the method \code{SetTrialPoints()}.
  //
  //  \index{itk::FastMarchingImageFilter!SetTrialPoints()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  fastMarching->SetTrialPoints(  seeds  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Since the FastMarchingImageFilter is used here only as a distance map
  //  generator, it does not require a speed image as input.  Instead, the
  //  constant value $1.0$ is passed using the \code{SetSpeedConstant()}
  //  method.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  fastMarching->SetSpeedConstant( 1.0 );
  //  Software Guide : EndCodeSnippet


  //  Here we configure all the writers required to see the intermediate
  //  outputs of the pipeline. This is added here only for
  //  pedagogical/debugging purposes. These intermediate output are normaly
  //  not required. Only the output of the final thresholding filter should
  //  be relevant.  Observing intermediate output is helpful in the process
  //  of fine tuning the parameters of filters in the pipeline.
  //
  CastFilterType::Pointer caster1 = CastFilterType::New();
  CastFilterType::Pointer caster2 = CastFilterType::New();
  CastFilterType::Pointer caster3 = CastFilterType::New();
  CastFilterType::Pointer caster4 = CastFilterType::New();

  WriterType::Pointer writer1 = WriterType::New();
  WriterType::Pointer writer2 = WriterType::New();
  WriterType::Pointer writer3 = WriterType::New();
  WriterType::Pointer writer4 = WriterType::New();

  caster1->SetInput( smoothing->GetOutput() );
  writer1->SetInput( caster1->GetOutput() );
  writer1->SetFileName("ShapeDetectionLevelSetFilterOutput1.png");
  caster1->SetOutputMinimum(   0 );
  caster1->SetOutputMaximum( 255 );
  writer1->Update();

  caster2->SetInput( gradientMagnitude->GetOutput() );
  writer2->SetInput( caster2->GetOutput() );
  writer2->SetFileName("ShapeDetectionLevelSetFilterOutput2.png");
  caster2->SetOutputMinimum(   0 );
  caster2->SetOutputMaximum( 255 );
  writer2->Update();

  caster3->SetInput( sigmoid->GetOutput() );
  writer3->SetInput( caster3->GetOutput() );
  writer3->SetFileName("ShapeDetectionLevelSetFilterOutput3.png");
  caster3->SetOutputMinimum(   0 );
  caster3->SetOutputMaximum( 255 );
  writer3->Update();

  caster4->SetInput( fastMarching->GetOutput() );
  writer4->SetInput( caster4->GetOutput() );
  writer4->SetFileName("ShapeDetectionLevelSetFilterOutput4.png");
  caster4->SetOutputMinimum(   0 );
  caster4->SetOutputMaximum( 255 );


  //  Software Guide : BeginLatex
  //
  //  The FastMarchingImageFilter requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter have been called
  //  directly or indirectly.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  fastMarching->SetOutputSize(
           reader->GetOutput()->GetBufferedRegion().GetSize() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  ShapeDetectionLevelSetImageFilter provides two parameters to control
  //  the competition between the propagation or expansion term and the
  //  curvature smoothing term. The methods \code{SetPropagationScaling()}
  //  and \code{SetCurvatureScaling()} defines the relative weighting between
  //  the two terms. In this example, we will set the propagation scaling to
  //  one and let the curvature scaling be an input argument. The larger the
  //  the curvature scaling parameter the smoother the resulting
  //  segmentation.  However, the curvature scaling parameter should not be
  //  set too large, as it will draw the contour away from the shape
  //  boundaries.
  //
  //  \index{itk::Shape\-Detection\-LevelSet\-Image\-Filter!SetPropagationScaling()}
  //  \index{itk::Segmentation\-Level\-Set\-Image\-Filter!SetPropagationScaling()}
  //  \index{itk::Shape\-Detection\-Level\-Set\-Image\-Filter!SetCurvatureScaling()}
  //  \index{itk::Segmentation\-Level\-Set\-Image\-Filter!SetCurvatureScaling()}
  //
  //  Software Guide : EndLatex

  const double curvatureScaling   = atof( argv[  9 ] );
  const double propagationScaling = atof( argv[ 10 ] );

  //  Software Guide : BeginCodeSnippet
  shapeDetection->SetPropagationScaling(  propagationScaling );
  shapeDetection->SetCurvatureScaling( curvatureScaling );
  //  Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Once activated, the level set evolution will stop if the convergence
  //  criteria or the maximum number of iterations is reached.  The
  //  convergence criteria are defined in terms of the root mean squared
  //  (RMS) change in the level set function. The evolution is said to have
  //  converged if the RMS change is below a user-specified threshold.  In a
  //  real application, it is desirable to couple the evolution of the zero
  //  set to a visualization module, allowing the user to follow the
  //  evolution of the zero set. With this feedback, the user may decide when
  //  to stop the algorithm before the zero set leaks through the regions of
  //  low gradient in the contour of the anatomical structure to be
  //  segmented.
  //
  //  \index{itk::Shape\-Detection\-Level\-Set\-Image\-Filter!SetMaximumRMSError()}
  //  \index{itk::Shape\-Detection\-Level\-Set\-Image\-Filter!SetNumberOfIterations()}
  //  \index{itk::Segmentation\-Level\-Set\-Image\-Filter!SetMaximumRMSError()}
  //  \index{itk::Segmentation\-Level\-Set\-Image\-Filter!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  shapeDetection->SetMaximumRMSError( 0.02 );
  shapeDetection->SetNumberOfIterations( 800 );
  //  Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The invocation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  As usual, the call is placed in a
  //  \code{try/catch} block should any errors occur or exceptions be thrown.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  // Print out some useful information
  std::cout << std::endl;
  std::cout << "Max. no. iterations: " << shapeDetection->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << shapeDetection->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << shapeDetection->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << shapeDetection->GetRMSChange() << std::endl;

  writer4->Update();


  // The following writer type is used to save the output of the time-crossing
  // map in a file with apropiate pixel representation. The advantage of saving
  // this image in native format is that it can be used with a viewer to help
  // determine an appropriate threshold to be used on the output of the
  // fastmarching filter.
  //
  typedef itk::ImageFileWriter< InternalImageType > InternalWriterType;

  InternalWriterType::Pointer mapWriter = InternalWriterType::New();
  mapWriter->SetInput( fastMarching->GetOutput() );
  mapWriter->SetFileName("ShapeDetectionLevelSetFilterOutput4.mha");
  mapWriter->Update();

  InternalWriterType::Pointer speedWriter = InternalWriterType::New();
  speedWriter->SetInput( sigmoid->GetOutput() );
  speedWriter->SetFileName("ShapeDetectionLevelSetFilterOutput3.mha");
  speedWriter->Update();

  InternalWriterType::Pointer gradientWriter = InternalWriterType::New();
  gradientWriter->SetInput( gradientMagnitude->GetOutput() );
  gradientWriter->SetFileName("ShapeDetectionLevelSetFilterOutput2.mha");
  gradientWriter->Update();


  //  Software Guide : BeginLatex
  //
  //  Let's now run this example using as input the image
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations.
  //  Table~\ref{tab:ShapeDetectionLevelSetFilterOutput} presents the
  //  parameters used for some structures.  For all of the examples illustrated
  //  in this table, the propagation scaling was set to $1.0$, and the
  //  curvature scaling set to 0.05.
  //
  //  \begin{table}
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|c|c|}
  //  \hline
  //  Structure    & Seed Index & Distance & $\sigma$ & $\alpha$ & $\beta$ & Output Image \\ \hline
  //  Left Ventricle  & $(81,114)$ & 5.0 & 1.0 & -0.5 & 3.0  & First  in Figure \ref{fig:ShapeDetectionLevelSetFilterOutput2} \\  \hline
  //  Right Ventricle & $(99,114)$ & 5.0 & 1.0 & -0.5 & 3.0  & Second in Figure \ref{fig:ShapeDetectionLevelSetFilterOutput2} \\  \hline
  //  White matter    & $(56, 92)$ & 5.0 & 1.0 & -0.3 & 2.0  & Third  in Figure \ref{fig:ShapeDetectionLevelSetFilterOutput2} \\  \hline
  //  Gray matter     & $(40, 90)$ & 5.0 & 0.5 & -0.3 & 2.0  & Fourth in Figure \ref{fig:ShapeDetectionLevelSetFilterOutput2} \\  \hline
  //  \end{tabular}
  //  \end{center}
  //  \itkcaption[ShapeDetection example parameters]{Parameters used for
  //  segmenting some brain structures shown in
  //  Figure~\ref{fig:ShapeDetectionLevelSetFilterOutput} using the filter
  //  ShapeDetectionLevelSetFilter. All of them used a propagation
  //  scaling of $1.0$ and curvature scaling of
  //  $0.05$.\label{tab:ShapeDetectionLevelSetFilterOutput}}
  //  \end{table}
  //
  //  Figure~\ref{fig:ShapeDetectionLevelSetFilterOutput} presents the
  //  intermediate outputs of the pipeline illustrated in
  //  Figure~\ref{fig:ShapeDetectionCollaborationDiagram}. They are from left
  //  to right: the output of the anisotropic diffusion filter, the gradient
  //  magnitude of the smoothed image and the sigmoid of the gradient magnitude
  //  which is finally used as the edge potential for the
  //  ShapeDetectionLevelSetImageFilter.
  //
  //  Notice that in Figure~\ref{fig:ShapeDetectionLevelSetFilterOutput2} the
  //  segmented shapes are rounder than in
  //  Figure~\ref{fig:FastMarchingImageFilterOutput2} due to the effects of the
  //  curvature term in the driving equation. As with the previous example,
  //  segmentation of the gray matter is still problematic.
  //
  // \begin{figure} \center
  // \includegraphics[height=0.40\textheight]{BrainProtonDensitySlice}
  // \includegraphics[height=0.40\textheight]{ShapeDetectionLevelSetFilterOutput1}
  // \includegraphics[height=0.40\textheight]{ShapeDetectionLevelSetFilterOutput2}
  // \includegraphics[height=0.40\textheight]{ShapeDetectionLevelSetFilterOutput3}
  // \itkcaption[ShapeDetectionLevelSetImageFilter intermediate output]{Images generated by
  // the segmentation process based on the ShapeDetectionLevelSetImageFilter. From left
  // to right and top to bottom: input image to be segmented, image smoothed with an
  // edge-preserving smoothing filter, gradient magnitude of the smoothed
  // image, sigmoid of the gradient magnitude. This last image, the sigmoid, is
  // used to compute the speed term for the front propagation.}
  // \label{fig:ShapeDetectionLevelSetFilterOutput}
  // \end{figure}
  //
  //  A larger number of iterations is reguired for segmenting large
  //  structures since it takes longer for the front to propagate and cover
  //  the structure. This drawback can be easily mitigated by setting many
  //  seed points in the initialization of the
  //  FastMarchingImageFilter. This will generate an initial level
  //  set much closer in shape to the object to be segmented and hence
  //  require fewer iterations to fill and reach the edges of the anatomical
  //  structure.
  //
  // \begin{figure} \center
  // \includegraphics[width=0.24\textwidth]{ShapeDetectionLevelSetFilterOutput5}
  // \includegraphics[width=0.24\textwidth]{ShapeDetectionLevelSetFilterOutput6}
  // \includegraphics[width=0.24\textwidth]{ShapeDetectionLevelSetFilterOutput7}
  // \includegraphics[width=0.24\textwidth]{ShapeDetectionLevelSetFilterOutput8}
  // \itkcaption[ShapeDetectionLevelSetImageFilter segmentations]{Images generated by the
  // segmentation process based on the ShapeDetectionLevelSetImageFilter. From left to
  // right: segmentation of the left ventricle, segmentation of the right
  // ventricle, segmentation of the white matter, attempt of segmentation of
  // the gray matter.}
  // \label{fig:ShapeDetectionLevelSetFilterOutput2}
  // \end{figure}
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
