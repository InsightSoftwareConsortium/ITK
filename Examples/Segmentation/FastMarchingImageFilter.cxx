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
//    OUTPUTS: {FastMarchingImageFilterOutput5.png}
//    ARGUMENTS:    81 114 1.0  -0.5  3.0   100 100
//    OUTPUTS: {FastMarchingFilterOutput1.png}
//    OUTPUTS: {FastMarchingFilterOutput2.png}
//    OUTPUTS: {FastMarchingFilterOutput3.png}
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {FastMarchingImageFilterOutput6.png}
//    ARGUMENTS:    99 114 1.0  -0.5  3.0   100 100
//    OUTPUTS: {FastMarchingFilterOutput1.png}
//    OUTPUTS: {FastMarchingFilterOutput2.png}
//    OUTPUTS: {FastMarchingFilterOutput3.png}
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {FastMarchingImageFilterOutput7.png}
//    ARGUMENTS:    56 92 1.0  -0.3  2.0   200 100
//    OUTPUTS: {FastMarchingFilterOutput1.png}
//    OUTPUTS: {FastMarchingFilterOutput2.png}
//    OUTPUTS: {FastMarchingFilterOutput3.png}
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {FastMarchingImageFilterOutput8.png}
//    ARGUMENTS:    40 90 0.5  -0.3  2.0   200 100
//    OUTPUTS: {FastMarchingFilterOutput1.png}
//    OUTPUTS: {FastMarchingFilterOutput2.png}
//    OUTPUTS: {FastMarchingFilterOutput3.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// When the differential equation governing the level set evolution has
// a very simple form, a fast evolution algorithm called fast marching
// can be used.
//
// The following example illustrates the use of the
// \doxygen{FastMarchingImageFilter}. This filter implements a fast marching
// solution to a simple level set evolution problem.  In this example, the
// speed term used in the differential equation is expected to be provided by
// the user in the form of an image.  This image is typically computed as a
// function of the gradient magnitude.  Several mappings are popular in the
// literature, for example, the negative exponential $exp(-x)$ and the
// reciprocal $1/(1+x)$. In the current example we decided to use a Sigmoid
// function since it offers a good number of control parameters that can be
// customized to shape a nice speed image.
//
// The mapping should be done in such a way that the propagation speed of the
// front will be very low close to high image gradients while it will move
// rather fast in low gradient areas. This arrangement will make the contour
// propagate until it reaches the edges of anatomical structures in the image
// and then slow down in front of those edges.  The output of the
// FastMarchingImageFilter is a \emph{time-crossing map} that
// indicates, for each pixel, how much time it would take for the front to
// arrive at the pixel location.
//
// \begin{figure} \center
// \includegraphics[width=\textwidth]{FastMarchingCollaborationDiagram1}
// \itkcaption[FastMarchingImageFilter collaboration diagram]{Collaboration
// diagram of the FastMarchingImageFilter applied to a segmentation task.}
// \label{fig:FastMarchingCollaborationDiagram}
// \end{figure}
//
// The application of a threshold in the output image is then equivalent to
// taking a snapshot of the contour at a particular time during its evolution.
// It is expected that the contour will take a longer time to cross over
// the edges of a particular anatomical structure. This should result in large
// changes on the time-crossing map values close to the structure edges.
// Segmentation is performed with this filter by locating a time range in which
// the contour was contained for a long time in a region of the image space.
//
// Figure~\ref{fig:FastMarchingCollaborationDiagram} shows the major components
// involved in the application of the FastMarchingImageFilter to a
// segmentation task. It involves an initial stage of smoothing using the
// \doxygen{CurvatureAnisotropicDiffusionImageFilter}. The smoothed image is
// passed as the input to the
// \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and then to the
// \doxygen{SigmoidImageFilter}.  Finally, the output of the
// FastMarchingImageFilter is passed to a
// \doxygen{BinaryThresholdImageFilter} in order to produce a binary mask
// representing the segmented object.
//
// The code in the following example illustrates the typical setup of a
// pipeline for performing segmentation with fast marching. First, the input
// image is smoothed using an edge-preserving filter. Then the magnitude of its
// gradient is computed and passed to a sigmoid filter. The result of the
// sigmoid filter is the image potential that will be used to affect the speed
// term of the differential equation.
//
// Let's start by including the following headers. First we include the header
// of the CurvatureAnisotropicDiffusionImageFilter that will be used
// for removing noise from the input image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//
//  The headers of the GradientMagnitudeRecursiveGaussianImageFilter and
//  SigmoidImageFilter are included below. Together, these two filters will
//  produce the image potential for regulating the speed term in the
//  differential equation describing the evolution of the level set.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Of course, we will need the \doxygen{Image} class and the
//  FastMarchingImageFilter class. Hence we include their headers.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkFastMarchingImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The time-crossing map resulting from the FastMarchingImageFilter
//  will be thresholded using the BinaryThresholdImageFilter. We
//  include its header here.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBinaryThresholdImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Reading and writing images will be done with the \doxygen{ImageFileReader}
//  and \doxygen{ImageFileWriter}.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet


//  The \doxygen{RescaleIntensityImageFilter} is used to renormailize the
//  output of filters before sending them to files.
//
#include "itkRescaleIntensityImageFilter.h"

static void PrintCommandLineUsage( const int argc, const char * const argv[] )
{
  std::cerr << "Missing Parameters " << std::endl;
  std::cerr << "Usage: " << argv[0];
  std::cerr << " inputImage  outputImage seedX seedY";
  std::cerr << " Sigma SigmoidAlpha SigmoidBeta TimeThreshold StoppingValue";
  std::cerr << " smoothingOutputImage gradientMagnitudeOutputImage sigmoidOutputImage" << std::endl;

  for (int qq=0; qq< argc; ++qq)
    {
    std::cout << "argv[" << qq << "] = " << argv[qq] << std::endl;
    }
}

int main( int argc, char *argv[] )
{
  if (argc != 13)
    {
    PrintCommandLineUsage(argc, argv);
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  We now define the image type using a pixel type and a particular
  //  dimension. In this case the \code{float} type is used for the pixels due
  //  to the requirements of the smoothing filter.
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
  //  The type of the BinaryThresholdImageFilter filter is
  //  instantiated below using the internal image type and the output image
  //  type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryThresholdImageFilter< InternalImageType,
                        OutputImageType    >    ThresholdingFilterType;
  ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The upper threshold passed to the BinaryThresholdImageFilter
  //  will define the time snapshot that we are taking from the time-crossing
  //  map. In an ideal application the user should be able to select this
  //  threshold interactively using visual feedback. Here, since it is a
  //  minimal example, the value is taken from the command line arguments.
  //
  //  Software Guide : EndLatex

  const InternalPixelType  timeThreshold = atof( argv[8] );

  // Software Guide : BeginCodeSnippet
  thresholder->SetLowerThreshold(           0.0  );
  thresholder->SetUpperThreshold( timeThreshold  );

  thresholder->SetOutsideValue(  0  );
  thresholder->SetInsideValue(  255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // We instantiate reader and writer types in the following lines.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  The RescaleIntensityImageFilter type is declared below. This filter will
  //  renormalize image before sending them to writers.
  //
  typedef itk::RescaleIntensityImageFilter<
                               InternalImageType,
                               OutputImageType >   CastFilterType;

  //  Software Guide : BeginLatex
  //
  //  The CurvatureAnisotropicDiffusionImageFilter type is
  //  instantiated using the internal image type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   itk::CurvatureAnisotropicDiffusionImageFilter<
                               InternalImageType,
                               InternalImageType >  SmoothingFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, the filter is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SmoothingFilterType::Pointer smoothing = SmoothingFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The types of the
  //  GradientMagnitudeRecursiveGaussianImageFilter and
  //  SigmoidImageFilter are instantiated using the internal image
  //  type.
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
  //  The corresponding filter objects are instantiated with the
  //  \code{New()} method.
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
  //  to the FastMarchingImageFilter. Additional details on the use of
  //  the SigmoidImageFilter are presented in
  //  Section~\ref{sec:IntensityNonLinearMapping}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sigmoid->SetOutputMinimum(  0.0  );
  sigmoid->SetOutputMaximum(  1.0  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We now declare the type of the FastMarchingImageFilter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  itk::FastMarchingImageFilter< InternalImageType,
                              InternalImageType >    FastMarchingFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, we construct one filter of this class using the \code{New()}
  //  method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FastMarchingFilterType::Pointer  fastMarching
                                              = FastMarchingFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filters are now connected in a pipeline shown in
  //  Figure~\ref{fig:FastMarchingCollaborationDiagram} using the following
  //  lines.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );
  gradientMagnitude->SetInput( smoothing->GetOutput() );
  sigmoid->SetInput( gradientMagnitude->GetOutput() );
  fastMarching->SetInput( sigmoid->GetOutput() );
  thresholder->SetInput( fastMarching->GetOutput() );
  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The CurvatureAnisotropicDiffusionImageFilter class requires a couple
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

  const double sigma = atof( argv[5] );

  // Software Guide : BeginCodeSnippet
  gradientMagnitude->SetSigma(  sigma  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The SigmoidImageFilter class requires two parameters to define the linear
  //  transformation to be applied to the sigmoid argument. These parameters
  //  are passed using the \code{SetAlpha()} and \code{SetBeta()} methods. In
  //  the context of this example, the parameters are used to intensify the
  //  differences between regions of low and high values in the speed image. In
  //  an ideal case, the speed value should be $1.0$ in the homogeneous regions
  //  of anatomical structures and the value should decay rapidly to $0.0$
  //  around the edges of structures. The heuristic for finding the values is
  //  the following: From the gradient magnitude image, let's call $K1$ the
  //  minimum value along the contour of the anatomical structure to be
  //  segmented. Then, let's call $K2$ an average value of the gradient
  //  magnitude in the middle of the structure. These two values indicate the
  //  dynamic range that we want to map to the interval $[0:1]$ in the speed
  //  image.  We want the sigmoid to map $K1$ to $0.0$ and $K2$ to $1.0$. Given
  //  that $K1$ is expected to be higher than $K2$ and we want to map those
  //  values to $0.0$ and $1.0$ respectively, we want to select a negative
  //  value for alpha so that the sigmoid function will also do an inverse
  //  intensity mapping. This mapping will produce a speed image such that the
  //  level set will march rapidly on the homogeneous region and will
  //  definitely stop on the contour. The suggested value for beta is
  //  $(K1+K2)/2$ while the suggested value for alpha is $(K2-K1)/6$, which
  //  must be a negative number.  In our simple example the values are provided
  //  by the user from the command line arguments. The user can estimate these
  //  values by observing the gradient magnitude image.
  //
  //  Software Guide : EndLatex

  const double alpha =  atof( argv[6] );
  const double beta  =  atof( argv[7] );


  // Software Guide : BeginCodeSnippet
  sigmoid->SetAlpha( alpha );
  sigmoid->SetBeta(  beta  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The FastMarchingImageFilter requires the user to provide a seed point
  //  from which the contour will expand. The user can actually pass not only
  //  one seed point but a set of them. A good set of seed points increases
  //  the chances of segmenting a complex object without missing parts. The
  //  use of multiple seeds also helps to reduce the amount of time needed by
  //  the front to visit a whole object and hence reduces the risk of leaks
  //  on the edges of regions visited earlier. For example, when segmenting
  //  an elongated object, it is undesirable to place a single seed at one
  //  extreme of the object since the front will need a long time to
  //  propagate to the other end of the object. Placing several seeds along
  //  the axis of the object will probably be the best strategy to ensure
  //  that the entire object is captured early in the expansion of the
  //  front. One of the important properties of level sets is their natural
  //  ability to fuse several fronts implicitly without any extra
  //  bookkeeping. The use of multiple seeds takes good advantage of this
  //  property.
  //
  //  \index{itk::FastMarchingImageFilter!Multiple seeds}
  //
  //  The seeds are passed stored in a container. The type of this
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
  //  Nodes are created as stack variables and initialized with a value and an
  //  \doxygen{Index} position.
  //
  //  \index{itk::FastMarchingImageFilter!Seed initialization}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  NodeType node;
  const double seedValue = 0.0;

  node.SetValue( seedValue );
  node.SetIndex( seedPosition );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.
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


  //  Here we configure all the writers required to see the intermediate
  //  outputs of the pipeline. This is added here to provide
  //  the necessary images for generating the ITKSoftwareGuide.
  //  These intermediate outputs are normally not required. Only the output
  //  of the final thresholding filter should be relevant.  Observing
  //  intermediate output is helpful in the process of fine tuning the
  //  parameters of filters in the pipeline.
  //
  try
    {
    CastFilterType::Pointer caster1 = CastFilterType::New();
    WriterType::Pointer writer1 = WriterType::New();
    caster1->SetInput( smoothing->GetOutput() );
    writer1->SetInput( caster1->GetOutput() );
    writer1->SetFileName(argv[10]);
    caster1->SetOutputMinimum(   0 );
    caster1->SetOutputMaximum( 255 );
    writer1->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    CastFilterType::Pointer caster2 = CastFilterType::New();
    WriterType::Pointer writer2 = WriterType::New();
    caster2->SetInput( gradientMagnitude->GetOutput() );
    writer2->SetInput( caster2->GetOutput() );
    writer2->SetFileName(argv[11]);
    caster2->SetOutputMinimum(   0 );
    caster2->SetOutputMaximum( 255 );
    writer2->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    CastFilterType::Pointer caster3 = CastFilterType::New();
    WriterType::Pointer writer3 = WriterType::New();
    caster3->SetInput( sigmoid->GetOutput() );
    writer3->SetInput( caster3->GetOutput() );
    writer3->SetFileName(argv[12]);
    caster3->SetOutputMinimum(   0 );
    caster3->SetOutputMaximum( 255 );
    writer3->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  The FastMarchingImageFilter requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()} method. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} method of this filter has been called
  //  directly or indirectly.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  fastMarching->SetOutputSize(
           reader->GetOutput()->GetBufferedRegion().GetSize() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Since the front representing the contour will propagate continuously
  //  over time, it is desirable to stop the process once a certain time has
  //  been reached. This allows us to save computation time under the
  //  assumption that the region of interest has already been computed. The
  //  value for stopping the process is defined with the method
  //  \code{SetStoppingValue()}. In principle, the stopping value should be a
  //  little bit higher than the threshold value.
  //
  //  \index{itk::FastMarchingImageFilter!SetStoppingValue()}
  //
  //  Software Guide : EndLatex

  const double stoppingTime = atof( argv[9] );

  // Software Guide : BeginCodeSnippet
  fastMarching->SetStoppingValue(  stoppingTime  );
  // Software Guide : EndCodeSnippet


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

  try
    {
    CastFilterType::Pointer caster4 = CastFilterType::New();
    WriterType::Pointer writer4 = WriterType::New();
    caster4->SetInput( fastMarching->GetOutput() );
    writer4->SetInput( caster4->GetOutput() );
    writer4->SetFileName("FastMarchingFilterOutput4.png");
    caster4->SetOutputMinimum(   0 );
    caster4->SetOutputMaximum( 255 );
    writer4->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }


  // The following writer type is used to save the output of the
  // time-crossing map in a file with appropiate pixel representation. The
  // advantage of saving this image in native format is that it can be used
  // with a viewer to help determine an appropriate threshold to be used on
  // the output of the \code{fastmarching} filter.
  //
  typedef itk::ImageFileWriter< InternalImageType > InternalWriterType;

  InternalWriterType::Pointer mapWriter = InternalWriterType::New();
  mapWriter->SetInput( fastMarching->GetOutput() );
  mapWriter->SetFileName("FastMarchingFilterOutput4.mha");
  mapWriter->Update();

  InternalWriterType::Pointer speedWriter = InternalWriterType::New();
  speedWriter->SetInput( sigmoid->GetOutput() );
  speedWriter->SetFileName("FastMarchingFilterOutput3.mha");
  speedWriter->Update();

  InternalWriterType::Pointer gradientWriter = InternalWriterType::New();
  gradientWriter->SetInput( gradientMagnitude->GetOutput() );
  gradientWriter->SetFileName("FastMarchingFilterOutput2.mha");
  gradientWriter->Update();


  //  Software Guide : BeginLatex
  //
  //  Now let's run this example using the input image
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations. The following
  //  table presents the parameters used for some structures.
  //
  //  \begin{table}
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|c|c|p{2cm}|}
  //  \hline
  //  Structure    & Seed Index & $\sigma$ & $\alpha$ & $\beta$ & Threshold & Output Image from left \\ \hline
  //  Left Ventricle  & $(81,114)$ & 1.0 & -0.5 & 3.0  & 100 & First   \\  \hline
  //  Right Ventricle & $(99,114)$ & 1.0 & -0.5 & 3.0  & 100 & Second  \\  \hline
  //  White matter    & $(56, 92)$ & 1.0 & -0.3 & 2.0  & 200 & Third   \\  \hline
  //  Gray matter     & $(40, 90)$ & 0.5 & -0.3 & 2.0  & 200 & Fourth  \\  \hline
  //  \end{tabular}
  //  \end{center}
  //  \itkcaption[FastMarching segmentation example parameters]{Parameters used
  //  for segmenting some brain structures shown in
  //  Figure~\ref{fig:FastMarchingImageFilterOutput2} using the filter
  //  FastMarchingImageFilter. All of them used a stopping value of
  //  100.\label{tab:FastMarchingImageFilterOutput2}}
  //  \end{table}
  //
  //  Figure~\ref{fig:FastMarchingImageFilterOutput} presents the intermediate
  //  outputs of the pipeline illustrated in
  //  Figure~\ref{fig:FastMarchingCollaborationDiagram}. They are from left to
  //  right: the output of the anisotropic diffusion filter, the gradient
  //  magnitude of the smoothed image and the sigmoid of the gradient magnitude
  //  which is finally used as the speed image for the
  //  FastMarchingImageFilter.
  //
  // \begin{figure} \center
  // \includegraphics[height=0.40\textheight]{BrainProtonDensitySlice}
  // \includegraphics[height=0.40\textheight]{FastMarchingFilterOutput1}
  // \includegraphics[height=0.40\textheight]{FastMarchingFilterOutput2}
  // \includegraphics[height=0.40\textheight]{FastMarchingFilterOutput3}
  // \itkcaption[FastMarchingImageFilter intermediate output]{Images generated by
  // the segmentation process based on the FastMarchingImageFilter. From left
  // to right and top to bottom: input image to be segmented, image smoothed with an
  // edge-preserving smoothing filter, gradient magnitude of the smoothed
  // image, sigmoid of the gradient magnitude. This last image, the sigmoid, is
  // used to compute the speed term for the front propagation. }
  // \label{fig:FastMarchingImageFilterOutput}
  // \end{figure}
  //
  //  Notice that the gray matter is not being completely segmented.  This
  //  illustrates the vulnerability of the level set methods when the
  //  anatomical structures to be segmented do not occupy extended regions of
  //  the image. This is especially true when the width of the structure is
  //  comparable to the size of the attenuation bands generated by the
  //  gradient filter. A possible workaround for this limitation is to use
  //  multiple seeds distributed along the elongated object. However, note
  //  that white matter versus gray matter segmentation is not a trivial task,
  //  and may require a more elaborate approach than the one used in this
  //  basic example.
  //
  // \begin{figure} \center
  // \includegraphics[width=0.24\textwidth]{FastMarchingImageFilterOutput5}
  // \includegraphics[width=0.24\textwidth]{FastMarchingImageFilterOutput6}
  // \includegraphics[width=0.24\textwidth]{FastMarchingImageFilterOutput7}
  // \includegraphics[width=0.24\textwidth]{FastMarchingImageFilterOutput8}
  // \itkcaption[FastMarchingImageFilter segmentations]{Images generated by the
  // segmentation process based on the FastMarchingImageFilter. From left to
  // right: segmentation of the left ventricle, segmentation of the right
  // ventricle, segmentation of the white matter, attempt of segmentation of
  // the gray matter.}
  // \label{fig:FastMarchingImageFilterOutput2}
  // \end{figure}
  //
  //  Software Guide : EndLatex
  return EXIT_SUCCESS;
}
