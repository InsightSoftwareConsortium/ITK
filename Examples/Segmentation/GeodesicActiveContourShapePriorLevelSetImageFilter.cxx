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

// Software Guide : BeginLatex
//
// In medical imaging applications, the general shape, location and
// orientation of an anatomical structure of interest is typically
// known \emph{a priori}. This information can be used to aid the
// segmentation process especially when image contrast is low or
// when the object boundary is not distinct.
//
// In \cite{Leventon2000}, Leventon \emph{et al.} extended the
// geodesic active contours method with an additional shape-influenced term in
// the driving PDE. The \doxygen{GeodesicActiveContourShapePriorLevelSetFilter}
// is a generalization of Leventon's approach and its use is illustrated
// in the following example.
//
// To support shape-guidance, the generic level set
// equation (Eqn(~\ref{eqn:LevelSetEquation})) is extended to incorporate a
// shape guidance term:
//
// \begin{equation}
// \label{eqn:ShapeInfluenceTerm}
// \xi \left(\psi^{*}(\mathbf{x}) - \psi(\mathbf{x})\right)
// \end{equation}
//
// where $\psi^{*}$ is the signed distance function of the ``best-fit'' shape
// with respect to a shape model. The new term has the effect of driving the
// contour towards the best-fit shape. The scalar $\xi$ weights the influence
// of the shape term in the overall evolution. In general, the best-fit shape
// is not known ahead of time and has to be iteratively estimated in
// conjunction with the contour evolution.
//
// As with the \doxygen{GeodesicActiveContourLevelSetImageFilter}, the
// GeodesicActiveContourShapePriorLevelSetImageFilter expects two input
// images: the first is an initial level set and the second a feature image
// that represents the image edge potential. The configuration of this
// example is quite similar to the example in
// Section~\ref{sec:GeodesicActiveContourImageFilter} and hence the description
// will focus on the new objects involved in the segmentation process as shown
// in Figure~\ref{fig:GeodesicActiveContourShapePriorCollaborationDiagram}.
//
// \begin{figure} \center
// \includegraphics[width=\textwidth]{GeodesicActiveContourShapePriorCollaborationDiagram}
// \itkcaption[GeodesicActiveContourShapePriorLevelSetImageFilter collaboration
// diagram]{Collaboration diagram for the GeodesicActiveContourShapePriorLevelSetImageFilter
// applied to a segmentation task.}
// \label{fig:GeodesicActiveContourShapePriorCollaborationDiagram}
// \end{figure}
//
// The process pipeline begins with centering the input image using the
// the \doxygen{ChangeInformationImageFilter} to simplify the estimation of the pose
// of the shape, to be explained later.
// The centered image is then smoothed using non-linear diffusion to
// remove noise and the gradient magnitude is computed from the smoothed image.
// For simplicity, this example uses the \doxygen{BoundedReciprocalImageFilter}
// to produce the edge potential image.
//
// The \doxygen{FastMarchingImageFilter} creates an initial level set using three
// user specified seed positions and a initial contour radius. Three seeds are
// used in this example to facilitate the segmentation of long narrow objects
// in a smaller number of iterations.
// The output of the FastMarchingImageFilter is passed
// as the input to the GeodesicActiveContourShapePriorLevelSetImageFilter.
// At then end of the segmentation process, the output level set is passed
// to the \doxygen{BinaryThresholdImageFilter} to produce a binary mask
// representing the segmented object.
//
// The remaining objects in
// Figure~\ref{fig:GeodesicActiveContourShapePriorCollaborationDiagram}
// are used for shape modeling and estimation.
// The \doxygen{PCAShapeSignedDistanceFunction} represents a statistical
// shape model defined by a mean signed distance and the first $K$
// principal components modes; while the \doxygen{Euler2DTransform} is used
// to represent the pose of the shape. In this implementation, the
// best-fit shape estimation problem is reformulated as a minimization problem
// where the \doxygen{ShapePriorMAPCostFunction} is the cost function to
// be optimized using the \doxygen{OnePlusOneEvolutionaryOptimizer}.
//
// It should be noted that, although particular shape model, transform
// cost function, and optimizer are used in this example, the implementation
// is generic, allowing different instances of these components to be
// plugged in. This flexibility allows a user to tailor the behavior of the
// segmentation process to suit the circumstances of the targeted application.
//
// Let's start the example by including the headers of the new filters
// involved in the segmentation.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkBoundedReciprocalImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Next, we include the headers of the objects involved in shape
//  modeling and estimation.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkPCAShapeSignedDistanceFunction.h"
#include "itkEuler2DTransform.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkNormalVariateGenerator.h"
#include "vnl/vnl_sample.h"
#include "itkNumericSeriesFileNames.h"
// Software Guide : EndCodeSnippet

#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"


// Software Guide : BeginLatex
//
// Given the numerous parameters involved in tuning this segmentation method
// it is not uncommon for a segmentation process to
// run for several minutes and still produce an unsatisfactory result. For debugging
// purposes it is quite helpful to track the evolution of the
// segmentation as it progresses. The following defines a
// custom \doxygen{Command} class
// for monitoring the RMS change and shape parameters at each iteration.
//
//  \index{itk::Geodesic\-Active\-Contour\-Shape\-Prior\-LevelSet\-Image\-Filter!Monitoring}
//  \index{itk::Shape\-Prior\-Segmentation\-Level\-Set\-Image\-Filter!Monitoring}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCommand.h"

template<class TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  typedef CommandIterationUpdate   Self;
  typedef itk::Command             Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:

  void Execute(itk::Object *caller,
               const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) caller, event);
    }

  void Execute(const itk::Object * object,
               const itk::EventObject & event) ITK_OVERRIDE
    {
    const TFilter * filter = static_cast< const TFilter * >( object );
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      { return; }

    std::cout << filter->GetElapsedIterations() << ": ";
    std::cout << filter->GetRMSChange() << " ";
    std::cout << filter->GetCurrentParameters() << std::endl;
    }

};
// Software Guide : EndCodeSnippet


int main( int argc, char *argv[] )
{
  if( argc < 18 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage";
    std::cerr << " seed1X seed1Y";
    std::cerr << " seed2X seed2Y";
    std::cerr << " seed3X seed3Y";
    std::cerr << " initialDistance";
    std::cerr << " sigma";
    std::cerr << " propagationScaling shapePriorScaling";
    std::cerr << " meanShapeImage numberOfModes shapeModeFilePattern";
    std::cerr << " startX startY" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We define the image type using a particular pixel type and
  //  dimension. In this case we will use 2D \code{float} images.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet


  //  The following lines instantiate the thresholding filter that will
  //  process the final level set at the output of the
  //  GeodesicActiveContourLevelSetImageFilter.
  //
  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::BinaryThresholdImageFilter<
                        InternalImageType,
                        OutputImageType    >       ThresholdingFilterType;

  ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();

  thresholder->SetLowerThreshold( -1000.0 );
  thresholder->SetUpperThreshold(     0.0 );

  thresholder->SetOutsideValue(  0  );
  thresholder->SetInsideValue(  255 );


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
  typedef itk::RescaleIntensityImageFilter<
                               InternalImageType,
                               OutputImageType >   CastFilterType;


  //  The \doxygen{CurvatureAnisotropicDiffusionImageFilter} type is
  //  instantiated using the internal image type.
  //
  typedef   itk::CurvatureAnisotropicDiffusionImageFilter<
                               InternalImageType,
                               InternalImageType >  SmoothingFilterType;

  SmoothingFilterType::Pointer smoothing = SmoothingFilterType::New();


  //  The types of the
  //  GradientMagnitudeRecursiveGaussianImageFilter is
  //  instantiated using the internal image type.
  //
  typedef   itk::GradientMagnitudeRecursiveGaussianImageFilter<
                               InternalImageType,
                               InternalImageType >  GradientFilterType;

  GradientFilterType::Pointer  gradientMagnitude = GradientFilterType::New();


  //  We declare now the type of the FastMarchingImageFilter that
  //  will be used to generate the initial level set in the form of a distance
  //  map.
  //
  typedef  itk::FastMarchingImageFilter<
                              InternalImageType,
                              InternalImageType >    FastMarchingFilterType;


  //  Next we construct one filter of this class using the \code{New()}
  //  method.
  //
  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();

  //  Software Guide : BeginLatex
  //
  //  The following line instantiate a
  //  \doxygen{GeodesicActiveContourShapePriorLevelSetImageFilter}
  //  using the \code{New()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GeodesicActiveContourShapePriorLevelSetImageFilter<
            InternalImageType, InternalImageType >
                                              GeodesicActiveContourFilterType;
  GeodesicActiveContourFilterType::Pointer geodesicActiveContour =
                                       GeodesicActiveContourFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // The \doxygen{ChangeInformationImageFilter} is the first filter in the preprocessing
  // stage and is used to force the image origin to the center of the image.
  //
  //  \index{itk::ChangeInformationImageFilter!CenterImageOn()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ChangeInformationImageFilter<
                               InternalImageType >  CenterFilterType;

  CenterFilterType::Pointer center = CenterFilterType::New();
  center->CenterImageOn();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // In this example, we will use the bounded reciprocal $1/(1+x)$ of
  // the image gradient magnitude as the edge potential feature image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   itk::BoundedReciprocalImageFilter<
                               InternalImageType,
                               InternalImageType >  ReciprocalFilterType;

  ReciprocalFilterType::Pointer reciprocal = ReciprocalFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In the GeodesicActiveContourShapePriorLevelSetImageFilter, scaling parameters
  //  are used to trade off between the propagation (inflation), the
  //  curvature (smoothing), the advection, and the shape influence terms.
  //  These parameters are set
  //  using methods \code{SetPropagationScaling()},
  //  \code{SetCurvatureScaling()}, \code{SetAdvectionScaling()} and
  //  \code{SetShapePriorScaling()}. In this
  //  example, we will set the curvature and advection scales to one and let
  //  the propagation and shape prior scale be command-line arguments.
  //
  //  \index{itk::Geodesic\-Active\-Contour\-Shape\-Prior\-LevelSet\-Image\-Filter!SetPropagationScaling()}
  //  \index{itk::Shape\-Prior\-Segmentation\-Level\-Set\-Image\-Filter!SetPropagationScaling()}
  //  \index{itk::Geodesic\-Active\-Contour\-Shape\-Prior\-LevelSet\-Image\-Filter!SetCurvatureScaling()}
  //  \index{itk::Shape\-Prior\-Segmentation\-Level\-Set\-Image\-Filter!SetCurvatureScaling()}
  //  \index{itk::Geodesic\-Active\-Contour\-Shape\-Prior\-LevelSet\-Image\-Filter!SetAdvectionScaling()}
  //  \index{itk::Shape\-Prior\-Segmentation\-Level\-Set\-Image\-Filter!SetAdvectionScaling()}
  //
  //  Software Guide : EndLatex

  const double propagationScaling = atof( argv[11] );
  const double shapePriorScaling  = atof( argv[12] );

  //  Software Guide : BeginCodeSnippet
  geodesicActiveContour->SetPropagationScaling( propagationScaling );
  geodesicActiveContour->SetShapePriorScaling( shapePriorScaling );
  geodesicActiveContour->SetCurvatureScaling( 1.0 );
  geodesicActiveContour->SetAdvectionScaling( 1.0 );
  //  Software Guide : EndCodeSnippet

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

  geodesicActiveContour->SetMaximumRMSError( 0.005 );
  geodesicActiveContour->SetNumberOfIterations( 400 );

  //  Software Guide : BeginLatex
  //
  //  Each iteration, the current ``best-fit'' shape is estimated from the
  //  edge potential image and the current contour. To increase speed, only
  //  information within the sparse field layers of the current contour is used
  //  in the estimation. The default number of sparse field layers is
  //  the same as
  //  the ImageDimension which does not contain enough information to get
  //  a reliable best-fit shape estimate. Thus, we override the default and
  //  set the number of layers to 4.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  geodesicActiveContour->SetNumberOfLayers( 4 );
  //  Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filters are then connected in a pipeline as illustrated in
  //  Figure~\ref{fig:GeodesicActiveContourShapePriorCollaborationDiagram}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  center->SetInput( reader->GetOutput() );
  smoothing->SetInput( center->GetOutput() );
  gradientMagnitude->SetInput( smoothing->GetOutput() );
  reciprocal->SetInput( gradientMagnitude->GetOutput() );

  geodesicActiveContour->SetInput(  fastMarching->GetOutput() );
  geodesicActiveContour->SetFeatureImage( reciprocal->GetOutput() );

  thresholder->SetInput( geodesicActiveContour->GetOutput() );
  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  The CurvatureAnisotropicDiffusionImageFilter requires a couple of
  //  parameter to be defined. The following are typical values for $2D$
  //  images. However they may have to be adjusted depending on the amount of
  //  noise present in the input image. This filter has been discussed in
  //  section~\ref{sec:GradientAnisotropicDiffusionImageFilter}.

  smoothing->SetTimeStep( 0.125 );
  smoothing->SetNumberOfIterations(  5 );
  smoothing->SetConductanceParameter( 9.0 );


  //  The GradientMagnitudeRecursiveGaussianImageFilter performs the
  //  equivalent of a convolution with a Gaussian kernel, followed by a
  //  derivative operator. The sigma of this Gaussian can be used to control
  //  the range of influence of the image edges. This filter has been discussed
  //  in Section~\ref{sec:GradientMagnitudeRecursiveGaussianImageFilter}.

  const double sigma = atof( argv[10] );
  gradientMagnitude->SetSigma(  sigma  );


  //  The FastMarchingImageFilter requires the user to provide a seed
  //  point from which the level set will be generated. The user can actually
  //  pass not only one seed point but a set of them. Note that the
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


  //  Nodes are created as stack variables and initialized with a value and an
  //  \doxygen{Index} position. Note that here we assign the value of minus the
  //  user-provided distance to the unique node of the seeds passed to the
  //  FastMarchingImageFilter. In this way, the value will increment
  //  as the front is propagated, until it reaches the zero value corresponding
  //  to the contour. After this, the front will continue propagating until it
  //  fills up the entire image. The initial distance is taken here from the
  //  command line arguments. The rule of thumb for the user is to select this
  //  value as the distance from the seed points at which she want the initial
  //  contour to be.
  const double initialDistance = atof( argv[9] );

  NodeType node;

  const double seedValue = - initialDistance;

  node.SetValue( seedValue );
  node.SetIndex( seedPosition );


  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.

  seeds->Initialize();
  seeds->InsertElement( 0, node );

  seedPosition[0] = atoi( argv[5] );
  seedPosition[1] = atoi( argv[6] );
  node.SetIndex( seedPosition );
  seeds->InsertElement( 1, node );

  seedPosition[0] = atoi( argv[7] );
  seedPosition[1] = atoi( argv[8] );
  node.SetIndex( seedPosition );
  seeds->InsertElement( 2, node );


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


  //  Here we configure all the writers required to see the intermediate
  //  outputs of the pipeline. This is added here only for
  //  pedagogical/debugging purposes. These intermediate output are normaly not
  //  required. Only the output of the final thresholding filter should be
  //  relevant.  Observing intermediate output is helpful in the process of
  //  fine tuning the parameters of filters in the pipeline.
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
  writer1->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput1.png");
  caster1->SetOutputMinimum(   0 );
  caster1->SetOutputMaximum( 255 );
  writer1->Update();

  caster2->SetInput( gradientMagnitude->GetOutput() );
  writer2->SetInput( caster2->GetOutput() );
  writer2->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput2.png");
  caster2->SetOutputMinimum(   0 );
  caster2->SetOutputMaximum( 255 );
  writer2->Update();

  caster3->SetInput( reciprocal->GetOutput() );
  writer3->SetInput( caster3->GetOutput() );
  writer3->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput3.png");
  caster3->SetOutputMinimum(   0 );
  caster3->SetOutputMaximum( 255 );
  writer3->Update();

  caster4->SetInput( fastMarching->GetOutput() );
  writer4->SetInput( caster4->GetOutput() );
  writer4->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput4.png");
  caster4->SetOutputMinimum(   0 );
  caster4->SetOutputMaximum( 255 );


  //  The FastMarchingImageFilter requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputRegion()}. Note that the size is obtained here from the
  //  output image of the centering filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //
  fastMarching->SetOutputRegion(
           center->GetOutput()->GetBufferedRegion() );
  fastMarching->SetOutputSpacing(
           center->GetOutput()->GetSpacing() );
  fastMarching->SetOutputOrigin(
           center->GetOutput()->GetOrigin() );


  //  Software Guide : BeginLatex
  //
  //  Next, we define the shape model. In this example,
  //  we use an implicit shape model based on the principal components
  //  such that:
  //
  //  \begin{equation}
  //  \psi^{*}(\mathbf{x}) = \mu(\mathbf{x}) + \sum_k \alpha_k u_k(\mathbf{x})
  //  \end{equation}
  //
  //  where $\mu(\mathbf{x})$ is the mean signed distance computed from training
  //  set of segmented objects and $u_k(\mathbf{x})$ are the first $K$ principal
  //  components of the offset (signed distance - mean).
  //  The coefficients $\{\alpha_k\}$ form the
  //  set of \emph{shape} parameters.
  //
  //  Given a set of training data, the \doxygen{ImagePCAShapeModelEstimator}
  //  can be used to obtain
  //  the mean and principal mode shape images required by PCAShapeSignedDistanceFunction.
  //
  //  \index{itk::PCAShapeSignedDistanceFunction!New()}
  //  \index{itk::PCAShapeSignedDistanceFunction!SetNumberOfPrincipalComponents()}
  //
  //
  //  Software Guide : EndLatex

  const unsigned int numberOfPCAModes = atoi( argv[14] );

  // Software Guide : BeginCodeSnippet
  typedef itk::PCAShapeSignedDistanceFunction<
                              double,
                              Dimension,
                              InternalImageType >     ShapeFunctionType;

  ShapeFunctionType::Pointer shape = ShapeFunctionType::New();

  shape->SetNumberOfPrincipalComponents( numberOfPCAModes );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  In this example, we will read the mean shape and
  //  principal mode images from file. We will assume that
  //  the filenames of the mode images form a numeric series starting from index 0.
  //
  //  \index{itk::PCAShapeSignedDistanceFunction!SetMeanImage()}
  //  \index{itk::PCAShapeSignedDistanceFunction!SetPrincipalComponentsImages()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer meanShapeReader = ReaderType::New();
  meanShapeReader->SetFileName( argv[13] );
  meanShapeReader->Update();

  std::vector<InternalImageType::Pointer> shapeModeImages( numberOfPCAModes );

  itk::NumericSeriesFileNames::Pointer fileNamesCreator =
          itk::NumericSeriesFileNames::New();

  fileNamesCreator->SetStartIndex( 0 );
  fileNamesCreator->SetEndIndex( numberOfPCAModes - 1 );
  fileNamesCreator->SetSeriesFormat( argv[15] );
  const std::vector<std::string> & shapeModeFileNames =
          fileNamesCreator->GetFileNames();

  for (unsigned int k = 0; k < numberOfPCAModes; ++k )
    {
    ReaderType::Pointer shapeModeReader = ReaderType::New();
    shapeModeReader->SetFileName( shapeModeFileNames[k].c_str() );
    shapeModeReader->Update();
    shapeModeImages[k] = shapeModeReader->GetOutput();
    }

  shape->SetMeanImage( meanShapeReader->GetOutput() );
  shape->SetPrincipalComponentImages( shapeModeImages );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Further we assume that the shape modes have been normalized
  // by multiplying with the corresponding singular value. Hence,
  // we can set the principal component standard deviations to all
  // ones.
  //
  //  \index{itk::PCAShapeSignedDistanceFunction!Set\-Principal\-Component\-Standard\-Deviations()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ShapeFunctionType::ParametersType pcaStandardDeviations( numberOfPCAModes );
  pcaStandardDeviations.Fill( 1.0 );

  shape->SetPrincipalComponentStandardDeviations( pcaStandardDeviations );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next, we instantiate a \doxygen{Euler2DTransform} and connect it to the
  // PCASignedDistanceFunction. The transform represent
  // the pose of the shape. The parameters of the transform
  // forms the set of \emph{pose} parameters.
  //
  //  \index{itk::PCAShapeSignedDistanceFunction!SetTransform()}
  //  \index{itk::ShapeSignedDistanceFunction!SetTransform()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Euler2DTransform<double>    TransformType;
  TransformType::Pointer transform = TransformType::New();

  shape->SetTransform( transform );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Before updating the level set at each iteration, the parameters
  // of the current best-fit shape is estimated by minimizing the
  // \doxygen{ShapePriorMAPCostFunction}. The cost function is composed of
  // four terms: contour fit, image fit, shape prior and pose prior.
  // The user can specify the weights applied to each term.
  //
  //  \index{itk::ShapePriorMAPCostFunction!SetWeights()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ShapePriorMAPCostFunction<
                              InternalImageType,
                              InternalPixelType >     CostFunctionType;

  CostFunctionType::Pointer costFunction = CostFunctionType::New();

  CostFunctionType::WeightsType weights;
  weights[0] =  1.0;  // weight for contour fit term
  weights[1] =  20.0; // weight for image fit term
  weights[2] =  1.0;  // weight for shape prior term
  weights[3] =  1.0;  // weight for pose prior term

  costFunction->SetWeights( weights );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Contour fit measures the likelihood of seeing the current
  // evolving contour for a given set of shape/pose parameters.
  // This is computed by counting the number of pixels inside
  // the current contour but outside the current shape.
  //
  // Image fit measures the likelihood of seeing certain image
  // features for a given set of shape/pose parameters. This is
  // computed by assuming that ( 1 - edge potential ) approximates
  // a zero-mean, unit variance Gaussian along the normal of
  // the evolving contour. Image fit is then computed by computing
  // the Laplacian goodness of fit of the Gaussian:
  //
  // \begin{equation}
  // \sum \left( G(\psi(\mathbf{x})) - |1 - g(\mathbf{x})| \right)^2
  // \end{equation}
  //
  // where $G$ is a zero-mean, unit variance Gaussian and $g$ is
  // the edge potential feature image.
  //
  // The pose parameters are assumed to have a uniform distribution
  // and hence do not contribute to the cost function.
  // The shape parameters are assumed to have a Gaussian distribution.
  // The parameters of the distribution are user-specified. Since we
  // assumed the principal modes have already been normalized,
  // we set the distribution to zero mean and unit variance.
  //
  //  \index{itk::ShapePriorMAPCostFunction!SetShapeParameterMeans()}
  //  \index{itk::ShapePriorMAPCostFunction!SetShapeParameterStandardDeviations()}
  //
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CostFunctionType::ArrayType mean(   shape->GetNumberOfShapeParameters() );
  CostFunctionType::ArrayType stddev( shape->GetNumberOfShapeParameters() );

  mean.Fill( 0.0 );
  stddev.Fill( 1.0 );
  costFunction->SetShapeParameterMeans( mean );
  costFunction->SetShapeParameterStandardDeviations( stddev );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In this example, we will use the \doxygen{OnePlusOneEvolutionaryOptimizer}
  // to optimize the cost function.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::OnePlusOneEvolutionaryOptimizer    OptimizerType;
  OptimizerType::Pointer optimizer = OptimizerType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The evolutionary optimization algorithm is based on testing
  // random permutations of the parameters. As such, we need to provide
  // the optimizer with a random number generator. In the following lines,
  // we create a \doxygen{NormalVariateGenerator}, seed it, and
  // connect it to the optimizer.
  //
  //  \index{itk::Statistics::NormalVariateGenerator!Initialize()}
  //  \index{itk::OnePlusOneEvolutionaryOptimizer!SetNormalVariateGenerator()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::NormalVariateGenerator GeneratorType;
  GeneratorType::Pointer generator = GeneratorType::New();

  generator->Initialize( 20020702 );

  optimizer->SetNormalVariateGenerator( generator );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The cost function has $K+3$ parameters. The first $K$
  // parameters are the principal component multipliers, followed
  // by the 2D rotation parameter (in radians) and the x- and
  // y- translation parameters (in mm).  We need to carefully
  // scale the different types of parameters to compensate
  // for the differences in the dynamic ranges of the parameters.
  //
  //  \index{itk::OnePlusOneEvolutionaryOptimizer!SetScales()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  OptimizerType::ScalesType scales( shape->GetNumberOfParameters() );
  scales.Fill( 1.0 );
  for( unsigned int k = 0; k < numberOfPCAModes; k++ )
    {
    scales[k] = 20.0;  // scales for the pca mode multiplier
    }
  scales[numberOfPCAModes] = 350.0;  // scale for 2D rotation
  optimizer->SetScales( scales );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next, we specify the initial radius, the shrink and
  // grow mutation factors and termination criteria of the optimizer.
  // Since the best-fit shape is re-estimated each iteration of
  // the curve evolution, we do not need to spend too much time finding the true
  // minimizing solution each time; we only need to head towards it. As such,
  // we only require a small number of optimizer iterations.
  //
  //  \index{itk::OnePlusOneEvolutionaryOptimizer!Initialize()}
  //  \index{itk::OnePlusOneEvolutionaryOptimizer!SetEpsilon()}
  //  \index{itk::OnePlusOneEvolutionaryOptimizer!SetMaximumIteration()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double initRadius = 1.05;
  double grow = 1.1;
  double shrink = pow(grow, -0.25);
  optimizer->Initialize(initRadius, grow, shrink);

  optimizer->SetEpsilon(1.0e-6); // minimal search radius

  optimizer->SetMaximumIteration(15);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Before starting the segmentation process we need to also supply the initial
  // best-fit shape estimate. In this example, we start with the unrotated mean shape
  // with the initial x- and y- translation specified through command-line
  // arguments.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ShapeFunctionType::ParametersType parameters(
                                             shape->GetNumberOfParameters() );
  parameters.Fill( 0.0 );
  parameters[numberOfPCAModes + 1] = atof( argv[16] ); // startX
  parameters[numberOfPCAModes + 2] = atof( argv[17] ); // startY
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Finally, we connect all the components to the filter and add our
  // observer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  geodesicActiveContour->SetShapeFunction( shape );
  geodesicActiveContour->SetCostFunction( costFunction );
  geodesicActiveContour->SetOptimizer( optimizer );
  geodesicActiveContour->SetInitialParameters( parameters );

  typedef CommandIterationUpdate<GeodesicActiveContourFilterType> CommandType;
  CommandType::Pointer observer = CommandType::New();
  geodesicActiveContour->AddObserver( itk::IterationEvent(), observer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The invocation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  As usual, the call is placed in a
  //  \code{try/catch} block to handle exceptions should errors occur.
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
  std::cout << "Max. no. iterations: " << geodesicActiveContour->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << geodesicActiveContour->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << geodesicActiveContour->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << geodesicActiveContour->GetRMSChange() << std::endl;
  std::cout << "Parameters: " << geodesicActiveContour->GetCurrentParameters() << std::endl;

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
  mapWriter->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput4.mha");
  mapWriter->Update();

  InternalWriterType::Pointer speedWriter = InternalWriterType::New();
  speedWriter->SetInput( reciprocal->GetOutput() );
  speedWriter->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput3.mha");
  speedWriter->Update();

  InternalWriterType::Pointer gradientWriter = InternalWriterType::New();
  gradientWriter->SetInput( gradientMagnitude->GetOutput() );
  gradientWriter->SetFileName("GeodesicActiveContourShapePriorImageFilterOutput2.mha");
  gradientWriter->Update();

  // Also write out the initial and final best fit shape
  typedef itk::SpatialFunctionImageEvaluatorFilter<
                              ShapeFunctionType,
                              InternalImageType,
                              InternalImageType >  EvaluatorFilterType;

  EvaluatorFilterType::Pointer evaluator = EvaluatorFilterType::New();
  evaluator->SetInput( geodesicActiveContour->GetOutput() );
  evaluator->SetFunction( shape );
  shape->SetParameters( geodesicActiveContour->GetInitialParameters() );

  thresholder->SetInput( evaluator->GetOutput() );
  writer->SetFileName( "GeodesicActiveContourShapePriorImageFilterOutput5.png" );
  writer->Update();

  shape->SetParameters( geodesicActiveContour->GetCurrentParameters() );
  evaluator->Modified();
  writer->SetFileName( "GeodesicActiveContourShapePriorImageFilterOutput6.png" );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // Deviating from previous examples, we will demonstrate this example using
  // \code{BrainMidSagittalSlice.png}
  // (Figure~\ref{fig:GeodesicActiveContourShapePriorImageFilterOutput}, left)
  // from the \code{Examples/Data} directory.
  // The aim here is to segment the corpus callosum from the image using a shape model
  // defined by \code{CorpusCallosumMeanShape.mha} and the first three principal
  // components \code{CorpusCallosumMode0.mha}, \code{CorpusCallosumMode1.mha} and
  // \code{CorpusCallosumMode12.mha}. As shown in Figure~\ref{fig:CorpusCallosumPCAModes},
  // the first mode captures scaling, the second mode captures the shifting of mass between
  // the rostrum and the splenium and the third mode captures the degree of curvature.
  // Segmentation results with and without shape
  // guidance are shown in
  // Figure~\ref{fig:GeodesicActiveContourShapePriorImageFilterOutput2}.
  //
  //
  // \begin{figure} \center
  // \includegraphics[width=0.30\textwidth]{BrainMidSagittalSlice}
  // \includegraphics[width=0.30\textwidth]{GeodesicActiveContourShapePriorImageFilterOutput5}
  // \itkcaption[GeodesicActiveContourShapePriorImageFilter input image and initial model]{
  // The input image to the GeodesicActiveContourShapePriorLevelSetImageFilter is a
  // synthesized MR-T1 mid-sagittal slice ($217 \times 180$ pixels, $1 \times 1$ mm spacing)
  // of the brain (left) and the initial best-fit shape
  // (right) chosen to roughly overlap the corpus callosum in the image to be segmented.}
  //
  // \label{fig:GeodesicActiveContourShapePriorImageFilterOutput}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \begin{tabular}{cccc}
  // & $-3\sigma$ & mean & $+3\sigma$ \\ mode 0: &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumModeMinus0} &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumMeanShape} &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumModePlus0} \\ mode 1: &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumModeMinus1} &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumMeanShape} &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumModePlus1} \\ mode 2: &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumModeMinus2} &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumMeanShape} &
  // \includegraphics[width=0.10\textwidth]{CorpusCallosumModePlus2} \\ \end{tabular}
  // \itkcaption[Corpus callosum PCA modes]{First three PCA modes of a low-resolution
  // ($58 \times 31$ pixels, $2 \times 2$ mm spacing) corpus callosum model used in the
  // shape guided geodesic active contours example.}
  //
  // \label{fig:CorpusCallosumPCAModes}
  // \end{figure}
  //
  //
  //
  // A sigma value of $1.0$ was used to compute the image gradient and the
  // propagation and shape prior scaling are respectively set to $0.5$ and $0.02$.
  // An initial level set was created by placing one seed point in the
  // rostrum $(60,102)$, one in the splenium $(120, 85)$ and one
  // centrally in the body $(88,83)$ of the corpus callosum with
  // an initial radius of $6$ pixels at each seed position.
  // The best-fit shape was initially placed with a translation of
  // $(10,0)$mm so that it roughly overlapped
  // the corpus callosum in the image as shown in
  // Figure~\ref{fig:GeodesicActiveContourShapePriorImageFilterOutput} (right).
  //
  //
  // From Figure~\ref{fig:GeodesicActiveContourShapePriorImageFilterOutput2} it can be
  // observed that without
  // shape guidance (left), segmentation using geodesic active contour leaks in the
  // regions where the corpus callosum blends into the surrounding brain tissues. With
  // shape guidance (center), the segmentation is constrained by the global shape model
  // to prevent leaking.
  //
  // The final best-fit shape parameters after the segmentation process is:
  //
  // \begin{verbatim}
  // Parameters: [-0.384988, -0.578738, 0.557793, 0.275202, 16.9992, 4.73473]
  // \end{verbatim}
  //
  // and is shown in
  // Figure~\ref{fig:GeodesicActiveContourShapePriorImageFilterOutput2} (right). Note that a
  // $0.28$ radian ($15.8$ degree) rotation has been introduced to match the model to
  // the corpus callosum in the image. Additionally, a negative weight for the first
  // mode shrinks the size relative to the mean shape. A negative weight for the second mode
  // shifts the mass to splenium, and a positive weight for the third mode
  // increases the curvature. It can also be observed that the final segmentation is
  // a combination of the best-fit shape with additional local deformation. The combination
  // of both global and local shape allows the segmentation to capture fine details not represented
  // in the shape model.
  //
  //
  // \begin{figure} \center
  // \includegraphics[width=0.30\textwidth]{GeodesicActiveContourShapePriorImageFilterOutput1}
  // \includegraphics[width=0.30\textwidth]{GeodesicActiveContourShapePriorImageFilterOutput2}
  // \includegraphics[width=0.30\textwidth]{GeodesicActiveContourShapePriorImageFilterOutput6}
  // \itkcaption[GeodesicActiveContourShapePriorImageFilter segmentations]{Corpus callosum
  // segmentation using geodesic active contours without (left) and with (center) shape guidance.
  // The image on the right represents the best-fit shape at the end of the segmentation process.}
  //
  // \label{fig:GeodesicActiveContourShapePriorImageFilterOutput2}
  // \end{figure}
  //
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;

}
