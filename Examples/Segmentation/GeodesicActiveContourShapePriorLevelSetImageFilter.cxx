/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GeodesicActiveContourShapePriorLevelSetImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

 =========================================================================*/

// Software Guide : BeginLatex
//
// In medical imaging applications, the general shape, location and 
// orientation of a anatomical structure of interest is sometimes
// known \emph{a priori}. This information can be used to aid the
// segmentation process especially when image contrast is low or
// when the object boundary is not distinct.
//
// In \cite{Leventon2000}, Leventon \emph{et al.} extended the edge-based
// geodesic active contours method with an additional shape-influenced term in
// the driving PDE. The \doxygen{GeodesicActiveContourShapePriorLevelSetFilter}
// is a generalization of Leventon's approach and its use is illustrated 
// in the following example.
//
// To support shape-guidance, the generic level set 
// equation~\ref{eqn:LevelSetEquation}is extended to incorporate a shape-influence 
// term:
//
// \begin{equation}
// \label{eqn:ShapeInfluenceTerm}
// \xi \left(\psi^{*}(\mathbf{x}) - \psi(\mathbf{x})\right)
// \end{equation}
//
// where $\psi^{*}$ is the signed distance function of the ``best-fit'' shape
// with respect to the shape model. The new term has the effect of driving the
// contour towards the best-fit shape and scalar $\xi$ weights the influence
// of the shape term in the overall evolution. In general, the best-fit shape
// is not known and has to be iteratively estimated in conjunction with the
// contour evolution.
//
// As with the GeodesicActiveContourLevelSetImageFilter, the 
// GeodesicActiveContourShapePriorLevelSetImageFilter expects two input
// images: the first is an initial level set and the second a feature image
// that represents the image edge potential. The configuration of this
// example is quite similar to the example on the use of the
// GeodesicActiveContourLevelSetImageFilter and hence the description will focus
// on the new objects involved in the segmentation process as shown
// in Figure~\ref{fig:GeodesicActiveContourShapePriorCollaborationDiagram}.
//
// \begin{figure} \center
// %\includegraphics[width=\textwidth]{GeodesicActiveContourShapePriorCollaborationDiagram1.eps}
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
// For simplicity, this example using then uses the \doxygen{BoundedReciprocalImageFilter}
// to produce the edge potential image.
//
// The \doxygen{FastMarchingImageFilter} creates an initial level set using a
// user specified seed position and initial contour radius which then passed
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
// prinicpal components modes and the \doxygen{Euler2DTransform} is used
// to represent the pose of the shape. In this implementation, the
// best-fit shape estimation problem is reformulated as minimization problem
// where the \doxygen{ShapePriorMAPCostFunction} is the cost function to
// to optimized. In this example, 
// we will use the \doxygen{OnePlusOneEvolutionaryOptimizer} to perform
// the minimization.
//
// It should be noted that, although a particular shape model, transform
// cost function and optimizer is used in this example, the implementation
// is generic allowing different instances of these components to be
// plugged in. This flexible allows a user to talior the behavoir of the
// segmentation to suit the circumstances of a targeted application. 
//  
// Let's start the example by including the headers of the new filters 
// involved in the segmentation. 
//
// Software Guide : EndLatex 

#include "itkImage.h"

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
#include "itkShapePriorMAPCostFunction.h"
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
// Given the numerous parameters involved in tuning this segmentation method for
// a particular application, it is not uncommon for a segmentation process to
// run for several minutes and still produce a useless result.  To avoid
// this situation it is quite helpful to track the evolution of the
// segmentation as it progresses. The following defines a 
// custom \doxygen{Command} class
// for monitoring the RMS change and shape parameters each iteration.
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

  void Execute(itk::Object *caller, const itk::EventObject & event)
    { 
      Execute( (const itk::Object *) caller, event); 
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
      const TFilter * filter =
                  dynamic_cast< const TFilter * >( object );
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
  if( argc < 14 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage";
    std::cerr << " seedX seedY initialDistance";
    std::cerr << " sigma";
    std::cerr << " propagationScaling shapePriorScaling";
    std::cerr << " meanShapeImage numberOfModes shapeModeFilePattern";
    std::cerr << " startX startY" << std::endl;
    return 1;
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
  typedef unsigned char OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::BinaryThresholdImageFilter< 
                        InternalImageType, 
                        OutputImageType    >    ThresholdingFilterType;
  
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
  //  GeodesicActiveContourShapePriorLevelSetImageFilter using the \code{New()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::GeodesicActiveContourShapePriorLevelSetImageFilter< 
                              InternalImageType, 
                              InternalImageType >   GeodesicActiveContourFilterType;
  GeodesicActiveContourFilterType::Pointer geodesicActiveContour = 
                                     GeodesicActiveContourFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  // The ChangeInformationImageFilter is the first filter in the preprocessing
  // stage and is use to force the image origin to the center of the image.
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
  // the image gradient as the edge potential feature image.
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
  //  For the GeodesicActiveContourShapePriorLevelSetImageFilter, scaling parameters
  //  are used to trade off between the propagation (inflation), the
  //  curvature (smoothing), the advection terms and the shape influence term. 
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

  const double propagationScaling = atof( argv[7] );
  const double shapePriorScaling  = atof( argv[8] );

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

  geodesicActiveContour->SetMaximumRMSError( 0.008 );
  geodesicActiveContour->SetNumberOfIterations( 1000 );

  //  Software Guide : BeginLatex
  //
  //  Each iteration the current ``best-fit'' shape is estimated from the
  //  edge potential image and the current contour. To increase speed, only 
  //  information within the sparse field layers of the current contour is used
  //  in the estimation. The default number of sparse field layers is same as
  //  the ImageDimension which does not contain enough information to get 
  //  a reliable best-fit shape estimation. Thus, we override the default and
  //  set the number of layers to 8.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  geodesicActiveContour->SetNumberOfLayers( 8 );
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
  //  in Section~\ref{sec:GradientMagnitudeRecursiveGaussianImageFilter}

  const double sigma = atof( argv[6] );
  gradientMagnitude->SetSigma(  sigma  );
  

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
  const double initialDistance = atof( argv[5] );

  NodeType node;

  const double seedValue = - initialDistance;
  
  node.SetValue( seedValue );
  node.SetIndex( seedPosition );


  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.

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
  //  Next, we define the statistical shape model. In this example, 
  //  we use an implicit shape model based the principcal components 
  //  such that:
  //
  //  \begin{equation}
  //  \psi^{*}(\mathbf{x}) = \mu(\mathbf{x}) + \sum_k \alpha_k u_k(\mathbf{x})
  //  \end{equation}
  //
  //  where $\mu(\mathbf{x})$ is the mean signed distance computed from training
  //  data and $u_k(\mathbf{x})$ are the first $K$ principal components of the
  //  offset (signed distance - mean). The coefficient $\alpha_k$ form the
  //  \emph{shape} parameters. 
  //
  //  \index{itk::PCAShapeSignedDistanceFunction!New()}
  //  \index{itk::PCAShapeSignedDistanceFunction!SetNumberOfPrincipalComponents()}
  //
  //
  //  Software Guide : EndLatex

  const unsigned int numberOfPCAModes = atoi( argv[10] );

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
  //  principal mode signed distance images from file. We will assume that 
  //  the filenames of the mode images form a numeric series starting from index 0. 
  //
  //  \index{itk::PCAShapeSignedDistanceFunction!SetMeanImage()}
  //  \index{itk::PCAShapeSignedDistanceFunction!SetPrincipalComponentsImages()}
  // 
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer meanShapeReader = ReaderType::New();
  meanShapeReader->SetFileName( argv[9] );
  meanShapeReader->Update();

  std::vector<InternalImageType::Pointer> shapeModeImages( numberOfPCAModes );

  itk::NumericSeriesFileNames::Pointer fileNamesCreator = 
          itk::NumericSeriesFileNames::New();

  fileNamesCreator->SetStartIndex( 0 );
  fileNamesCreator->SetEndIndex( numberOfPCAModes - 1 );
  fileNamesCreator->SetSeriesFormat( argv[11] );
  const std::vector<std::string> & shapeModeFileNames = 
          fileNamesCreator->GetFileNames();

  for ( unsigned int k = 0; k < numberOfPCAModes; k++ )
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
  // Further we assume that the shape mode have been normalized 
  // by multiplying with the corresponding singular value. Hence
  // will can set the principal component standard deviations to all
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
  // Next we instantiated a Euler2DTransform and connect it to the
  // PCASignedDistanceFunction. The transform represent 
  // the pose of the shape. The parameters of the transform
  // forms the \emph{pose} parameters.
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

  // set up the cost function
  typedef itk::ShapePriorMAPCostFunction<
                              InternalImageType,
                              InternalPixelType >     CostFunctionType;

  CostFunctionType::Pointer costFunction = CostFunctionType::New();

  CostFunctionType::ArrayType mean(   shape->GetNumberOfShapeParameters() );
  CostFunctionType::ArrayType stddev( shape->GetNumberOfShapeParameters() );

  mean.Fill( 0.0 );
  stddev.Fill( 1.0 );
  costFunction->SetShapeParameterMeans( mean );
  costFunction->SetShapeParameterStandardDeviations( stddev );

  CostFunctionType::WeightsType weights;
  weights[0] =  1.0;  // weight for contour fit term
  weights[1] = 20.0;  // weight for image fit term
  weights[2] =  1.0;  // weight for shape prior term
  weights[3] =  1.0;  // weight for pose prior term

  costFunction->SetWeights( weights );

  // set up the optimizer
  typedef itk::OnePlusOneEvolutionaryOptimizer    OptimizerType;
  OptimizerType::Pointer optimizer = OptimizerType::New();

  OptimizerType::ScalesType scales( shape->GetNumberOfParameters() );
  scales.Fill( 1.0 );
  for( unsigned int k = 0; k < numberOfPCAModes; k++ )
    {
    scales[k] = 20.0;  // scales for the component multiplier
    }
  scales[numberOfPCAModes] = 100.0;  // scale for 2D rotation
  optimizer->SetScales( scales );

  // set up random number generator
  typedef itk::Statistics::NormalVariateGenerator GeneratorType;
  GeneratorType::Pointer generator = GeneratorType::New() ;
  generator->Initialize( 20020702 ) ;
  optimizer->SetNormalVariateGenerator( generator ) ;

  // initialization
  double initRadius = 1.05;
  double grow = 1.1 ;
  double shrink = pow(grow, -0.25) ;
  optimizer->Initialize(initRadius, grow, shrink) ;

  // minimal search radius
  optimizer->SetEpsilon(1.0e-6) ;

  // max iteration
  optimizer->SetMaximumIteration(15) ;

  // Set up the initial parameters
  ShapeFunctionType::ParametersType parameters( shape->GetNumberOfParameters() );
  parameters.Fill( 0.0 );
  parameters[numberOfPCAModes + 1] = atof( argv[12] ); // startX
  parameters[numberOfPCAModes + 2] = atof( argv[13] ); // startY

  // connect all the components up to the filter
  geodesicActiveContour->SetShapeFunction( shape );
  geodesicActiveContour->SetCostFunction( costFunction );
  geodesicActiveContour->SetOptimizer( optimizer );
  geodesicActiveContour->SetInitialParameters( parameters );

  //  Plug an observer onto the filter
  typedef CommandIterationUpdate<GeodesicActiveContourFilterType> CommandType;
  CommandType::Pointer observer = CommandType::New();
  geodesicActiveContour->AddObserver( itk::IterationEvent(), observer );
  
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


  return 0;
}




