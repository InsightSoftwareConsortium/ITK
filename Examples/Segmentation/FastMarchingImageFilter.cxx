/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FastMarchingImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// The following example illustrates the use of the
// \doxygen{FastMarchingImageFilter}. This filter implements a Level Set
// approach to segmentation. The object to be segmented is delimited by a
// contour which is represented here as the \emph{zero set} of a function. It
// can also be said that the contour is defined as an implicit function of the
// form $f(\bf{X})=0$.  The contour is initialized by the user and then is
// made evolve in order to make it fit to the form of an anatomical structure
// in the image. The evolution of the \emph{zero set} is simulated by changing
// the $f$ function under the control of a differential equation.  The terms in
// the equation basically represent a diffusion equation in which the speed
// term can be customized by the user. In the case of the
// \doxygen{FastMarchingImageFilter} the speed term is expected to be provided
// by the user in the form of an image. This image is typically computed as the
// negative exponential of the gradient magnitude. This choice makes that the
// propagation speed of the front will be very low close to hight gradients
// while it will move rather fast in low gradient areas. This arrangement will
// make the contour propagate until it reaches the edges of anatomical
// structures in the image and to slow down in front of those edges.  The
// output of this filter is a \emph{time-crossing map} that indicates for each
// pixel, how much time would take for the front to arrive to the pixel
// location.
//
// \begin{figure} \center
// \includegraphics[width=14cm]{FastMarchingCollaborationDiagram1.eps}
// \caption[FastMarchingImageFilter collaboration diagram]{Collaboration
// diagram of the FastMarchingImageFilter applied to a segmentation task.}
// \label{fig:FastMarchingCollaborationDiagram}
// \end{figure}
//
// The application of a threshold in the output image is then equivalent to
// taking a snapshot of the contour at a particular time during its evolution.
// It is to expect that the contour will take longer times trying to cross over
// the edges of a particular anatomical structure. This should result in large
// changes on the time-crossing map values close to the structure edges.
// Segmentation is performed with this filter by locating a time range in which
// the contour was contained for a long time in a region of the image space.
//
// Figure~\ref{fig:FastMarchingCollaborationDiagram} shows the major components
// involved in the application of the \doxygen{FastMarchingImageFilter} to a
// segmentation task. It involves a first stage of smoothing using the
// \doxygen{CurvatureAnisotropicDiffusionImageFilter}. The smoothed image is
// passed as the input for the \doxygen{FastMarchingImageFilter} and it is also
// used to produce the image potential by passing it to the
// \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and then to the
// \doxygen{ExpNegativeImageFilter}.  Finally the output of the
// \doxygen{FastMarchingImageFilter} is passed to a
// \doxygen{BinaryThresholdImageFilter} in order to produce a binary mask
// representing the segmented object.
//
// The code in the following example illustrates the typical setup of a
// pipeline for performing segmentation. First, the input image is smoothing
// using an edge-preserving filter. Then the magnitude of its gradient is
// computed and passed to a negative exponential filter. The result of the
// exponential filter is the image potential that will be used to affect the
// speed term of the differential equation. 
//
// Let's start by including the following headers. First we include the header
// of the \doxygen{CurvatureAnisotropicDiffusionImageFilter} that will be used
// for removing noise from the input image.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet

//  Software Guide : BeginLatex
//  
//  The headers of the \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and
//  \doxygen{ExpNegativeImageFilter} are included below. This two
//  filters combined will produce the image potential for regulating the speed
//  term in the differential equation describing the evolution of the level
//  set.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkExpNegativeImageFilter.h"
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//  
//  Of course, we will need the \doxygen{Image} class and the
//  \doxygen{FastMarchingImageFilter} class. Hence we include their headers.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkFastMarchingImageFilter.h"
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//  
//  The time-crossing map resulting from the \doxygen{FastMarchingImageFilter}
//  will be thresholded using the \doxygen{BinaryThresholdImageFilter}. We
//  include its header here.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkBinaryThresholdImageFilter.h"
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//  
//  Reading and writing images will be done with the \doxygen{FileImageReader}
//  and \doxygen{FileImageWriter}.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet


// 
//  The RescaleIntensityImageFilter is used to renormailize the output 
//  of filters before sending them to files. 
// 
#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char **argv )
{


  if( argc < 8 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY";
    std::cerr << " Sigma ExponentialFactor TimeThreshold StoppingValue" << std::endl;
    return 1;
    }




  //  Software Guide : BeginLatex
  //  
  //  We declare now the image type using a pixel type and a particular
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
  //  The output image, on the other hand, is selected to be binary.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned char OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The type of the \doxygen{BinaryThresholdImageFilter} filter is
  //  instantiated below using the internal image type and the output image
  //  type.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryThresholdImageFilter< 
                        InternalImageType, 
                        OutputImageType    >    ThresholdingFilterType;
  
  ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();
  // Software Guide : EndCodeSnippet
                        



  //  Software Guide : BeginLatex
  //
  //  The upper threshold passed to the \doxygen{BinaryThresholdImageFilter}
  //  will define the time snap-shot that we are taking from the time-crossing
  //  map. In an ideal application the user should be able to select this
  //  threshold interactively using visual feedback. Here, since it is a
  //  minimal example, the value is taken from the command line arguments.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  thresholder->SetLowerThreshold( 0.0 );
  thresholder->SetUpperThreshold( atof( argv[7] ) );

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



  //
  //  The RescaleIntensityImageFilter type is declared below. This filter will
  //  renormalize image before sending them to writers.
  //
  typedef itk::RescaleIntensityImageFilter< 
                               InternalImageType, 
                               OutputImageType >   CastFilterType;

  //  Software Guide : BeginLatex
  //  
  //  
  //  The \doxygen{CurvatureAnisotropicDiffusionImageFilter} type is
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
  //  The types of the \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and
  //  \doxygen{ExpNegativeImageFilter} are instantiated using the internal
  //  image type. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   itk::GradientMagnitudeRecursiveGaussianImageFilter< 
                               InternalImageType, 
                               InternalImageType >  GradientFilterType;

  typedef   itk::ExpNegativeImageFilter<                               
                               InternalImageType, 
                               InternalImageType >  ExpNegativeFilterType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The corresponding filter objects are created with the method
  //  \code{New()}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  GradientFilterType::Pointer  gradientMagnitude = GradientFilterType::New();

  ExpNegativeFilterType::Pointer negativeExponential = ExpNegativeFilterType::New();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  We declare now the type of the
  //  \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and the
  //  \doxygen{ExpNegativeImageFilter} that will generate the image
  //  potential required for the \doxygen{FastMarchingImageFilter}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::FastMarchingImageFilter< 
                              InternalImageType, 
                              InternalImageType >    FastMarchingFilterType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  then, we  construct one filter of this class using the \code{New()} method. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:FastMarchingCollaborationDiagram} using the following
  //  lines. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );

  gradientMagnitude->SetInput( smoothing->GetOutput() );

  negativeExponential->SetInput( gradientMagnitude->GetOutput() );

  fastMarching->SetSpeedImage( negativeExponential->GetOutput() );
  fastMarching->SetInput( smoothing->GetOutput() );
  
  thresholder->SetInput( fastMarching->GetOutput() );

  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The \doxygen{CurvatureAnisotropicDiffusionImageFilter} requires a couple
  //  of parameter to be defined. The following are typical values for $2D$
  //  images. However they may have to be adjusted depending on the amount of
  //  noise present in the input image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  smoothing->SetTimeStep( 0.25 );
  smoothing->SetIterations( 5 );
  smoothing->SetConductanceParameter( 3.0 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} performs the
  //  equivalent of a convolution with a Gaussian kernel, followed by a
  //  derivative operator. The sigma of this Gaussian can be used to control
  //  the range of influence of the image edges.
  //
  //  \index{itk::GradientMagnitudeRecursiveGaussianImageFilter!SetSigma()}
  //
  //  Software Guide : EndLatex 

  const double sigma = atof( argv[5] );

  // Software Guide : BeginCodeSnippet
  gradientMagnitude->SetSigma(  sigma  );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The \doxygen{ExpNegativeImageFilter} requires the exponential factor as a
  //  parameter. This value will multiply all the input pixel intensities
  //  before their exponential is computed. The factor is passed using the
  //  \code{SetFactor()} method. In the context of this example, the factor can
  //  be used to intensify the differences between regions of low and high
  //  values in the speed image. In an ideal case, the speed value should be
  //  $1.0$ in the homogeneous regions of anatomical structures and the value
  //  should decay rapidly to $0.0$ around the edges of structures.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  negativeExponential->SetFactor( atof( argv[6] ) );
  // Software Guide : EndCodeSnippet



  
  //  Software Guide : BeginLatex
  //
  //  The \doxygen{FastMarchingImageFilter} requires the user to provide a seed
  //  point from which the level set will be generated. The user can actually
  //  pass not only one seed point but a set of them. A good set of seed points
  //  increase the changes of segmenting a complex object without missing
  //  parts. The seeds are passed stored in a container. The type of this
  //  container is defined as \code{NodeContainer} among the
  //  \doxygen{FastMarchingImageFilter} traits.
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
  //  index position.
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
  //  The set of seed nodes is passed now to the
  //  \doxygen{FastMarchingImageFilter} with the method
  //  \code{SetTrialPoints()}.
  //
  //  \index{itk::FastMarchingImageFilter!SetTrialPoints()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fastMarching->SetTrialPoints(  seeds  );
  // Software Guide : EndCodeSnippet




  //
  //  Here we configure all the writers required to see the intermediate
  //  outputs of the pipeline. This is added here only for
  //  pedagogical/debugging purposes. These intermediate output are normaly not
  //  required. Only the output of the final thresholding filter should be
  //  relevant.  Observing intermediate output is helpful in the process of
  //  fine tunning the parameters of filters in the pipeline. 
  //
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
  writer1->SetFileName("FastMarchingFilterOutput1.png");
  caster1->SetOutputMinimum(   0 );
  caster1->SetOutputMaximum( 255 );
  writer1->Update();

  caster2->SetInput( gradientMagnitude->GetOutput() );
  writer2->SetInput( caster2->GetOutput() );
  writer2->SetFileName("FastMarchingFilterOutput2.png");
  caster2->SetOutputMinimum(   0 );
  caster2->SetOutputMaximum( 255 );
  writer2->Update();

  caster3->SetInput( negativeExponential->GetOutput() );
  writer3->SetInput( caster3->GetOutput() );
  writer3->SetFileName("FastMarchingFilterOutput3.png");
  caster3->SetOutputMinimum(   0 );
  caster3->SetOutputMaximum( 255 );
  writer3->Update();

  caster4->SetInput( fastMarching->GetOutput() );
  writer4->SetInput( caster4->GetOutput() );
  writer4->SetFileName("FastMarchingFilterOutput4.png");
  caster4->SetOutputMinimum(   0 );
  caster4->SetOutputMaximum( 255 );
  



  //  Software Guide : BeginLatex
  //
  //  The \doxygen{FastMarchingImageFilter} requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fastMarching->SetOutputSize( 
           reader->GetOutput()->GetBufferedRegion().GetSize() );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  Since the front representing the contour will propagate continuosly over
  //  time, it is desirable to stop the process once a certain time has been
  //  reached. This allows to save computation time under the assumption that
  //  the region of interest has already been computed. The value for stopping
  //  the process is defined with the method \code{SetStoppingValue()}. In
  //  principle the stopping value should be a little bit higher than the
  //  threshold value.
  //
  //  \index{itk::FastMarchingImageFilter!SetStoppingValue()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fastMarching->SetStoppingValue( atof( argv[8] ) );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The invokation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  As usual, the call is placed in a
  //  \code{try/catch} block should any errors ocurr and exceptions are thrown.
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



  writer4->Update();




  //
  // The following writer type is used to save the output of the time-crossing
  // map in a file with apropiate pixel representation. The advantage of saving
  // this image in native format is that it can be used with a viewer to help
  // determine an appropriate threshold to be used on the output of the
  // fastmarching filter.
  //
  typedef itk::ImageFileWriter< InternalImageType > InternalWriterType;

  InternalWriterType::Pointer mapWriter = InternalWriterType::New();
  mapWriter->SetInput( fastMarching->GetOutput() );
  mapWriter->SetFileName("FastMarchingFilterOutput4.mha");
  mapWriter->Update();

  InternalWriterType::Pointer speedWriter = InternalWriterType::New();
  speedWriter->SetInput( negativeExponential->GetOutput() );
  speedWriter->SetFileName("FastMarchingFilterOutput3.mha");
  speedWriter->Update();





  //  Software Guide : BeginLatex
  //
  //  Let's now run this example using as input the image
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Insight/Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations. For example
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|}
  //  \hline
  //  Structure & Seed Index & Output Image \\
  //  \hline
  //  White matter & $(60,116)$ & Second from left in Figure \ref{fig:ConfidenceConnectedOutput} \\ 
  //  Ventricle    & $(81,112)$ & Third  from left in Figure \ref{fig:ConfidenceConnectedOutput} \\ 
  //  Gray matter  & $(107,69)$ & Fourth from left in Figure \ref{fig:ConfidenceConnectedOutput} \\ 
  //  \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=5cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=5cm]{FastMarchingImageFilterOutput1.eps}
  // \includegraphics[width=5cm]{FastMarchingImageFilterOutput2.eps}
  // \includegraphics[width=5cm]{FastMarchingImageFilterOutput3.eps}
  // \caption[FastMarchingImageFilter output images]{Images generated by the
  // segmentation process based on the FastMarchingImageFilter}
  // \label{fig:FastMarchingImageFilterOutput}
  // \end{figure}
  //
  //  It can be noticed that the gray matter is not being completly segmented.
  //  This illustrates the vulnerability of the region growing methods when the
  //  anatomical structures to be segmented do not have a homogeneous
  //  statistical distribution over the image space. You may want to experiment
  //  with differnt numbers of iterations to verify how the accepted region
  //  will extend.
  //
  //  Software Guide : EndLatex 




  return 0;

}




