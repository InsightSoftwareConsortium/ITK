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



int main( int argc, char **argv )
{


  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX seedY " << std::endl;
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
  //  defined. First, the factor $f$ that the defines how large the range of
  //  intensities will be. Small values of the multiplier will restrict the
  //  inclusion of pixels to those having very similar intensities to those in
  //  the current region. Larger values of the multiplier will relax the
  //  accepting condition and will result in more generous growth of the
  //  region. Too large values will make the region ingest neighbor regions in
  //  the image that may actually belong to separate anatomical structures.
  //
  //  \index{itk::ConfidenceConnectedImageFilter!SetMultiplier()}
  //
  //  Software Guide : EndLatex 

  const double sigma = atof( argv[5] );

  // Software Guide : BeginCodeSnippet
  gradientMagnitude->SetSigma(  sigma  );
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
  //  \index{itk::FastMarchingImageFilter!SetSeed()}
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



  NodeType node;

  const double seedValue = 0.0;
  
  node.SetValue( seedValue );
  node.SetIndex( seedPosition );




  //  Software Guide : BeginCodeSnippet
  seeds->Initialize();

  seeds->InsertElement( 0, node );
  //  Software Guide : EndCodeSnippet 




  
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
  // \includegraphics[width=4cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=4cm]{FastMarchingOutput1.eps}
  // \includegraphics[width=4cm]{FastMarchingOutput2.eps}
  // \includegraphics[width=4cm]{FastMarchingOutput3.eps}
  // \caption{Segmentation results of the ConfidenceConnected filter for various seed points.}
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




