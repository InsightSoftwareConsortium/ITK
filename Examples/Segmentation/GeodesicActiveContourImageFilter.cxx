/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    GeodesicActiveContourImageFilter.cxx
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
// \doxygen{GeodesicActiveContourImageFilter}.  The implementation of this
// filter in ITK is based on the paper by Caselles \cite{Caselles1997}.  This
// filter expects three inputs. The first is an initial Level Set in the form
// of an \doxygen{Image}. The second input is an edge potential image which
// basically follows the same rules used for the
// \doxygen{ShapeDetectionLevelSetFilter} discussed in
// section~\ref{sec:ShapeDetectionLevelSetFilter}. The third input is the
// gradient image of the edge potential map. This third image is used to
// improve the convergence of the zero set towards the edges of the eventual
// structures present in the image. This filter also introduce an additional
// factor on the forces driving the contours evolution. This new factor is an
// inflation force that produce an effect similar to blowing up a ballon inside
// the contour.
//
// The configuration of this example is quite similar to the example on the use
// of the \doxygen{ShapeDetectionLevelSetFilter} in
// section~\ref{sec:ShapeDetectionLevelSetFilter}. We omit most of the
// redundant description. A look at the code will reveal the large similarity
// between both examples.
//
// \begin{figure} \center
// \includegraphics[width=15cm]{GeodesicActiveContoursCollaborationDiagram1.eps}
// \caption[GeodesicActiveContourImageFilter collaboration
// diagram]{Collaboration diagram of the GeodesicActiveContourImageFilter
// applied to a segmentation task. Note the addition of the gradient filter
// computing the derivative of the edge potential map.}
// \label{fig:GeodesicActiveContoursCollaborationDiagram}
// \end{figure}
//
// Figure~\ref{fig:GeodesicActiveContoursCollaborationDiagram} shows the major
// components involved in the application of the
// \doxygen{GeodesicActiveContourImageFilter} to a segmentation task. This
// pipeline is quite similar to the one used by the
// \doxygen{ShapeDetectionLevelSetFilter} in
// section~\ref{sec:ShapeDetectionLevelSetFilter}. The most relevant change
// here is the introduction of the
// \doxygen{GradientRecursiveGaussianImageFilter} for computing the derivatives
// of the edge potential map and passing it as input to the
// \doxygen{GeodesicActiveContourImageFilter}.  
//
// The pipeline involves a first stage of smoothing using the
// \doxygen{CurvatureAnisotropicDiffusionImageFilter}. The smoothed image is
// passed as the input for the
// \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and then to the
// \doxygen{SigmoidImageFilter} in order to produce the Edge Potential image.
// A set of user-provided seeds are passed to a
// \doxygen{FastMarchingImageFilter} in order to compute their distance map. A
// constant value is subtracted from this map in order to obtain a Level Set in
// which the \emph{Zero Set} represents the initial contour. This level set is
// also passed as input to the \doxygen{GeodesicActiveContourImageFilter}.
// 
// Finally the LevelSet at the output of the
// \doxygen{GeodesicActiveContourImageFilter} is passed to a
// \doxygen{BinaryThresholdImageFilter} in order to produce a binary mask
// representing the segmented object.
//
// Let's start by including the headers of the main filters involved in the
// preprocessing. 
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkGeodesicActiveContourImageFilter.h"
// Software Guide : EndCodeSnippet




#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"



//  Software Guide : BeginLatex
//  
//  One of the main differences between the
//  \doxygen{GeodesicActiveContourImageFilter} and the
//  \doxygen{ShapDetectionImageFilter} is that the the former uses the
//  derivatives of the edge image map in order to improve convergence of the
//  contour. It is then necessary to compute such derivatives and pass them as
//  input to the filter. In this example we use the
//  \doxygen{GradientRecursiveGaussianImageFilter} for computing the
//  derivatives of the edge potential.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
//  Software Guide : EndCodeSnippet 








int main( int argc, char **argv )
{


  if( argc < 11 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage";
    std::cerr << " seedX seedY InitialDistance";
    std::cerr << " Sigma SigmoidAlpha SigmoidBeta Iterations";
    std::cerr << " InflationStrength"  << std::endl;
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
  //  The \doxygen{GeodesicActiveContourImageFilter} requires as input the
  //  derivative of the Edge Potential map. We declare in the following lines
  //  the type of the derivative image.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::CovariantVector< float, Dimension >  DerivativePixelType;

  typedef itk::Image< DerivativePixelType, Dimension > DerivativeImageType;  
  //  Software Guide : EndCodeSnippet 


  //  Software Guide : BeginLatex
  //  
  //  With this derivative image type we can instantiate the type of a
  //  \doxygen{GradientRecursiveGaussianImageFilter} that will compute the
  //  gradient of the edge map image.
  //
  //  Software Guide : EndLatex 
  
  //  Software Guide : BeginCodeSnippet
  typedef itk::GradientRecursiveGaussianImageFilter< 
                                     InternalImageType, 
                                     DerivativeImageType >  DerivativeFilterType;
  //  Software Guide : EndCodeSnippet 



                                     

  //  
  //  The following lines instantiate the tresholding filter that will
  //  process the final level set at the output of the GeodesicActiveContourImageFilter.                                    
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



  //  
  // We instantiate reader and writer types in the following lines.
  //

  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;


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

  //  
  //  The \doxygen{CurvatureAnisotropicDiffusionImageFilter} type is
  //  instantiated using the internal image type. 
  //
  typedef   itk::CurvatureAnisotropicDiffusionImageFilter< 
                               InternalImageType, 
                               InternalImageType >  SmoothingFilterType;

  SmoothingFilterType::Pointer smoothing = SmoothingFilterType::New();


  //  
  //  The types of the \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} and
  //  \doxygen{SigmoidImageFilter} are instantiated using the internal
  //  image type. 
  //
  typedef   itk::GradientMagnitudeRecursiveGaussianImageFilter< 
                               InternalImageType, 
                               InternalImageType >  GradientFilterType;

  typedef   itk::SigmoidImageFilter<                               
                               InternalImageType, 
                               InternalImageType >  SigmoidFilterType;

  GradientFilterType::Pointer  gradientMagnitude = GradientFilterType::New();

  SigmoidFilterType::Pointer sigmoid = SigmoidFilterType::New();


  //
  //  The minimum and maximum values of the \doxygen{SigmoidImageFilter} output
  //  are defined with the methods \code{SetOutputMinimum()} and
  //  \code{SetOutputMaximum()}. In our case, we want these two values to be
  //  $0.0$ and $1.0$ respectively in order to get a nice speed image to feed
  //  the \code{FastMarchingImageFilter}. Additional details on the user of the
  //  \doxygen{SigmoidImageFilter} are presented in
  //  section~\ref{sec:IntensityNonLinearMapping}.
  //

  sigmoid->SetOutputMinimum(  0.0  );
  sigmoid->SetOutputMaximum(  1.0  );




  //  
  //  We declare now the type of the \doxygen{FastMarchingImageFilter} that
  //  will be used to generate the initial level set in the form of a distance
  //  map.
  //
  typedef  itk::FastMarchingImageFilter< 
                              InternalImageType, 
                              InternalImageType >    FastMarchingFilterType;


  //  
  //  then, we  construct one filter of this class using the \code{New()} method. 
  //
  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();





  //  Software Guide : BeginLatex
  //  
  //  It is now time for creating the derivative filter that will compute the
  //  gradient of the edge potential image. Note that this filter is different
  //  from the \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} used as
  //  intermediate step in the computation of the actual edge potential. The
  //  current filter produce as output an image of \doxygen{CovariantVector}s.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  DerivativeFilterType::Pointer derivativeFilter  = DerivativeFilterType::New();
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //  
  //  We connect the edge potential image as input of the derivative filter.
  //  The output will be passed later to the
  //  \doxygen{GeodesicActiveContourImageFilter}. We also define the sigma to
  //  be used in the Gaussian derivative operator. we do not need to blurr much
  //  this image since it has already been blurred when the gradient magnitude
  //  was computed.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  derivativeFilter->SetInput( sigmoid->GetOutput() ); 
  
  derivativeFilter->SetSigma( 1.0 );
  //  Software Guide : EndCodeSnippet 




  
  //  Software Guide : BeginLatex
  //  
  //  In the following lines we instantiate the type of the
  //  \doxygen{GeodesicActiveContourImageFilter} and create an object of this type
  //  using the \code{New()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef  itk::GeodesicActiveContourImageFilter< 
                              InternalImageType, 
                              InternalImageType,
                              DerivativeImageType
                                       >    GeodesicActiveContourFilterType;

  GeodesicActiveContourFilterType::Pointer geodesicActiveContour = GeodesicActiveContourFilterType::New();                              
  // Software Guide : EndCodeSnippet


  
  //  Software Guide : BeginLatex
  //  
  //  The differential equation used by the
  //  \doxygen{GeodesicActiveContourImageFilter} includes an inflation term.
  //  This value drives a ballon-like force that help expand the contour. The
  //  value of this factor is passed to the filter with the
  //  \code{SetInflationStrength()} method. In our current example, we take
  //  this value from the command line arguments.
  //
  //  \index{GeodesicActiveContourImageFilter!SetInflationStrength()}
  //
  //  Software Guide : EndLatex 

  const double inflationStrength = atof( argv[10] );

  //  Software Guide : BeginCodeSnippet
  geodesicActiveContour->SetInflationStrength( inflationStrength ); 
  //  Software Guide : EndCodeSnippet 


  //  Software Guide : BeginLatex
  //  
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:GeodesicActiveContoursCollaborationDiagram} using the following
  //  lines. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );

  gradientMagnitude->SetInput( smoothing->GetOutput() );

  sigmoid->SetInput( gradientMagnitude->GetOutput() );

  geodesicActiveContour->SetInput(           fastMarching->GetOutput()     );
  geodesicActiveContour->SetEdgeImage(       sigmoid->GetOutput()          );
  geodesicActiveContour->SetDerivativeImage( derivativeFilter->GetOutput() );

  thresholder->SetInput( geodesicActiveContour->GetOutput() );

  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet




  //
  //  The \doxygen{CurvatureAnisotropicDiffusionImageFilter} requires a couple
  //  of parameter to be defined. The following are typical values for $2D$
  //  images. However they may have to be adjusted depending on the amount of
  //  noise present in the input image. This filter has been discussed in
  //  section~\ref{sec:GradientAnisotropicDiffusionImageFilter}.
  //
  
  smoothing->SetTimeStep( 0.25 );
  smoothing->SetIterations(  5 );
  smoothing->SetConductanceParameter( 3.0 );




  //
  //  The \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} performs the
  //  equivalent of a convolution with a Gaussian kernel, followed by a
  //  derivative operator. The sigma of this Gaussian can be used to control
  //  the range of influence of the image edges. This filter has been discussed
  //  in section~\ref{sec:GradientMagnitudeRecursiveGaussianImageFilter}
  //

  const double sigma = atof( argv[6] );

  gradientMagnitude->SetSigma(  sigma  );



  //
  //  The \doxygen{SigmoidImageFilter} requires two parameters that define the
  //  linear transformation to be applied to the sigmoid argument. This
  //  parameters have been discussed in sections~\ref{sec:SigmoidImageFilter}
  //  and \ref{sec:FastMarchingImageFilter}.
  //

  const double alpha =  atof( argv[7] );
  const double beta  =  atof( argv[8] );


  sigmoid->SetAlpha( alpha );
  sigmoid->SetBeta(  beta  );



  
  //
  //  The \doxygen{FastMarchingImageFilter} requires the user to provide a seed
  //  point from which the level set will be generated. The user can actually
  //  pass not only one seed point but a set of them. Note the the
  //  \doxygen{FastMarchingImageFilter} is used here only as a helper in the
  //  determination of an initial Level Set. We could have used the
  //  \doxygen{DanielssonDistanceMapImageFilter} in the same way.
  //
  //
  //  The seeds are passed stored in a container. The type of this
  //  container is defined as \code{NodeContainer} among the
  //  \doxygen{FastMarchingImageFilter} traits.
  //

  typedef FastMarchingFilterType::NodeContainer           NodeContainer;
  typedef FastMarchingFilterType::NodeType                NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();
  

  InternalImageType::IndexType  seedPosition;
  
  seedPosition[0] = atoi( argv[3] );
  seedPosition[1] = atoi( argv[4] );


  //
  //  Nodes are created as stack variables and initialized with a value and an
  //  \doxygen{Index} position. Note that here we assign the value of minus the
  //  user-provided distance to the unique node of the seeds passed to the
  //  \doxygen{FastMarchingImageFilter}. In this way, the value will increment
  //  as the front is propagated, until it reaches the zero value corresponding
  //  to the contour. After this, the front will continue propagating until it
  //  fills up the entire image. The initial distance is taken here from the
  //  command line arguments. The rule of thumb for the user is to select this
  //  value as the distance from the seed points at which he want the initial
  //  contour to be.
  //

  const double initialDistance = atof( argv[5] );

  NodeType node;

  const double seedValue = - initialDistance;
  
  node.SetValue( seedValue );
  node.SetIndex( seedPosition );



  //
  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.
  //

  seeds->Initialize();
  seeds->InsertElement( 0, node );




  //
  //  The set of seed nodes is passed now to the
  //  \doxygen{FastMarchingImageFilter} with the method
  //  \code{SetTrialPoints()}.
  //

  fastMarching->SetTrialPoints(  seeds  );




  //  
  //  Since the \doxygen{FastMarchingImageFilter} is used here just as a
  //  Distance Map generator. It does not require a speed image as input.
  //  Instead the constant value $1.0$ is passed using the
  //  \code{SetSpeedConstant()} method.
  //
  
  fastMarching->SetSpeedConstant( 1.0 );


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
  writer1->SetFileName("GeodesicActiveContourImageFilterOutput1.png");
  caster1->SetOutputMinimum(   0 );
  caster1->SetOutputMaximum( 255 );
  writer1->Update();

  caster2->SetInput( gradientMagnitude->GetOutput() );
  writer2->SetInput( caster2->GetOutput() );
  writer2->SetFileName("GeodesicActiveContourImageFilterOutput2.png");
  caster2->SetOutputMinimum(   0 );
  caster2->SetOutputMaximum( 255 );
  writer2->Update();

  caster3->SetInput( sigmoid->GetOutput() );
  writer3->SetInput( caster3->GetOutput() );
  writer3->SetFileName("GeodesicActiveContourImageFilterOutput3.png");
  caster3->SetOutputMinimum(   0 );
  caster3->SetOutputMaximum( 255 );
  writer3->Update();

  caster4->SetInput( fastMarching->GetOutput() );
  writer4->SetInput( caster4->GetOutput() );
  writer4->SetFileName("GeodesicActiveContourImageFilterOutput4.png");
  caster4->SetOutputMinimum(   0 );
  caster4->SetOutputMaximum( 255 );
  



  //
  //  The \doxygen{FastMarchingImageFilter} requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //

  fastMarching->SetOutputSize( 
           reader->GetOutput()->GetBufferedRegion().GetSize() );


  //  
  //  Several parameters should be set in the
  //  \doxygen{GeodesicActiveContourImageFilter}. In particular, the number of
  //  iterations to perform and the time step for solving the differential
  //  equation. The time step should be choosed in such a way that the zero set
  //  does not move more than one pixel at each iteration. A typical value in
  //  $2D$ is $0.25$. The number of iterations is a critical value since
  //  technically the level set will never converge. Instead the front keeps
  //  moving continuously. It is expected though that it will slow down
  //  considerably in the regions where the speed image has close to zero
  //  values. In a real application is desirable to couple the evolution of the
  //  zero set to a visualization module allowing the user to follow the
  //  evolution of the zero set. With this feedback, the user may decide when
  //  to stop the algorithm before the zero set leaks through the regions of
  //  low gradient in the contour of the anatomical structure to be segmented.
  //
  
  const unsigned int numberOfIterations = atoi( argv[ 9] ); 
 
  geodesicActiveContour->SetNumberOfIterations(  numberOfIterations );

  geodesicActiveContour->SetTimeStepSize( 0.25 ); 





  //  Software Guide : BeginLatex
  //  
  //  In order to speed up the computation, we enable the option of using a
  //  narrow band technique for computing th evolution of the contour.
  //
  //  \index{itk::GeodesicActiveContourImageFilter!NarrowBandingOn()}
  //  \index{itk::LevelSetImageFilter!NarrowBandingOn()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  geodesicActiveContour->NarrowBandingOn();
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
  mapWriter->SetFileName("GeodesicActiveContourImageFilterOutput4.mha");
  mapWriter->Update();

  InternalWriterType::Pointer speedWriter = InternalWriterType::New();
  speedWriter->SetInput( sigmoid->GetOutput() );
  speedWriter->SetFileName("GeodesicActiveContourImageFilterOutput3.mha");
  speedWriter->Update();


  InternalWriterType::Pointer gradientWriter = InternalWriterType::New();
  gradientWriter->SetInput( gradientMagnitude->GetOutput() );
  gradientWriter->SetFileName("GeodesicActiveContourImageFilterOutput2.mha");
  gradientWriter->Update();



  //  Software Guide : BeginLatex
  //
  //  Let's now run this example using as input the image
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Insight/Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seeds in the appropriate locations. The following
  //  table presents the parameters used for some structures.
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|c|c|c|c|}
  //  \hline
  //  Structure    & Seed Index &  Distance   &   $\sigma$  &     
  //  $\alpha$     &  $\beta$   & Iterations  & Inflation & Output Image \\   
  //  \hline
  //  Left Ventricle  & $(81,114)$ & 5.0 & 1.0 & -0.5 & 3.0  & 100 & 3.0 & First  in Figure \ref{fig:GeodesicActiveContourImageFilterOutput2} \\ 
  //  Right Ventricle & $(99,114)$ & 5.0 & 1.0 & -0.5 & 3.0  & 100 & 3.0 & Second in Figure \ref{fig:GeodesicActiveContourImageFilterOutput2} \\ 
  //  White matter    & $(56, 92)$ & 5.0 & 1.0 & -0.3 & 2.0  & 500 & 3.0 & Third  in Figure \ref{fig:GeodesicActiveContourImageFilterOutput2} \\ 
  //  Gray matter     & $(40, 90)$ & 5.0 & 0.5 & -0.3 & 2.0  & 200 & 3.0 & Fourth in Figure \ref{fig:GeodesicActiveContourImageFilterOutput2} \\ 
  //  \hline
  //  \end{tabular}
  //  \end{center}
  //
  //  The intermediate outputs of this pipeline are identical to those of the
  //  \doxygen{ShapDetectionImageFilter} presented in
  //  Figure~\ref{fig:ShapDetectionImageFilterOutput}.
  //
  //  An interesting trade-off exists between the number of iterations to run,
  //  the inflation strength and the contrast of the borders in the anatomical
  //  structure. In principle we want to set the inflation strength as high as
  //  possible since this will make the front propagate faster and hence less
  //  iteration will be needed to move the contour to the structure edges.
  //  However the inflation force may provoke a premature leaking of the
  //  contour through the regions of weak contrast in the structure edges.
  //  Unfortunately only experimentation can allow to determine the optimal
  //  values for the inflation strength and the number of iterations. In a real
  //  application we could imagine an interactive mechanism by which a human
  //  operator identifies the regions of weak contrast in the object edges, and
  //  from this value a reasonable guess of the inflation strength could be
  //  made. Then the filter could be run for a certain number of iterations
  //  stopping periodically in order to display intermediate results showing
  //  the current position of the contour. In this setup, an operator will be
  //  able to supervise the growth of the contour and make informed decisions
  //  on whether new parameters are needed or simply more iterations are
  //  required.
  //
  // \begin{figure} \center
  // \includegraphics[width=4cm]{GeodesicActiveContourImageFilterOutput5.eps}
  // \includegraphics[width=4cm]{GeodesicActiveContourImageFilterOutput6.eps}
  // \includegraphics[width=4cm]{GeodesicActiveContourImageFilterOutput7.eps}
  // \includegraphics[width=4cm]{GeodesicActiveContourImageFilterOutput8.eps}
  // \caption[GeodesicActiveContourImageFilter segmentations]{Images generated by the
  // segmentation process based on the GeodesicActiveContourImageFilter. From left to
  // right: Segmentation of the left ventricle, segmentation of the right
  // ventricle, segmentation of the white matter, attempt of segmentation of
  // the gray matter.}
  // \label{fig:GeodesicActiveContourImageFilterOutput2}
  // \end{figure}
  //
  //  Software Guide : EndLatex 




  return 0;

}




