/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ThresholdSegmentationLevelSetImageFilter.cxx
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
// \index{itk::ThresholdSegmentationLevelSetImageFilter}
//
// The \doxygen{ThresholdSegmentationLevelSetImageFilter} is an extension of threshold
// connected-component segmentation to the level-set framework.  The goal is to
// define a range of intensity values that classify the tissue type of interest
// and then base the propagation term of the level-set equation on that
// intensity range.  Using the level-set approach, the smoothness of the
// evolving surface can be constrained to prevent some of the ``leaking'' that
// is common in connected-component schemes.
//
// The propagation term $P$ from equation~\ref{eqn:LevelSetEquation} is
// calculated from the \code{FeatureImage} input $g$ with \code{UpperThreshold}
// $U$ and \code{LowerThreshold} $L$ according to the following formula.
//
// \begin{equation}
// \label{eqn:ThresholdSegmentationLevelSetImageFilterPropagationTerm}
// P(\mathbf{x}) = \left\{ \begin{array}{ll} g(\mathbf{x}) - L &
// \mbox{if $g(\mathbf{x}) < (U-L)/2 + L$} \\ U - g(\mathbf{x}) &
// \mbox{otherwise} \end{array} \right.  \end{equation}
//
// Figure~\ref{fig:ThresholdSegmentationSpeedTerm} illustrates the propagation
// term function.  Intensity values in $g$ between $L$ and $H$ yield positive
// values in $P$, while outside intensities yield negative values in $P$.
//
// \begin{figure} \center
// \includegraphics[width=8cm]{ThresholdSegmentationLevelSetImageFilterFigure1.eps}
// \caption[Propagation term for threshold-based level-set segmentation]{Propagation term for threshold-based level-set
// segmentation. From
// equation~\ref{eqn:ThresholdSegmentationLevelSetImageFilterPropagationTerm}. }
// \label{fig:ThresholdSegmentationSpeedTerm} 
// \end{figure}
//
// \doxygen{ThresholdSegmentationLevelSetImageFilter} expects two inputs.  The
// first is an initial level set in the form of an \doxygen{Image}. The second
// input is the feature image $g$.  For many applications, this filter requires
// little or no preprocessing of its input.  Smoothing the input image is not
// usually required to produce reasonable solutions, though it may still be
// warranted in some cases.
// 
// The following example illustrates the use of the
// \doxygen{ThresholdSegmentationLevelSetImageFilter}.
// Figure~\ref{fig:ThresholdSegmentationLevelSetImageFilterDiagram} shows how
// the image processing pipeline is constructed. The initial surface is
// generated using the fast marching filter.  The output of the segmentation
// filter is passed to a \doxygen{BinaryThresholdImageFilter} to create a
// binary representation of the segmented object.
//
// \begin{figure} \center
// \includegraphics[width=15cm]{ThresholdSegmentationLevelSetImageFilterCollaborationDiagram1.eps}
// \caption[ThresholdSegmentationLevelSetImageFilter collaboration
// diagram]{Collaboration diagram for the ThresholdSegmentationLevelSetImageFilter
// applied to a segmentation task.}
// \label{fig:ThresholdSegmentationLevelSetImageFilterDiagram1}
// \end{figure}
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 


#include "itkImage.h"
// Software Guide : BeginCodeSnippet
#include "itkThresholdSegmentationLevelSetImageFilter.h"
// Software Guide : EndCodeSnippet
#include "itkFastMarchingImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkZeroCrossingImageFilter.h"


int main( int argc, char *argv[] )
{

  if( argc < 8 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage";
    std::cerr << " seedX seedY InitialDistance";
    std::cerr << " LowerThreshold";
    std::cerr << " UpperThreshold" << std::endl;
    return 1;
    }

  //  Software Guide : BeginLatex
  //  
  //  We declare the image type using a pixel type and a particular
  //  dimension. In this case we will use 2D \code{float} images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet

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

  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  //  
  //  We now declare the type of the \doxygen{FastMarchingImageFilter} that
  //  will be used to generate the initial level set in the form of a distance
  //  map.
  //
  typedef  itk::FastMarchingImageFilter< 
                              InternalImageType, 
                              InternalImageType >    FastMarchingFilterType;

  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();
  
  //  Software Guide : BeginLatex
  //  
  //  The following lines instantiate a
  //  \doxygen{ThresholdSegmentationLevelSetImageFilter} and create an object of this type
  //  using the \code{New()} method.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef  itk::ThresholdSegmentationLevelSetImageFilter< 
                              InternalImageType, 
                              InternalImageType >    ThresholdSegmentationLevelSetImageFilterType;

  ThresholdSegmentationLevelSetImageFilterType::Pointer thresholdSegmentation = 
                                     ThresholdSegmentationLevelSetImageFilterType::New();
  // Software Guide : EndCodeSnippet

  
  //  Software Guide : BeginLatex
  //  
  //  For the \doxygen{ThresholdSegmentationLevelSetImageFilter}, scaling
  //  parameters are used to balance the influence of the propagation
  //  (inflation) and the curvature (surface smoothing) terms from
  //  equation~\ref{eqn:LevelSetEquation}. The advection term is not used in
  //  this filter. Set the terms with methods \code{SetPropagationScaling()}
  //  and \code{SetCurvatureScaling()}. Both terms are set to 1.0 in this
  //  example.
  //
  //  \index{itk::ThresholdSegmentationLevelSetImageFilter!SetPropagationScaling()}
  //  \index{itk::SegmentationLevelSetImageFilter!SetPropagationScaling()}
  //  \index{itk::ThresholdSegmentationLevelSetImageFilter!SetCurvatureScaling()}
  //  \index{itk::SegmentationLevelSetImageFilter!SetCurvatureScaling()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  thresholdSegmentation->SetPropagationScaling( 1.0 );
  thresholdSegmentation->SetCurvatureScaling( 1.0 );
  //  Software Guide : EndCodeSnippet 

  //  The level-set solver will stop if the convergence criteria has
  //  been reached or if the maximum number of iterations has elasped.
  //  The convergence criteria is defined in terms of the root mean squared (RMS)
  //  change in the level set function. When RMS change for an iteration is
  //  below a user-specified threshold, the solution is considered to have converged.
  thresholdSegmentation->SetMaximumRMSError( 0.02 );
  thresholdSegmentation->SetMaximumIterations( 1200 );

  // Software Guide : BeginLatex
  //
  // The convergence criteria \code{MaximumRMSError} and
  // \code{MaximumIterations} are set as in previous examples.  We now set the
  // upper and lower threshold values $U$ and $L$, and the isosurface value to
  // use in the initial model.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  thresholdSegmentation->SetUpperThreshold( ::atof(argv[7]) );
  thresholdSegmentation->SetLowerThreshold( ::atof(argv[6]) );
  thresholdSegmentation->SetIsoSurfaceValue(0.0);
  thresholdSegmentation->SetUseNegativeFeaturesOn();
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //  
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:ThresholdLevelSetSegmentationCollaborationDiagram1}.
  //  Remember that before calling \code{Update()} on the file writer object, the fast
  //  marching filter must be initialized with the seed points and the output
  //  from the reader object.  See previous examples and the source code for
  //  this section for details.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  thresholdSegmentation->SetInput( fastMarching->GetOutput() );
  thresholdSegmentation->SetFeatureImage( reader->GetOutput() );
  thresholder->SetInput( thresholdSegmentation->GetOutput() );
  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet

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
  //  The \doxygen{FastMarchingImageFilter} requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //

  
  //  Software Guide : BeginLatex
  //  
  //  Invoking the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  As usual, the call is placed in a
  //  \code{try/catch} block should any errors ocurr or exceptions be thrown.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  try
    {
      reader->Update();
      fastMarching->SetOutputSize( 
           reader->GetOutput()->GetBufferedRegion().GetSize() );

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
  std::cout << "Max. no. iterations: " << thresholdSegmentation->GetMaximumIterations() << std::endl;
  std::cout << "Max. RMS error: " << thresholdSegmentation->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << thresholdSegmentation->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << thresholdSegmentation->GetRMSChange() << std::endl;


  //
  // We write out some intermediate images for debugging.  These images can
  // help tune parameters.
  //
  typedef itk::ImageFileWriter< InternalImageType > InternalWriterType;

  InternalWriterType::Pointer mapWriter = InternalWriterType::New();
  mapWriter->SetInput( fastMarching->GetOutput() );
  mapWriter->SetFileName("fastMarchingImage.mha");
  mapWriter->Update();

  InternalWriterType::Pointer speedWriter = InternalWriterType::New();
  speedWriter->SetInput( thresholdSegmentation->GetSpeedImage() );
  speedWriter->SetFileName("speedTermImage.mha");
  speedWriter->Update();


  //  Software Guide : BeginLatex
  //
  //  Let's run this application with the same data and parameters as the
  //  example given for \code{itk::ConnectedThreshold} in
  //  section~\ref{sec:ConnectedThreshold}. We will use a value of 5 as the
  //  initial distance of the surface from the seed points.  The algorithm is
  //  relatively insensitive to this initialization.
  //  Compare the results in
  //  figure~\ref{fig:ThresholdSegmentationLevelSetImageFilter} with those in
  //  figure~\ref{fig:ConnectedThresholdOutput}. Notice how the smoothness 
  //  constraint on the surface prevents leakage of the segmentation into both
  //  ventricles, but also localizes the segmentation to a smaller portion of
  //  the gray matter.
  //
  //  \begin{figure}
  //  \includegraphics[width=4cm]{BrainProtonDensitySlice.eps}
  //  \includegraphics[width=4cm]{ThresholdSegmentationLevelSetImageFilterWhiteMatter.eps}
  //  \includegraphics[width=4cm]{ThresholdSegmentationLevelSetImageFilterVentricle.eps}
  //  \includegraphics[width=4cm]{ThresholdSegmentationLevelSetImageFilterGrayMatter.eps}
  //  \label{fig:ThresholdSegmentationLevelSetImageFilter}
  //  \end{figure}
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|c|}
  //  \hline
  //  Structure & Seed Index & Lower & Upper & Output Image \\ \hline  \\ \hline
  //  White matter & $(60,116)$ & 150 & 180 & Second from left  \\  \hline
  //  Ventricle    & $(81,112)$ & 210 & 250 & Third  from left  \\  \hline
  //  Gray matter  & $(107,69)$ & 180 & 210 & Fourth from left  \\  \hline
  //  \end{tabular}
  //  \caption{Segmentation results of ThresholdSegmentationLevelSetImageFilter for various seed points}.
  //  \end{center}
  // 
  //  Software Guide : EndLatex 

  return 0;
}



