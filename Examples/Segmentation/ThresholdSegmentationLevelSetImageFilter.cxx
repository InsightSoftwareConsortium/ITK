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
//    OUTPUTS: {ThresholdSegmentationLevelSetImageFilterWhiteMatter.png}
//    ARGUMENTS:    60 116 5 150 180
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ThresholdSegmentationLevelSetImageFilterVentricle.png}
//    ARGUMENTS:    81 112 5 210 250
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ThresholdSegmentationLevelSetImageFilterGrayMatter.png}
//    ARGUMENTS:    107 69 5 180  210
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// \index{itk::Threshold\-Segmentation\-Level\-Set\-Image\-Filter}
//
// The \doxygen{ThresholdSegmentationLevelSetImageFilter} is an extension of
// the threshold connected-component segmentation to the level set framework.
// The goal is to define a range of intensity values that classify the tissue
// type of interest and then base the propagation term on the level set
// equation for that intensity range.  Using the level set approach, the
// smoothness of the evolving surface can be constrained to prevent some of
// the ``leaking'' that is common in connected-component schemes.
//
// The propagation term $P$ from Equation~\ref{eqn:LevelSetEquation} is
// calculated from the \code{FeatureImage} input $g$ with
// \code{UpperThreshold} $U$ and \code{LowerThreshold} $L$ according to the
// following formula.
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
// \includegraphics[width=0.8\textwidth]{ThresholdSegmentationLevelSetImageFilterCollaborationDiagram1}
// \itkcaption[ThresholdSegmentationLevelSetImageFilter collaboration
// diagram]{Collaboration diagram for the ThresholdSegmentationLevelSetImageFilter
// applied to a segmentation task.}
// \label{fig:ThresholdSegmentationLevelSetImageFilterDiagram}
// \end{figure}
//
// \begin{figure} \center
// \includegraphics[width=6.5cm]{ThresholdSegmentationLevelSetImageFilterFigure1}
// \itkcaption[Propagation term for threshold-based level set segmentation]
// {Propagation term for threshold-based level set segmentation.
// From Equation~\ref{eqn:ThresholdSegmentationLevelSetImageFilterPropagationTerm}.
// \label{fig:ThresholdSegmentationSpeedTerm}}
// \end{figure}
//
// The threshold segmentation filter expects two inputs.  The first is an
// initial level set in the form of an \doxygen{Image}. The second input is
// the feature image $g$.  For many applications, this filter requires little
// or no preprocessing of its input.  Smoothing the input image is not
// usually required to produce reasonable solutions, though it may still be
// warranted in some cases.
//
// Figure~\ref{fig:ThresholdSegmentationLevelSetImageFilterDiagram} shows how
// the image processing pipeline is constructed. The initial surface is
// generated using the fast marching filter.  The output of the segmentation
// filter is passed to a \doxygen{BinaryThresholdImageFilter} to create a
// binary representation of the segmented object.  Let's start by including the
// appropriate header file.
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
    std::cerr << " UpperThreshold";
    std::cerr << " [CurvatureScaling == 1.0]";
    std::cerr << std::endl;
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

  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::BinaryThresholdImageFilter<InternalImageType, OutputImageType>
                                                   ThresholdingFilterType;

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


  //  We now declare the type of the \doxygen{FastMarchingImageFilter} that
  //  will be used to generate the initial level set in the form of a distance
  //  map.
  //
  typedef  itk::FastMarchingImageFilter< InternalImageType, InternalImageType >
    FastMarchingFilterType;

  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();

  //  Software Guide : BeginLatex
  //
  //  The following lines instantiate a
  //  ThresholdSegmentationLevelSetImageFilter using the \code{New()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  itk::ThresholdSegmentationLevelSetImageFilter< InternalImageType,
    InternalImageType > ThresholdSegmentationLevelSetImageFilterType;
  ThresholdSegmentationLevelSetImageFilterType::Pointer thresholdSegmentation =
    ThresholdSegmentationLevelSetImageFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  For the ThresholdSegmentationLevelSetImageFilter, scaling
  //  parameters are used to balance the influence of the propagation
  //  (inflation) and the curvature (surface smoothing) terms from
  //  Equation~\ref{eqn:LevelSetEquation}. The advection term is not used in
  //  this filter. Set the terms with methods \code{SetPropagationScaling()}
  //  and \code{SetCurvatureScaling()}. Both terms are set to 1.0 in this
  //  example.
  //
  //  \index{itk::Threshold\-Segmentation\-Level\-Set\-Image\-Filter!SetPropagationScaling()}
  //  \index{itk::Segmentation\-Level\-Set\-Image\-Filter!SetPropagationScaling()}
  //  \index{itk::Threshold\-Segmentation\-Level\-Set\-Image\-Filter!SetCurvatureScaling()}
  //  \index{itk::Segmentation\-Level\-Set\-Image\-Filter!SetCurvatureScaling()}
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginCodeSnippet
  thresholdSegmentation->SetPropagationScaling( 1.0 );
  if ( argc > 8 )
    {
    thresholdSegmentation->SetCurvatureScaling( atof(argv[8]) );
    }
  else
    {
    thresholdSegmentation->SetCurvatureScaling( 1.0 );
    }

  //  Software Guide : EndCodeSnippet

  //  The level set solver will stop if the convergence criteria has been
  //  reached or if the maximum number of iterations has elasped.  The
  //  convergence criteria is defined in terms of the root mean squared (RMS)
  //  change in the level set function. When RMS change for an iteration is
  //  below a user-specified threshold, the solution is considered to have
  //  converged.
    thresholdSegmentation->SetMaximumRMSError( 0.02 );
    thresholdSegmentation->SetNumberOfIterations( 1200 );

  //    thresholdSegmentation->SetMaximumRMSError( atof(argv[8]) );
  //    thresholdSegmentation->SetNumberOfIterations( atoi(argv[9]) );

  // Software Guide : BeginLatex
  //
  // The convergence criteria \code{MaximumRMSError} and
  // \code{MaximumIterations} are set as in previous examples.  We now set
  // the upper and lower threshold values $U$ and $L$, and the isosurface
  // value to use in the initial model.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  thresholdSegmentation->SetUpperThreshold( ::atof(argv[7]) );
  thresholdSegmentation->SetLowerThreshold( ::atof(argv[6]) );
  thresholdSegmentation->SetIsoSurfaceValue(0.0);
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filters are now connected in a pipeline indicated in
  //  Figure~\ref{fig:ThresholdSegmentationLevelSetImageFilterDiagram}.
  //  Remember that before calling \code{Update()} on the file writer object,
  //  the fast marching filter must be initialized with the seed points and
  //  the output from the reader object.  See previous examples and the
  //  source code for this section for details.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  thresholdSegmentation->SetInput( fastMarching->GetOutput() );
  thresholdSegmentation->SetFeatureImage( reader->GetOutput() );
  thresholder->SetInput( thresholdSegmentation->GetOutput() );
  writer->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet

  //
  //  The FastMarchingImageFilter requires the user to provide a seed
  //  point from which the level set will be generated. The user can actually
  //  pass not only one seed point but a set of them. Note the the
  //  FastMarchingImageFilter is used here only as a helper in the
  //  determination of an initial Level Set. We could have used the
  //  \doxygen{DanielssonDistanceMapImageFilter} in the same way.
  //
  //  The seeds are passed stored in a container. The type of this
  //  container is defined as \code{NodeContainer} among the
  //  FastMarchingImageFilter traits.
  //
  typedef FastMarchingFilterType::NodeContainer           NodeContainer;
  typedef FastMarchingFilterType::NodeType                NodeType;

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
  //  value as the distance from the seed points at which he want the initial
  //  contour to be.

  const double initialDistance = atof( argv[5] );

  NodeType node;

  const double seedValue = - initialDistance;

  node.SetValue( seedValue );
  node.SetIndex( seedPosition );

  //
  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.
  seeds->Initialize();
  seeds->InsertElement( 0, node );


  //  The set of seed nodes is passed now to the
  //  FastMarchingImageFilter with the method
  //  \code{SetTrialPoints()}.
  fastMarching->SetTrialPoints(  seeds  );

  //
  //  Since the FastMarchingImageFilter is used here just as a
  //  Distance Map generator. It does not require a speed image as input.
  //  Instead the constant value $1.0$ is passed using the
  //  \code{SetSpeedConstant()} method.

  fastMarching->SetSpeedConstant( 1.0 );


  //  The FastMarchingImageFilter requires the user to specify the size of the
  //  image to be produced as output. This is done using the
  //  \code{SetOutputRegion()} method. Note that the size is obtained here from
  //  the output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly. Other image parameters such as Origin, Spacing
  //  and Direction are set in a similar manner.


  //  Software Guide : BeginLatex
  //
  //  Invoking the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  As usual, the call is placed in a
  //  \code{try/catch} block should any errors occur or exceptions be thrown.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    reader->Update();
    const InternalImageType * inputImage = reader->GetOutput();
    fastMarching->SetOutputRegion( inputImage->GetBufferedRegion() );
    fastMarching->SetOutputSpacing( inputImage->GetSpacing() );
    fastMarching->SetOutputOrigin( inputImage->GetOrigin() );
    fastMarching->SetOutputDirection( inputImage->GetDirection() );
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
  std::cout << "Max. no. iterations: " << thresholdSegmentation->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << thresholdSegmentation->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << thresholdSegmentation->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << thresholdSegmentation->GetRMSChange() << std::endl;


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
  //  example given for \doxygen{ConnectedThresholdImageFilter} in
  //  Section~\ref{sec:ConnectedThreshold}. We will use a value of 5 as the
  //  initial distance of the surface from the seed points.  The algorithm is
  //  relatively insensitive to this initialization.  Compare the results in
  //  Figure~\ref{fig:ThresholdSegmentationLevelSetImageFilter} with those in
  //  Figure~\ref{fig:ConnectedThresholdOutput}. Notice how the smoothness
  //  constraint on the surface prevents leakage of the segmentation into
  //  both ventricles, but also localizes the segmentation to a smaller
  //  portion of the gray matter.
  //
  //  \begin{figure}
  //  \includegraphics[width=0.24\textwidth]{BrainProtonDensitySlice}
  //  \includegraphics[width=0.24\textwidth]{ThresholdSegmentationLevelSetImageFilterWhiteMatter}
  //  \includegraphics[width=0.24\textwidth]{ThresholdSegmentationLevelSetImageFilterVentricle}
  //  \includegraphics[width=0.24\textwidth]{ThresholdSegmentationLevelSetImageFilterGrayMatter}
  // \itkcaption[ThresholdSegmentationLevelSet segmentations]{Images
  // generated by the segmentation process based on the
  // ThresholdSegmentationLevelSetImageFilter. From left to right:
  // segmentation of the left ventricle, segmentation of the right ventricle,
  // segmentation of the white matter, attempt of segmentation of the gray
  // matter. The parameters used in this segmentations are presented in
  // Table~\ref{tab:ThresholdSegmentationLevelSetImageFilter}.}
  // \label{fig:ThresholdSegmentationLevelSetImageFilter} \end{figure}
  //
  //  \begin{table}
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|c|}
  //  \hline
  //  Structure & Seed Index & Lower & Upper & Output Image \\ \hline
  //  White matter & $(60,116)$ & 150 & 180 & Second from left  \\  \hline
  //  Ventricle    & $(81,112)$ & 210 & 250 & Third  from left  \\  \hline
  //  Gray matter  & $(107,69)$ & 180 & 210 & Fourth from left  \\  \hline
  //  \end{tabular}
  //  \itkcaption[ThresholdSegmentationLevelSet segmentation parameters]
  //  {Segmentation results using the
  //  ThresholdSegmentationLevelSetImageFilter for various seed points. The
  //  resulting images are shown in
  //  Figure~\ref{fig:ThresholdSegmentationLevelSetImageFilter}
  //  \label{tab:ThresholdSegmentationLevelSetImageFilter}.}\end{center}
  //  \end{table}
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
