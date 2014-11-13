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
//    OUTPUTS: {MinMaxCurvatureFlowImageFilterOutput.png}
//    ARGUMENTS:    10 0.125 1
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//
// \begin{figure}
// \center
// \includegraphics[width=0.5\textwidth]{MinMaxCurvatureFlowFunctionDiagram}
// \itkcaption[MinMaxCurvatureFlow computation]{Elements involved in the
//  computation of min-max curvature flow.}
// \label{fig:MinMaxCurvatureFlowFunctionDiagram}
// \end{figure}
//
//  The MinMax curvature flow filter applies a variant of the curvature flow
//  algorithm where diffusion is turned on or off depending of the scale of the
//  noise that one wants to remove.  The evolution speed is switched between
//  $\min(\kappa,0)$ and $\max(\kappa,0)$ such that:
//
//  \begin{equation}
//  I_t = F |\nabla I|
//  \end{equation}
//
//  where $F$ is defined as
//
//  \begin{equation}
//  F = \left\{ \begin{array} {r@{\quad:\quad}l}
//         \max(\kappa,0) & \mbox{Average} < Threshold \\ \min(\kappa,0) & \mbox{Average} \ge Threshold
//             \end{array} \right.
//  \end{equation}
//
// The $Average$ is the average intensity computed over a neighborhood of a
// user-specified radius of the pixel. The choice of the radius governs the
// scale of the noise to be removed. The $Threshold$ is calculated as the
// average of pixel intensities along the direction perpendicular to the
// gradient at the \emph{extrema} of the local neighborhood.
//
// A speed of $F = max(\kappa,0)$ will cause small dark regions in a
// predominantly light region to shrink.  Conversely, a speed of $F =
// min(\kappa,0)$, will cause light regions in a predominantly dark region to
// shrink. Comparison between the neighborhood average and the threshold is
// used to select the the right speed function to use. This switching
// prevents the unwanted diffusion of the simple curvature flow method.
//
// Figure~\ref{fig:MinMaxCurvatureFlowFunctionDiagram} shows the main
// elements involved in the computation. The set of square pixels represent
// the neighborhood over which the average intensity is being computed. The
// gray pixels are those lying close to the direction perpendicular to the
// gradient.  The pixels which intersect the neighborhood bounds are used to
// compute the threshold value in the equation above. The integer radius of
// the neighborhood is selected by the user.
//
//  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use the \doxygen{MinMaxCurvatureFlowImageFilter}
//  is to include its header file.
//
//  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMinMaxCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  ";
    std::cerr << "numberOfIterations  timeStep  stencilRadius" << std::endl;
    return EXIT_FAILURE;
    }
  //  Software Guide : BeginLatex
  //
  //  Types should be selected based on the pixel types required for the
  //  input and output images. The input and output image types are
  //  instantiated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{MinMaxCurvatureFlowImageFilter} type is now instantiated
  //  using both the input image and the output image types. The filter is
  //  then created using the \code{New()} method.
  //
  //  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!instantiation}
  //  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!New()}
  //  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MinMaxCurvatureFlowImageFilter<
               InputImageType, OutputImageType >  FilterType;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  const unsigned int numberOfIterations = atoi( argv[3] );
  const double       timeStep = atof( argv[4] );
  typedef FilterType::RadiusValueType RadiusType;
  const RadiusType radius = atol( argv[5] );

  //  Software Guide : BeginLatex
  //
  //  The \doxygen{MinMaxCurvatureFlowImageFilter} requires the two normal
  //  parameters of the CurvatureFlow image, the number of iterations to be
  //  performed and the time step used in the computation of the level set
  //  evolution. In addition, the radius of the neighborhood is also
  //  required. This last parameter is passed using the
  //  \code{SetStencilRadius()} method. Note that the radius is provided as an
  //  integer number since it is referring to a number of pixels from the center
  //  to the border of the neighborhood. Then the filter can be executed by
  //  invoking \code{Update()}.
  //
  //  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!Update()}
  //  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!SetTimeStep()}
  //  \index{itk::MinMax\-Curvature\-Flow\-Image\-Filter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::MinMax\-Curvature\-Flow\-Image\-Filter}
  //  \index{SetNumberOfIterations()!itk::MinMax\-Curvature\-Flow\-Image\-Filter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetTimeStep( timeStep );
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetStencilRadius( radius );
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are $0.125$ in $2D$ images and
  //  $0.0625$ in $3D$ images. The number of iterations can be usually around
  //  $10$, more iterations will result in further smoothing and will
  //  increase the computing time linearly. The radius of the stencil can be
  //  typically $1$. The \emph{edge-preserving} characteristic is not perfect
  //  on this filter. Some degradation will occur on the edges and will
  //  increase as the number of iterations is increased.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down
  //  the pipeline, updating any of the downstream filters will
  //  trigger the execution of this one. For example, a writer filter can
  //  be used after the curvature flow filter.
  //
  //  Software Guide : EndLatex

  typedef unsigned char                          WritePixelType;
  typedef itk::Image< WritePixelType, 2 >        WriteImageType;
  typedef itk::RescaleIntensityImageFilter<
               OutputImageType, WriteImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );

  // Software Guide : BeginCodeSnippet
  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{MinMaxCurvatureFlowImageFilterOutput}
  // \itkcaption[MinMaxCurvatureFlowImageFilter output]{Effect of the
  // MinMaxCurvatureFlowImageFilter on a slice from a MRI proton density image
  // of the brain.}
  // \label{fig:MinMaxCurvatureFlowImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:MinMaxCurvatureFlowImageFilterInputOutput} illustrates
  //  the effect of this filter on a MRI proton density image of the
  //  brain. In this example the filter was run with a time step of $0.125$,
  //  $10$ iterations and a radius of $1$.  The figure shows how homogeneous
  //  regions are smoothed and edges are preserved. Notice also, that the
  //  result in the figure has sharper edges than the same example using
  //  simple curvature flow in Figure
  //  \ref{fig:CurvatureFlowImageFilterInputOutput}.
  //
  //  \relatedClasses
  //  \begin{itemize}
  //  \item \doxygen{CurvatureFlowImageFilter}
  //  \end{itemize}
  //
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
