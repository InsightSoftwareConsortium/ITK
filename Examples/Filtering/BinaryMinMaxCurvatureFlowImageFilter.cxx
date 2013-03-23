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
//    OUTPUTS: {BinaryMinMaxCurvatureFlowImageFilterOutput.png}
//    ARGUMENTS:    10 0.125 1 128
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginLatex
//
//  The \doxygen{BinaryMinMaxCurvatureFlowImageFilter} applies a variant of the
//  CurvatureFlow algorithm. Which means that the speed of propagation is
//  proportional to the curvature $\kappa$ of iso-contours. This filter adds
//  however, the restriction that negative curvatures are only accepted in
//  regions of the image having low intensities. The user should provide an
//  intensity threshold over which negative curvatures are not considered for
//  the propagation.
//
//  In practice the algorithm do the following for each pixel. First, the
//  curvature $\kappa$ is computed on the current pixel. If the computed
//  curvature is null this is returned as value.  Otherwise, an average of
//  neighbor pixel intensities is computed and it is compared against a
//  user-provided threshold. If this average is less than the threshold then
//  the algorithm returns $\min(\kappa,0)$. If the average intensity is greater
//  or equal than user-provided threshold, then the returned value is
//  $\max(\kappa,0)$.
//
//  \begin{equation}
//  I_t = F |\nabla I|
//  \end{equation}
//
//  where $F$ is defined as
//
//  \begin{equation}
//  F = \left\{ \begin{array} {r@{\quad:\quad}l} \min(\kappa,0) &
//  \mbox{Average} < \mbox{Threshold} \\ \max(\kappa,0) & \mbox{Average} \ge
//  \mbox{Threshold} \end{array} \right.
//  \end{equation}
//
//  \index{itk::Binary\-MinMax\-Curvature\-Flow\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv[] )
{


  if( argc < 7 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  ";
    std::cerr << "numberOfIterations  timeStep  stencilRadius  threshold" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be chosen for the pixels of the input and output images and
  //  with them the image types are instantiated.
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
  //  The BinaryMinMaxCurvatureFlowFilter type is now instantiated using both the
  //  input image and the output image types. The filter is then created using
  //  the \code{New()} method.
  //
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!instantiation}
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!New()}
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryMinMaxCurvatureFlowImageFilter<
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

  const double threshold = atof( argv[6] );


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{BinaryMinMaxCurvatureFlowImageFilter} requires the same
  //  parameters of the MinMaxCurvatureFlowImageFilter plus the value of the
  //  threshold against which the neighborhood average will be compared. The
  //  threshold is passed using the \code{SetThreshold()} method. Then the
  //  filter can be executed by invoking \code{Update()}.
  //
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!Update()}
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!SetTimeStep()}
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!SetNumberOfIterations()}
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!SetStencilRadius()}
  //  \index{itk::BinaryMinMaxCurvatureFlowImageFilter!SetThreshold()}
  //  \index{SetTimeStep()!itk::BinaryMinMaxCurvatureFlowImageFilter}
  //  \index{SetStencilRadius()!itk::BinaryMinMaxCurvatureFlowImageFilter}
  //  \index{SetThreshold()!itk::BinaryMinMaxCurvatureFlowImageFilter}
  //  \index{SetNumberOfIterations()!itk::BinaryMinMaxCurvatureFlowImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetTimeStep( timeStep );
  filter->SetNumberOfIterations( numberOfIterations );

  filter->SetStencilRadius( radius );
  filter->SetThreshold( threshold );

  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are $0.125$ in $2D$ images and $0.0625$ in
  //  $3D$ images. The number of iterations can be usually around $10$, more
  //  iterations will result in further smoothing and will increase linearly
  //  the computing time. The radius of the stencil can be typically $1$. The
  //  value of the threshold should be selected according to the gray levels of
  //  the object of interest and the gray level of its background.
  //
  //  Software Guide : EndLatex

  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter<
               OutputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );


  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );


  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{BinaryMinMaxCurvatureFlowImageFilterOutput}
  // \itkcaption[BinaryMinMaxCurvatureFlowImageFilter output]{Effect of the
  // BinaryMinMaxCurvatureFlowImageFilter on a slice from a MRI proton density
  // image  of the brain.}
  // \label{fig:BinaryMinMaxCurvatureFlowImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:BinaryMinMaxCurvatureFlowImageFilterInputOutput} illustrates
  //  the effect of this filter on a MRI proton density image of the brain. In
  //  this example the filter was run with a time step of $0.125$, $10$ iterations,
  //  a stencil radius of $1$ and a threshold of $128$.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;

}
