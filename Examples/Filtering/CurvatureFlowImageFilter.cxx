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
//    OUTPUTS: {CurvatureFlowImageFilterOutput.png}
//    ARGUMENTS:    10 0.25
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The \doxygen{CurvatureFlowImageFilter} performs edge-preserving smoothing
//  in a similar fashion to the classical anisotropic diffusion. The filter
//  uses a level set formulation where the iso-intensity contours in an image
//  are viewed as level sets, where pixels of a particular intensity form one
//  level set. The level set function is then evolved under the control of
//  a diffusion equation where the speed is proportional to the
//  curvature of the contour:
//
//  \begin{equation}
//  I_t = \kappa |\nabla I|
//  \end{equation}
//
//  where $ \kappa $ is the curvature.
//
//  Areas of high curvature will diffuse faster than areas of low curvature.
//  Hence, small jagged noise artifacts will disappear quickly, while large
//  scale interfaces will be slow to evolve, thereby preserving sharp
//  boundaries between objects. However, it should be noted that although the
//  evolution at the boundary is slow, some diffusion will still occur. Thus,
//  continual application of this curvature flow scheme will eventually
//  result in the removal of information as each contour shrinks to a point
//  and disappears.
//
//  \index{itk::CurvatureFlowImageFilter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::CurvatureFlowImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  numberOfIterations  timeStep" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Types should be selected based on the pixel types required for the
  //  input and output images.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  With them, the input and output image types can be instantiated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The CurvatureFlow filter type is now instantiated using both the
  //  input image and the output image types.
  //
  //  \index{itk::CurvatureFlowImageFilter!instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CurvatureFlowImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::CurvatureFlowImageFilter!New()}
  //  \index{itk::CurvatureFlowImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


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


  //  Software Guide : BeginLatex
  //
  //  The CurvatureFlow filter requires two parameters: the number of
  //  iterations to be performed and the time step used in the computation of
  //  the level set evolution. These two parameters are set using the methods
  //  \code{SetNumberOfIterations()} and \code{SetTimeStep()} respectively.
  //  Then the filter can be executed by invoking \code{Update()}.
  //
  //  \index{itk::CurvatureFlowImageFilter!Update()}
  //  \index{itk::CurvatureFlowImageFilter!SetTimeStep()}
  //  \index{itk::CurvatureFlowImageFilter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::CurvatureFlowImageFilter}
  //  \index{SetNumberOfIterations()!itk::CurvatureFlowImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are $0.125$ in $2D$ images and
  //  $0.0625$ in $3D$ images. The number of iterations can be usually around
  //  $10$, more iterations will result in further smoothing and will
  //  increase the computing time linearly. Edge-preserving behavior is not
  //  guaranteed by this filter. Some degradation will occur on the edges and
  //  will increase as the number of iterations is increased.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down
  //  the pipeline, updating any of the downstream filters will
  //  trigger the execution of this one. For example, a writer filter could
  //  be used after the curvature flow filter.
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
  // \includegraphics[width=0.44\textwidth]{CurvatureFlowImageFilterOutput}
  // \itkcaption[CurvatureFlowImageFilter output]{Effect of the
  // CurvatureFlowImageFilter on a slice from a MRI proton density image  of
  // the brain.}
  // \label{fig:CurvatureFlowImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:CurvatureFlowImageFilterInputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain. In this
  //  example the filter was run with a time step of $0.25$ and $10$
  //  iterations.  The figure shows how homogeneous regions are smoothed and
  //  edges are preserved.
  //
  //  \relatedClasses
  //  \begin{itemize}
  //  \item \doxygen{GradientAnisotropicDiffusionImageFilter}
  //  \item \doxygen{CurvatureAnisotropicDiffusionImageFilter}
  //  \item \doxygen{BilateralImageFilter}
  //  \end{itemize}
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
