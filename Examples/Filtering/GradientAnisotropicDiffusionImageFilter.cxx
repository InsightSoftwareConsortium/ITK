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
//    OUTPUTS: {GradientAnisotropicDiffusionImageFilterOutput.png}
//    ARGUMENTS:    15 0.1 3
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginLatex
//
//  The \doxygen{GradientAnisotropicDiffusionImageFilter}  implements an
//  $N$-dimensional version of the classic Perona-Malik anisotropic diffusion
//  equation for scalar-valued images \cite{Perona1990}.
//
//  The conductance term for this implementation is chosen as a function of the
//  gradient magnitude of the image at each point, reducing the strength of
//  diffusion at edge pixels.
//
//  \begin{equation}
//  C(\mathbf{x}) = e^{-(\frac{\parallel \nabla U(\mathbf{x}) \parallel}{K})^2}
//  \end{equation}
//
//  The numerical implementation of this equation is similar to that described
//  in the Perona-Malik paper \cite{Perona1990}, but uses a more robust technique
//  for gradient magnitude estimation and has been generalized to $N$-dimensions.
//
//  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter}
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
//  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile ";
    std::cerr << "numberOfIterations  timeStep  conductance" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be selected based on the pixel types required for the
  //  input and output images.  The image types are defined using the pixel
  //  type and the dimension.
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
  //  The filter type is now instantiated using both the input image and the
  //  output image types. The filter object is created by the \code{New()}
  //  method.
  //
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!instantiation}
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!New()}
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientAnisotropicDiffusionImageFilter<
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

  const double       conductance = atof( argv[5] );


  //  Software Guide : BeginLatex
  //
  //  This filter requires three parameters: the number of iterations to be
  //  performed, the time step and the conductance parameter used in the
  //  computation of the level set evolution. These parameters are set using
  //  the methods \code{SetNumberOfIterations()}, \code{SetTimeStep()} and
  //  \code{SetConductanceParameter()} respectively.  The filter can be
  //  executed by invoking \code{Update()}.
  //
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!Update()}
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!SetTimeStep()}
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!SetConductanceParameter()}
  //  \index{itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter}
  //  \index{SetNumberOfIterations()!itk::Gradient\-Anisotropic\-Diffusion\-Image\-Filter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  filter->SetConductanceParameter( conductance );

  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are $0.25$ in $2D$ images and $0.125$
  //  in $3D$ images. The number of iterations is typically set to $5$; more
  //  iterations result in further smoothing and will increase the computing
  //  time linearly.
  //
  //  Software Guide : EndLatex


  //
  //  The output of the filter is rescaled here and then sent to a writer.
  //
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


  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{GradientAnisotropicDiffusionImageFilterOutput}
  // \itkcaption[GradientAnisotropicDiffusionImageFilter output]{Effect of the
  // GradientAnisotropicDiffusionImageFilter on a slice from a MRI Proton
  // Density image  of the brain.}
  // \label{fig:GradientAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:GradientAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain. In this example the filter was run with a time step of $0.25$,
  //  and $5$ iterations.  The figure shows how homogeneous regions are
  // smoothed and edges are preserved.
  //
  //  \relatedClasses
  //  \begin{itemize}
  //  \item \doxygen{BilateralImageFilter}
  //  \item \doxygen{CurvatureAnisotropicDiffusionImageFilter}
  //  \item \doxygen{CurvatureFlowImageFilter}
  //  \end{itemize}
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
