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
//  INPUTS:  {VisibleWomanHeadSlice.png}
//  OUTPUTS: {RGBCurvatureAnisotropicDiffusionImageFilterOutput.png}
//  ARGUMENTS:    20 0.125
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The vector anisotropic diffusion approach can be applied equally well to
//  color images. As in the vector case, each RGB component is diffused
//  independently. The following example illustrates the use of the
//  \doxygen{VectorCurvatureAnisotropicDiffusionImageFilter} on an image with
//  \doxygen{RGBPixel} type.
//
//  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!RGB Images}
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Also the headers for \code{Image} and \code{RGBPixel} type are required.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  It is desirable to perform the computation on the RGB image using
//  \code{float} representation. However for input and output purposes
//  \code{unsigned char} RGB components are commonly used. It is necessary to
//  cast the type of color components in the pipeline before writing them to
//  a file. The \doxygen{VectorCastImageFilter} is used to achieve this goal.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVectorCastImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputRGBImageFile  outputRGBImageFile ";
    std::cerr << "numberOfIterations  timeStep  " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  The image type is defined using the pixel type and the dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< float >          InputPixelType;
  typedef itk::Image< InputPixelType, 2 > InputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated and a filter object is created by the
  //  \code{New()} method.
  //
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!instantiation}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!New()}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::VectorCurvatureAnisotropicDiffusionImageFilter<
                       InputImageType, InputImageType >  FilterType;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another
  //  filter. Here, an image reader is used as a source.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  const unsigned int numberOfIterations = atoi( argv[3] );
  const double       timeStep = atof( argv[4] );


  //  Software Guide : BeginLatex
  //
  //  This filter requires two parameters: the number of iterations to be
  //  performed and the time step used in the computation of the level set
  //  evolution. These parameters are set using the methods
  //  \code{SetNumberOfIterations()} and \code{SetTimeStep()} respectively.  The filter can
  //  be executed by invoking \code{Update()}.
  //
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!Update()}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!SetTimeStep()}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter}
  //  \index{SetNumberOfIterations()!itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  filter->SetConductanceParameter(1.0);
  filter->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filter output is now cast to \code{unsigned char} RGB components by
  //  using the \doxygen{VectorCastImageFilter}.
  //
  //  \index{itk::VectorCastImageFilter!instantiation}
  //  \index{itk::VectorCastImageFilter!New()}
  //  \index{itk::VectorCastImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< unsigned char >   WritePixelType;
  typedef itk::Image< WritePixelType, 2 >  WriteImageType;
  typedef itk::VectorCastImageFilter<
                InputImageType, WriteImageType >  CasterType;
  CasterType::Pointer caster = CasterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Finally, the writer type can be instantiated. One writer is created and
  //  connected to the output of the cast filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  caster->SetInput( filter->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{VisibleWomanHeadSlice}
  // \includegraphics[width=0.44\textwidth]{RGBCurvatureAnisotropicDiffusionImageFilterOutput}
  // \itkcaption[VectorCurvatureAnisotropicDiffusionImageFilter output on RGB]
  // {Effect of the VectorCurvatureAnisotropicDiffusionImageFilter on a RGB
  // image from a cryogenic section of the Visible Woman data set.}
  // \label{fig:RGBVectorCurvatureAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure
  //  \ref{fig:RGBVectorCurvatureAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a RGB image from a cryogenic
  //  section of the Visible Woman data set.  In this example the filter was
  //  run with a time step of $0.125$, and $20$ iterations.  The input image
  //  has $570 \times 670$ pixels and the processing took $4$ minutes on a
  //  Pentium 4 at 2GHz.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.32\textwidth]{VisibleWomanEyeSlice}
  // \includegraphics[width=0.32\textwidth]{RGBGradientAnisotropicDiffusionImageFilterOutput2}
  // \includegraphics[width=0.32\textwidth]{RGBCurvatureAnisotropicDiffusionImageFilterOutput2}
  // \itkcaption[Various Anisotropic Diffusion compared] {Comparison between
  // the gradient (center) and curvature (right) Anisotropic Diffusion filters.
  // Original image at left.}
  // \label{fig:ComparisionGradientCurvatureRGBAnisotropicDiffusion}
  // \end{figure}
  //
  //  Figure \ref{fig:ComparisionGradientCurvatureRGBAnisotropicDiffusion}
  //  compares the effect of the gradient and curvature anisotropic diffusion
  //  filters on a small region of the same cryogenic slice used in Figure
  //  \ref{fig:RGBVectorCurvatureAnisotropicDiffusionImageFilterInputOutput}.
  //  The region used in this figure is only  $127 \times 162$ pixels and took
  //  $14$ seconds to compute on the same platform.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
