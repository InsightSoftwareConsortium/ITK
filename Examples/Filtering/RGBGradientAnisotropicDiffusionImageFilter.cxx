/*=========================================================================
 *
 *  Copyright NumFOCUS
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
//  OUTPUTS: {RGBGradientAnisotropicDiffusionImageFilterOutput.png}
//  ARGUMENTS:    20 0.125
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The vector anisotropic diffusion approach applies to color images equally
//  well. As in the vector case, each RGB component is diffused
//  independently. The following example illustrates the use of the Vector
//  curvature anisotropic diffusion filter on an image with
//  \doxygen{RGBPixel} type.
//
//  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!RGB
//  Images}
//
//  Software Guide : EndLatex


//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVectorGradientAnisotropicDiffusionImageFilter.h"
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
//  cast the type of color components along the pipeline before writing them
//  to a file. The \doxygen{CastImageFilter} is used to achieve this
//  goal.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
// Software Guide : EndCodeSnippet


int
main(int argc, char * argv[])
{
  if (argc < 5)
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
  using InputPixelType = itk::RGBPixel<float>;
  using InputImageType = itk::Image<InputPixelType, 2>;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated and a filter object is created by
  //  the \code{New()} method.
  //
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!instantiation}
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!New()}
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using FilterType =
    itk::VectorGradientAnisotropicDiffusionImageFilter<InputImageType,
                                                       InputImageType>;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another
  //  filter. Here, an image reader is used as source.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  filter->SetInput(reader->GetOutput());
  // Software Guide : EndCodeSnippet


  const unsigned int numberOfIterations = std::stoi(argv[3]);
  const double       timeStep = std::stod(argv[4]);


  //  Software Guide : BeginLatex
  //
  //  This filter requires two parameters: the number of iterations to be
  //  performed and the time step used in the computation of the level set
  //  evolution. These parameters are set using the methods
  //  \code{SetNumberOfIterations()} and \code{SetTimeStep()} respectively.
  //  The filter can be executed by invoking \code{Update()}.
  //
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!Update()}
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!SetTimeStep()}
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter}
  //  \index{SetNumberOfIterations()!itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations(numberOfIterations);
  filter->SetTimeStep(timeStep);
  filter->SetConductanceParameter(1.0);
  filter->Update();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filter output is now cast to \code{unsigned char} RGB components by
  //  using the \doxygen{CastImageFilter}.
  //
  //  \index{itk::CastImageFilter!instantiation}
  //  \index{itk::CastImageFilter!New()}
  //  \index{itk::CastImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using WritePixelType = itk::RGBPixel<unsigned char>;
  using WriteImageType = itk::Image<WritePixelType, 2>;
  using CasterType = itk::CastImageFilter<InputImageType, WriteImageType>;
  CasterType::Pointer caster = CasterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, the writer type can be instantiated. One writer is created and
  //  connected to the output of the cast filter.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  using WriterType = itk::ImageFileWriter<WriteImageType>;
  WriterType::Pointer writer = WriterType::New();
  caster->SetInput(filter->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{VisibleWomanHeadSlice}
  // \includegraphics[width=0.44\textwidth]{RGBGradientAnisotropicDiffusionImageFilterOutput}
  // \itkcaption[VectorGradientAnisotropicDiffusionImageFilter on RGB] {Effect
  // of the VectorGradientAnisotropicDiffusionImageFilter on a RGB image from
  // a cryogenic section of the Visible Woman data set.}
  // \label{fig:RGBVectorGradientAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure
  //  \ref{fig:RGBVectorGradientAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a RGB image from a cryogenic
  //  section of the Visible Woman data set.  In this example the filter was
  //  run with a time step of $0.125$, and $20$ iterations.  The input image
  //  has $570 \times 670$ pixels and the processing took $4$ minutes on a
  //  Pentium 4 2GHz.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
