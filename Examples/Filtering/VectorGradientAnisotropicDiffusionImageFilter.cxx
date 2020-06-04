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

//  Software Guide : BeginLatex
//
//  The \doxygen{VectorGradientAnisotropicDiffusionImageFilter} implements an
//  $N$-dimensional version of the classic Perona-Malik anisotropic diffusion
//  equation for vector-valued images. Typically in vector-valued diffusion,
//  vector components are diffused independently of one another using a
//  conductance term that is linked across the components. The diffusion
//  equation was illustrated in
//  \ref{sec:GradientAnisotropicDiffusionImageFilter}.
//
//  This filter is designed to process images of \doxygen{Vector} type.  The
//  code relies on various type alias and overloaded operators defined in
//  \doxygen{Vector}. It is perfectly reasonable, however, to apply this
//  filter to images of other, user-defined types as long as the appropriate
//  type alias and operator overloads are in place.  As a general rule, follow
//  the example of \doxygen{Vector} in defining your data types.
//
//  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"


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


int
main(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputGradientImageFile ";
    std::cerr << "outputSmoothedGradientImageFile ";
    std::cerr << "numberOfIterations timeStep" << std::endl;
    return EXIT_FAILURE;
  }


  //  Software Guide : BeginLatex
  //
  //  Types should be selected based on required pixel type for the input and
  //  output images.  The image types are defined using the pixel type and
  //  the dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using InputPixelType = float;
  using VectorPixelType = itk::CovariantVector<float, 2>;
  using InputImageType = itk::Image<InputPixelType, 2>;
  using VectorImageType = itk::Image<VectorPixelType, 2>;
  // Software Guide : EndCodeSnippet


  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types. The filter object is created by the \code{New()}
  //  method.
  //
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!instantiation}
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!New()}
  //  \index{itk::Vector\-Gradient\-Anisotropic\-Diffusion\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using FilterType =
    itk::VectorGradientAnisotropicDiffusionImageFilter<VectorImageType,
                                                       VectorImageType>;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  using GradientFilterType =
    itk::GradientRecursiveGaussianImageFilter<InputImageType,
                                              VectorImageType>;
  GradientFilterType::Pointer gradient = GradientFilterType::New();


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source and its data is passed through a
  //  gradient filter in order to generate an image of vectors.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  gradient->SetInput(reader->GetOutput());
  filter->SetInput(gradient->GetOutput());
  // Software Guide : EndCodeSnippet


  const unsigned int numberOfIterations = std::stoi(argv[4]);
  const double       timeStep = std::stod(argv[5]);


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
  //  Typical values for the time step are $0.125$ in $2D$ images and
  //  $0.0625$ in $3D$ images. The number of iterations can be usually around
  //  $5$, however more iterations will result in further smoothing and will
  //  linearly increase the computing time.
  //
  //  Software Guide : EndLatex


  //  If the output of this filter has been connected to other filters down
  //  the pipeline, updating any of the downstream filters would have
  //  triggered the execution of this one. For example, a writer filter could
  //  have been used after the curvature flow filter.
  //
  using OutputPixelType = float;
  using OutputImageType = itk::Image<OutputPixelType, 2>;
  using ComponentFilterType =
    itk::VectorIndexSelectionCastImageFilter<VectorImageType,
                                             OutputImageType>;
  ComponentFilterType::Pointer component = ComponentFilterType::New();

  // Select the component to extract.
  component->SetIndex(0);

  using WritePixelType = unsigned char;
  using WriteImageType = itk::Image<WritePixelType, 2>;
  using RescaleFilterType =
    itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  using WriterType = itk::ImageFileWriter<WriteImageType>;
  WriterType::Pointer writer = WriterType::New();
  rescaler->SetInput(component->GetOutput());
  writer->SetInput(rescaler->GetOutput());

  // Save the component of the original gradient
  component->SetInput(gradient->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  // Save the component of the smoothed gradient
  component->SetInput(filter->GetOutput());
  writer->SetFileName(argv[3]);
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{VectorGradientAnisotropicDiffusionImageFilterInput}
  // \includegraphics[width=0.44\textwidth]{VectorGradientAnisotropicDiffusionImageFilterOutput}
  // \itkcaption[VectorGradientAnisotropicDiffusionImageFilter output]{Effect
  // of the VectorGradientAnisotropicDiffusionImageFilter on the $X$ component
  // of the gradient from a MRI proton density brain image.}
  // \label{fig:VectorGradientAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure
  //  \ref{fig:VectorGradientAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain. The images show the $X$ component of the gradient before
  //  (left) and after (right) the application of the filter. In this example
  //  the filter was run with a time step of $0.25$, and $5$ iterations.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
