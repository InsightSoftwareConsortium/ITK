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
//    INPUTS:  {BrainProtonDensitySlice.png}
//    ARGUMENTS:    LaplacianRecursiveGaussianImageFilterOutput3.mha 3
//    OUTPUTS: {LaplacianRecursiveGaussianImageFilterOutput3.png}
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    ARGUMENTS:    LaplacianRecursiveGaussianImageFilterOutput5.mha 5
//    OUTPUTS: {LaplacianRecursiveGaussianImageFilterOutput5.png}
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  This example illustrates how to use the
//  \doxygen{RecursiveGaussianImageFilter} for computing the Laplacian of a 2D
//  image.
//
//  \index{itk::RecursiveGaussianImageFilter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkAddImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::RecursiveGaussianImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet
#include "itkRescaleIntensityImageFilter.h"


int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr
      << argv[0]
      << "  inputImageFile  outputImageFile  sigma [RescaledOutputImageFile] "
      << std::endl;
    return EXIT_FAILURE;
  }


  //  Software Guide : BeginLatex
  //
  //  Types should be selected on the desired input and output pixel types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using InputPixelType = float;
  using OutputPixelType = float;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input and output image types are instantiated using the pixel types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using InputImageType = itk::Image<InputPixelType, 2>;
  using OutputImageType = itk::Image<OutputPixelType, 2>;
  // Software Guide : EndCodeSnippet


  using ReaderType = itk::ImageFileReader<InputImageType>;


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::RecursiveGaussianImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using FilterType =
    itk::RecursiveGaussianImageFilter<InputImageType, OutputImageType>;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);


  //  Software Guide : BeginLatex
  //
  //  This filter applies the approximation of the convolution along a single
  //  dimension. It is therefore necessary to concatenate several of these
  //  filters to produce smoothing in all directions.  In this example, we
  //  create a pair of filters since we are processing a $2D$ image.  The
  //  filters are created by invoking the \code{New()} method and assigning
  //  the result to a \doxygen{SmartPointer}.
  //
  //  We need two filters for computing the X component of the Laplacian and
  //  two other filters for computing the Y component.
  //
  //  \index{itk::RecursiveGaussianImageFilter!New()}
  //  \index{itk::RecursiveGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filterX1 = FilterType::New();
  FilterType::Pointer filterY1 = FilterType::New();

  FilterType::Pointer filterX2 = FilterType::New();
  FilterType::Pointer filterY2 = FilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Since each one of the newly created filters has the potential to perform
  //  filtering along any dimension, we have to restrict each one to a
  //  particular direction. This is done with the \code{SetDirection()}
  //  method.
  //
  //  \index{RecursiveGaussianImageFilter!SetDirection()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX1->SetDirection(0); // 0 --> X direction
  filterY1->SetDirection(1); // 1 --> Y direction

  filterX2->SetDirection(0); // 0 --> X direction
  filterY2->SetDirection(1); // 1 --> Y direction
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{RecursiveGaussianImageFilter} can approximate the
  //  convolution with the Gaussian or with its first and second
  //  derivatives. We select one of these options by using the
  //  \code{SetOrder()} method. Note that the argument is an \code{enum} whose
  //  values can be \code{ZeroOrder}, \code{FirstOrder} and
  //  \code{SecondOrder}. For example, to compute the $x$ partial derivative
  //  we should select \code{FirstOrder} for $x$ and \code{ZeroOrder} for $y$.
  //  Here we want only to smooth in $x$ and $y$, so we select
  //  \code{ZeroOrder} in both directions.
  //
  //  \index{RecursiveGaussianImageFilter!SetOrder()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX1->SetOrder(itk::GaussianOrderEnum::ZeroOrder);
  filterY1->SetOrder(itk::GaussianOrderEnum::SecondOrder);

  filterX2->SetOrder(itk::GaussianOrderEnum::SecondOrder);
  filterY2->SetOrder(itk::GaussianOrderEnum::ZeroOrder);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  There are two typical ways of normalizing Gaussians depending on their
  //  application. For scale-space analysis it is desirable to use a
  //  normalization that will preserve the maximum value of the input. This
  //  normalization is represented by the following equation.
  //
  //  \begin{equation}
  //          \frac{ 1 }{ \sigma  \sqrt{ 2 \pi } }
  //  \end{equation}
  //
  //  In applications that use the Gaussian as a solution of the diffusion
  //  equation it is desirable to use a normalization that preserves the
  //  integral of the signal. This last approach can be seen as a conservation
  //  of mass principle. This is represented by the following equation.
  //
  //  \begin{equation}
  //          \frac{ 1 }{ \sigma^2  \sqrt{ 2 \pi } }
  //  \end{equation}
  //
  //  The \doxygen{RecursiveGaussianImageFilter} has a boolean flag that
  //  allows users to select between these two normalization options.
  //  Selection is done with the method \code{SetNormalizeAcrossScale()}.
  //  Enable this flag when analyzing an image across scale-space.  In the
  //  current example, this setting has no impact because we are actually
  //  renormalizing the output to the dynamic range of the reader, so we
  //  simply disable the flag.
  //
  //  \index{RecursiveGaussianImageFilter!SetNormalizeAcrossScale()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const bool normalizeAcrossScale = false;
  filterX1->SetNormalizeAcrossScale(normalizeAcrossScale);
  filterY1->SetNormalizeAcrossScale(normalizeAcrossScale);
  filterX2->SetNormalizeAcrossScale(normalizeAcrossScale);
  filterY2->SetNormalizeAcrossScale(normalizeAcrossScale);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another
  //  filter. Here, an image reader is used as the source. The image is passed
  //  to the $x$ filter and then to the $y$ filter. The reason for keeping
  //  these two filters separate is that it is usual in scale-space
  //  applications to compute not only the smoothing but also combinations of
  //  derivatives at different orders and smoothing. Some factorization is
  //  possible when separate filters are used to generate the intermediate
  //  results. Here this capability is less interesting, though, since we only
  //  want to smooth the image in all directions.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX1->SetInput(reader->GetOutput());
  filterY1->SetInput(filterX1->GetOutput());

  filterY2->SetInput(reader->GetOutput());
  filterX2->SetInput(filterY2->GetOutput());
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  It is now time to select the $\sigma$ of the Gaussian used to smooth the
  //  data.  Note that $\sigma$ must be passed to both filters and that sigma
  //  is considered to be in millimeters. That is, at the moment of applying
  //  the smoothing process, the filter will take into account the spacing
  //  values defined in the image.
  //
  //  \index{itk::RecursiveGaussianImageFilter!SetSigma()}
  //  \index{SetSigma()!itk::RecursiveGaussianImageFilter}
  //
  //  Software Guide : EndLatex

  const double sigma = std::stod(argv[3]);

  // Software Guide : BeginCodeSnippet
  filterX1->SetSigma(sigma);
  filterY1->SetSigma(sigma);
  filterX2->SetSigma(sigma);
  filterY2->SetSigma(sigma);
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the two components of the Laplacian should be added together.
  //  The \doxygen{AddImageFilter} is used for this purpose.
  //
  //  \index{itk::AddImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using AddFilterType =
    itk::AddImageFilter<OutputImageType, OutputImageType, OutputImageType>;

  AddFilterType::Pointer addFilter = AddFilterType::New();

  addFilter->SetInput1(filterY1->GetOutput());
  addFilter->SetInput2(filterX2->GetOutput());
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filters are triggered by invoking \code{Update()} on the Add filter
  //  at the end of the pipeline.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
  {
    addFilter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The resulting image could be saved to a file using the
  //  \doxygen{ImageFileWriter} class.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using WritePixelType = float;

  using WriteImageType = itk::Image<WritePixelType, 2>;

  using WriterType = itk::ImageFileWriter<WriteImageType>;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(addFilter->GetOutput());

  writer->SetFileName(argv[2]);

  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{LaplacianRecursiveGaussianImageFilterOutput3}
  // \includegraphics[width=0.44\textwidth]{LaplacianRecursiveGaussianImageFilterOutput5}
  // \itkcaption[Output of the LaplacianRecursiveGaussianImageFilter.]{Effect
  // of the LaplacianRecursiveGaussianImageFilter on a slice from a MRI proton
  // density image of the brain.}
  // \label{fig:LaplacianRecursiveGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Software Guide : EndLatex


  // Rescale float outputs to png for inclusion in the Software guide
  //
  if (argc > 4)
  {
    using CharPixelType = unsigned char;
    using CharImageType = itk::Image<CharPixelType, 2>;

    using RescaleFilterType =
      itk::RescaleIntensityImageFilter<OutputImageType, CharImageType>;

    RescaleFilterType::Pointer rescale = RescaleFilterType::New();
    rescale->SetInput(addFilter->GetOutput());
    rescale->SetOutputMinimum(0);
    rescale->SetOutputMaximum(255);
    using CharWriterType = itk::ImageFileWriter<CharImageType>;
    CharWriterType::Pointer charWriter = CharWriterType::New();
    charWriter->SetFileName(argv[4]);
    charWriter->SetInput(rescale->GetOutput());
    charWriter->Update();
  }


  return EXIT_SUCCESS;
}
