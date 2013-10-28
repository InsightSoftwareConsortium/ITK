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
//    OUTPUTS: {SmoothingRecursiveGaussianImageFilterOutput3.png}
//    ARGUMENTS:    3
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {SmoothingRecursiveGaussianImageFilterOutput5.png}
//    ARGUMENTS:    5
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The classical method of smoothing an image by convolution with a Gaussian
//  kernel has the drawback that it is slow when the standard deviation $\sigma$ of
//  the Gaussian is large.  This is due to the larger size of the kernel,
//  which results in a higher number of computations per pixel.
//
//  The \doxygen{RecursiveGaussianImageFilter} implements an approximation of
//  convolution with the Gaussian and its derivatives by using
//  IIR\footnote{Infinite Impulse Response} filters. In practice this filter
//  requires a constant number of operations for approximating the convolution,
//  regardless of the $\sigma$ value \cite{Deriche1990,Deriche1993}.
//
//  \index{itk::RecursiveGaussianImageFilter}
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
//  \index{itk::RecursiveGaussianImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  sigma " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be selected on the desired input and output pixel types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input and output image types are instantiated using the pixel types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::RecursiveGaussianImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  This filter applies the approximation of the convolution along a single
  //  dimension. It is therefore necessary to concatenate several of these filters
  //  to produce smoothing in all directions.  In this example, we create a pair
  //  of filters since we are processing a $2D$ image.  The filters are
  //  created by invoking the \code{New()} method and assigning the result to
  //  a \doxygen{SmartPointer}.
  //
  //  \index{itk::RecursiveGaussianImageFilter!New()}
  //  \index{itk::RecursiveGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filterX = FilterType::New();
  FilterType::Pointer filterY = FilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Since each one of the newly created filters has the potential to perform
  //  filtering along any dimension, we have to restrict each one to a
  //  particular direction. This is done with the \code{SetDirection()} method.
  //
  //  \index{RecursiveGaussianImageFilter!SetDirection()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX->SetDirection( 0 );   // 0 --> X direction
  filterY->SetDirection( 1 );   // 1 --> Y direction
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{RecursiveGaussianImageFilter} can approximate the
  //  convolution with the Gaussian or with its first and second
  //  derivatives. We select one of these options by using the
  //  \code{SetOrder()} method. Note that the argument is an \code{enum} whose
  //  values can be \code{ZeroOrder}, \code{FirstOrder} and
  //  \code{SecondOrder}. For example, to compute the $x$ partial derivative we
  //  should select \code{FirstOrder} for $x$ and \code{ZeroOrder} for
  //  $y$. Here we want only to smooth in $x$ and $y$, so we select
  //  \code{ZeroOrder} in both directions.
  //
  //  \index{RecursiveGaussianImageFilter!SetOrder()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX->SetOrder( FilterType::ZeroOrder );
  filterY->SetOrder( FilterType::ZeroOrder );
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
  //  equation it is desirable to use a normalization that preserve the
  //  integral of the signal. This last approach can be seen as a conservation
  //  of mass principle. This is represented by the following equation.
  //
  //  \begin{equation}
  //          \frac{ 1 }{ \sigma^2  \sqrt{ 2 \pi } }
  //  \end{equation}
  //
  //  The \doxygen{RecursiveGaussianImageFilter} has a boolean flag that allows
  //  users to select between these two normalization options. Selection is
  //  done with the method \code{SetNormalizeAcrossScale()}. Enable this flag
  //  to analyzing an image across scale-space.  In the current example, this
  //  setting has no impact because we are actually renormalizing the output to
  //  the dynamic range of the reader, so we simply disable the flag.
  //
  //  \index{RecursiveGaussianImageFilter!SetNormalizeAcrossScale()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX->SetNormalizeAcrossScale( false );
  filterY->SetNormalizeAcrossScale( false );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another
  //  filter. Here, an image reader is used as the source. The image is passed to
  //  the $x$ filter and then to the $y$ filter. The reason for keeping these
  //  two filters separate is that it is usual in scale-space applications to
  //  compute not only the smoothing but also combinations of derivatives at
  //  different orders and smoothing. Some factorization is possible when
  //  separate filters are used to generate the intermediate results. Here
  //  this capability is less interesting, though, since we only want to smooth
  //  the image in all directions.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filterX->SetInput( reader->GetOutput() );
  filterY->SetInput( filterX->GetOutput() );
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

  const double sigma = atof( argv[3] );

  // Software Guide : BeginCodeSnippet
  filterX->SetSigma( sigma );
  filterY->SetSigma( sigma );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the pipeline is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::RecursiveGaussianImageFilter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filterY->Update();
  // Software Guide : EndCodeSnippet


  typedef unsigned char                              WritePixelType;
  typedef itk::Image< WritePixelType, 2 >            WriteImageType;
  typedef itk::RescaleIntensityImageFilter<
                   OutputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );

  rescaler->SetInput( filterY->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{SmoothingRecursiveGaussianImageFilterOutput3}
  // \includegraphics[width=0.44\textwidth]{SmoothingRecursiveGaussianImageFilterOutput5}
  // \itkcaption[Output of the SmoothingRecursiveGaussianImageFilter.]{Effect of the
  // SmoothingRecursiveGaussianImageFilter on a slice from a MRI proton density image
  // of the brain.}
  // \label{fig:SmoothingRecursiveGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure~\ref{fig:SmoothingRecursiveGaussianImageFilterInputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain using
  //  $\sigma$ values of $3$ (left) and $5$ (right).  The figure shows how the
  //  attenuation of noise can be regulated by selecting the appropriate
  //  standard deviation.  This type of scale-tunable filter is suitable for
  //  performing scale-space analysis.
  //
  //  The RecursiveGaussianFilters can also be applied on multi-component images. For instance,
  //  the above filter could have applied with RGBPixel as the pixel type. Each component is
  //  then independently filtered. However the RescaleIntensityImageFilter will not work on
  //  RGBPixels since it does not mathematically make sense to rescale the output
  //  of multi-component images.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
