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
//    OUTPUTS: {BilateralImageFilterOutput.png}
//    ARGUMENTS:    6 5
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The \doxygen{BilateralImageFilter} performs smoothing by using both
//  domain and range neighborhoods.  Pixels that are close to a pixel in the
//  image domain and similar to a pixel in the image range are used to
//  calculate the filtered value. Two Gaussian kernels (one in the image
//  domain and one in the image range) are used to smooth the image. The
//  result is an image that is smoothed in homogeneous regions yet has edges
//  preserved. The result is similar to anisotropic diffusion but the
//  implementation is non-iterative.  Another benefit to bilateral filtering
//  is that any distance metric can be used for kernel smoothing the image
//  range. Bilateral filtering is capable of reducing the noise in an image
//  by an order of magnitude while maintaining edges.  The bilateral operator
//  used here was described by Tomasi and Manduchi (\emph{Bilateral Filtering
//  for Gray and Color Images}. IEEE ICCV. 1998.)
//
//  The filtering operation can be described by the following equation
//
//  \begin{equation}
//  h(\mathbf{x}) = k(\mathbf{x})^{-1} \int_\omega f(\mathbf{w})
//  c(\mathbf{x},\mathbf{w}) s( f(\mathbf{x}),f(\mathbf{w})) d \mathbf{w}
//  \end{equation}
//
//  where $\mathbf{x}$ holds the coordinates of a $ND$ point, $f(\mathbf{x})$
//  is the input image and $h(\mathbf{x})$ is the output image. The
//  convolution kernels $c()$ and $s()$ are associated with the spatial and
//  intensity domain respectively. The $ND$ integral is computed over
//  $\omega$ which is a neighborhood of the pixel located at
//  $\mathbf{x}$. The normalization factor $k(\mathbf{x})$ is computed as
//
//  \begin{equation}
//  k(\mathbf{x}) = \int_\omega c(\mathbf{x},\mathbf{w})
//  s( f(\mathbf{x}),f(\mathbf{w})) d \mathbf{w}
//  \end{equation}
//
//  The default implementation of this filter uses Gaussian kernels for both
//  $c()$ and $s()$. The $c$ kernel can be described as
//
//  \begin{equation}
//  c(\mathbf{x},\mathbf{w}) = e^{(\frac{ {\left|| \mathbf{x} - \mathbf{w} \right||}^2 }{\sigma^2_c} )}
//  \end{equation}
//
//  where $\sigma_c$ is provided by the user and defines how close pixel
//  neighbors should be in order to be considered for the computation of the
//  output value.  The $s$ kernel is given by
//
//  \begin{equation}
//  s(f(\mathbf{x}),f(\mathbf{w})) = e^{(\frac{ {( f(\mathbf{x}) - f(\mathbf{w})}^2 }{\sigma^2_s} )}
//  \end{equation}
//
//  where $\sigma_s$ is provided by the user and defines how close the
//  neighbor's intensity be in order to be considered for the computation of
//  the output value.
//
//  \index{itk::BilateralImageFilter}
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
//  \index{itk::BilateralImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkBilateralImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  domainSigma  rangeSigma" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  The image types are instantiated using pixel type and dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef    unsigned char    InputPixelType;
  typedef    unsigned char    OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The bilateral filter type is now instantiated using both the input
  //  image and the output image types and the filter object is created.
  //
  //  \index{itk::BilateralImageFilter!instantiation}
  //  \index{itk::BilateralImageFilter!New()}
  //  \index{itk::BilateralImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BilateralImageFilter<
               InputImageType, OutputImageType >  FilterType;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another
  //  filter. Here, an image reader is used as a source.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The Bilateral filter requires two parameters. First, we must specify the
  //  standard deviation $\sigma$ to be used for the Gaussian kernel on image
  //  intensities. Second, the set of $\sigma$s to be used along each dimension
  //  in the space domain. This second parameter is supplied as an array of
  //  \code{float} or \code{double} values. The array dimension matches the
  //  image dimension. This mechanism makes it possible to enforce more
  //  coherence along some directions. For example, more smoothing can be done
  //  along the $X$ direction than along the $Y$ direction.
  //
  //  In the following code example, the $\sigma$ values are taken from the
  //  command line.  Note the use of \code{ImageType::ImageDimension} to get
  //  access to the image dimension at compile time.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = InputImageType::ImageDimension;
  double domainSigmas[ Dimension ];
  for(unsigned int i=0; i<Dimension; i++)
    {
    domainSigmas[i] = atof( argv[3] );
    }
  const double rangeSigma = atof( argv[4] );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter parameters are set with the methods \code{SetRangeSigma()}
  //  and \code{SetDomainSigma()}.
  //
  //  \index{itk::BilateralImageFilter!SetRangeSigma()}
  //  \index{itk::BilateralImageFilter!SetDomainSigma()}
  //  \index{SetDomainSigma()!itk::BilateralImageFilter}
  //  \index{SetRangeSigma()!itk::BilateralImageFilter}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->SetDomainSigma( domainSigmas );
  filter->SetRangeSigma(  rangeSigma   );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The output of the filter is connected here to a intensity rescaler
  //  filter and then to a writer. Invoking \code{Update()} on the writer
  //  triggers the execution of both filters.
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
  // \includegraphics[width=0.44\textwidth]{BilateralImageFilterOutput}
  // \itkcaption[BilateralImageFilter output]{Effect of the BilateralImageFilter
  // on a slice from a MRI proton density image  of the brain.}
  // \label{fig:BilateralImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:BilateralImageFilterInputOutput} illustrates the effect
  //  of this filter on a MRI proton density image of the brain. In this
  //  example the filter was run with a range $\sigma$ of $5.0$ and a domain
  //  $\sigma$ of $6.0$.  The figure shows how homogeneous regions are
  //  smoothed and edges are preserved.
  //
  //  \relatedClasses
  //  \begin{itemize}
  //  \item \doxygen{GradientAnisotropicDiffusionImageFilter}
  //  \item \doxygen{CurvatureAnisotropicDiffusionImageFilter}
  //  \item \doxygen{CurvatureFlowImageFilter}
  //  \end{itemize}
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
