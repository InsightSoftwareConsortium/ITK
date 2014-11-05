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
//    OUTPUTS: {GradientMagnitudeRecursiveGaussianImageFilterOutput3.png}
//    ARGUMENTS:    3
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {GradientMagnitudeRecursiveGaussianImageFilterOutput5.png}
//    ARGUMENTS:    5
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  Differentiation is an ill-defined operation over digital data. In practice
//  it is convenient to define a scale in which the differentiation should be
//  performed. This is usually done by preprocessing the data with a smoothing
//  filter. It has been shown that a Gaussian kernel is the most convenient
//  choice for performing such smoothing. By choosing a particular value for
//  the standard deviation ($\sigma$) of the Gaussian, an associated scale is
//  selected that ignores high frequency content, commonly considered image
//  noise.
//
//  The \doxygen{GradientMagnitudeRecursiveGaussianImageFilter} computes the
//  magnitude of the image gradient at each pixel location.  The computational
//  process is equivalent to first smoothing the image by convolving it with a
//  Gaussian kernel and then applying a differential operator.  The user
//  selects the value of $\sigma$.
//
//  Internally this is done by applying an IIR \footnote{Infinite Impulse
//  Response} filter that approximates a convolution with the derivative of the
//  Gaussian kernel.  Traditional convolution will produce a more accurate
//  result, but the IIR approach is much faster, especially using large
//  $\sigma$s \cite{Deriche1990,Deriche1993}.
//
//  GradientMagnitudeRecursiveGaussianImageFilter will work on images of
//  any dimension by taking advantage of the natural separability of the
//  Gaussian kernel and its derivatives.
//
//  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header
//  file.
//
//  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile   sigma" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be instantiated based on the pixels of the input and
  //  output images.
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
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!New()}
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!Pointer}
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


  //  Software Guide : BeginLatex
  //
  //  The standard deviation of the Gaussian smoothing kernel is now set.
  //
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!SetSigma()}
  //  \index{SetSigma()!itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter}
  //
  //  Software Guide : EndLatex
  const double sigma = atof( argv[3] );


  // Software Guide : BeginCodeSnippet
  filter->SetSigma( sigma );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the filter is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::Gradient\-Magnitude\-Recursive\-Gaussian\-Image\-Filter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  If connected to other filters in a pipeline, this filter will
  //  automatically update when any downstream filters are updated.  For
  //  example, we may connect this gradient magnitude filter to an image file
  //  writer and then update the writer.
  //
  //  Software Guide : EndLatex


  typedef unsigned char                   WritePixelType;
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
  // \includegraphics[width=0.44\textwidth]{GradientMagnitudeRecursiveGaussianImageFilterOutput3}
  // \includegraphics[width=0.44\textwidth]{GradientMagnitudeRecursiveGaussianImageFilterOutput5}
  // \itkcaption[GradientMagnitudeRecursiveGaussianImageFilter output]{Effect of
  // the GradientMagnitudeRecursiveGaussianImageFilter on a slice from a MRI
  // proton density image of the brain.}
  // \label{fig:GradientMagnitudeRecursiveGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure
  //  \ref{fig:GradientMagnitudeRecursiveGaussianImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain using $\sigma$ values of $3$ (left) and $5$
  //  (right).  The figure shows how the sensitivity to noise can be
  //  regulated by selecting an appropriate $\sigma$.  This type of
  //  scale-tunable filter is suitable for performing scale-space analysis.
  //
  // Attention should be paid to the image type chosen to represent the output
  //  image since the dynamic range of the gradient magnitude image is usually
  //  smaller than the dynamic range of the input image.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
