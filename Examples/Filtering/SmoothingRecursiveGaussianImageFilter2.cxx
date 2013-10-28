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

//  Software Guide : BeginLatex
//
//  Setting up a pipeline of $m$ filters in order to smooth an N-dimensional
//  image may be a lot of work to do for achieving a simple goal. In order to
//  avoid this inconvenience, a filter packaging this $m$ filters internally
//  is available. This filter is the
//  \doxygen{SmoothingRecursiveGaussianImageFilter}.
//
//  \index{itk::SmoothingRecursiveGaussianImageFilter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  In order to use this filter the following header file must be included.
//
//  \index{itk::SmoothingRecursiveGaussianImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSmoothingRecursiveGaussianImageFilter.h"
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
  //  Appropriate pixel types must be selected to support input and output
  //  images.
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
  //  output image types. If the second template parameter is omitted, the
  //  filter will assume that the output image has the same type as the input
  //  image.
  //
  //  \index{itk::SmoothingRecursiveGaussianImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::SmoothingRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  Now a single filter is enough for smoothing the image along all the
  //  dimensions.  The filter is created by invoking the \code{New()} method
  //  and assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::SmoothingRecursiveGaussianImageFilter!New()}
  //  \index{itk::SmoothingRecursiveGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  As in the previous examples we should decide what type of normalization
  //  to use during the computation of the Gaussians. The method
  //  \code{SetNormalizeAcrossScale()} serves this purpose.
  //  \index{SmoothingRecursiveGaussianImageFilter!SetNormalizeAcrossScale()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNormalizeAcrossScale( false );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source. The image is passed directly to the
  //  smoothing filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  It is now time for selecting the $\sigma$ of the Gaussian to use for
  //  smoothing the data. Note that $\sigma$ is considered to be in
  //  millimeters.  That is, at the moment of applying the smoothing process,
  //  the filter will take into account the spacing values defined in the
  //  image.
  //
  //  \index{itk::SmoothingRecursiveGaussianImageFilter!SetSigma()}
  //  \index{SetSigma()!itk::SmoothingRecursiveGaussianImageFilter}
  //
  //  Software Guide : EndLatex

  const double sigma = atof( argv[3] );

  // Software Guide : BeginCodeSnippet
  filter->SetSigma( sigma );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the pipeline is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::SmoothingRecursiveGaussianImageFilter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  typedef  unsigned char                             WritePixelType;
  typedef itk::Image< WritePixelType, 2 >            WriteImageType;
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
  // \includegraphics[width=0.44\textwidth]{SmoothingRecursiveGaussianImageFilterOutput3}
  // \includegraphics[width=0.44\textwidth]{SmoothingRecursiveGaussianImageFilterOutput5}
  // \itkcaption[SmoothingRecursiveGaussianImageFilter output]{Effect of the
  // SmoothingRecursiveGaussianImageFilter on a slice from a MRI proton density image of the brain.}
  // \label{fig:SmoothingRecursiveGaussianImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:SmoothingRecursiveGaussianImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain using a $\sigma$ value of $3$ (left) and a value of $5$
  //  (right).  The figure shows how the attenuation of noise can be
  //  regulated by selecting an appropriate sigma.  This type of scale-tunable
  //  filter is suitable for performing scale-space analysis.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
