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
//    OUTPUTS: {SigmoidImageFilterOutput.png}
//    ARGUMENTS:    10 240 10 170
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The \doxygen{SigmoidImageFilter} is commonly used as an intensity
//  transform.  It maps a specific range of intensity values into a new
//  intensity range by making a very smooth and continuous transition in the
//  borders of the range.  Sigmoids are widely used as a mechanism for focusing
//  attention on a particular set of values and progressively attenuating the
//  values outside that range. In order to extend the flexibility of the
//  Sigmoid filter, its implementation in ITK includes four parameters that can
//  be tuned to select its input and output intensity ranges. The following
//  equation represents the Sigmoid intensity transformation, applied
//  pixel-wise.
//
//  \begin{equation}
//  I' = (Max-Min)\cdot \frac{1}{\left(1+e^{-\left(\frac{ I - \beta }{\alpha } \right)} \right)} + Min
//  \end{equation}
//
//  In the equation above, $I$ is the intensity of the input pixel, $I'$ the
//  intensity of the output pixel, $Min,Max$ are the minimum and maximum values
//  of the output image, $\alpha$ defines the width of the input intensity
//  range, and $\beta$ defines the intensity around which the range is
//  centered. Figure~\ref{fig:SigmoidParameters} illustrates the significance
//  of each parameter.
//
// \begin{figure} \center
// \includegraphics[width=0.44\textwidth]{SigmoidParameterAlpha}
// \includegraphics[width=0.44\textwidth]{SigmoidParameterBeta}
// \itkcaption[Sigmoid Parameters]{Effects of the various parameters in the
// SigmoidImageFilter.  The alpha parameter defines the width of the intensity
// window.  The beta parameter defines the center of the intensity window.}
// \label{fig:SigmoidParameters} \end{figure}
//
//  This filter will work on images of any dimension and will take advantage of
//  multiple processors when available.
//
//  \index{itk::SigmoidImageFilter }
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::SigmoidImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkSigmoidImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile";
    std::cerr << " OutputMin OutputMax SigmoidAlpha SigmoidBeta" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Then pixel and image types for the filter input and output must be
  //  defined.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  Using the image types, we instantiate the filter type
  //  and create the filter object.
  //
  //  \index{itk::SigmoidImageFilter!instantiation}
  //  \index{itk::SigmoidImageFilter!New()}
  //  \index{itk::SigmoidImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::SigmoidImageFilter<
               InputImageType, OutputImageType >  SigmoidFilterType;
  SigmoidFilterType::Pointer sigmoidFilter = SigmoidFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The minimum and maximum values desired in the output are defined using the
  //  methods \code{SetOutputMinimum()} and \code{SetOutputMaximum()}.
  //
  //  \index{itk::SigmoidImageFilter!SetOutputMaximum()}
  //  \index{itk::SigmoidImageFilter!SetOutputMinimum()}
  //
  //  Software Guide : EndLatex

  const OutputPixelType outputMinimum = atoi( argv[3] );
  const OutputPixelType outputMaximum = atoi( argv[4] );

  // Software Guide : BeginCodeSnippet
  sigmoidFilter->SetOutputMinimum(   outputMinimum  );
  sigmoidFilter->SetOutputMaximum(   outputMaximum  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The coefficients $\alpha$ and $\beta$ are set with the methods
  //  \code{SetAlpha()} and \code{SetBeta()}.  Note that $\alpha$ is
  //  proportional to the width of the input intensity window.  As rule of
  //  thumb, we may say that the window is the interval $[-3\alpha, 3\alpha]$.
  //  The boundaries of the intensity window are not sharp.  The $\alpha$ curve
  //  approaches its extrema smoothly, as shown in
  //  Figure~\ref{fig:SigmoidParameters}.  You may want to think about this in
  //  the same terms as when taking a range in a population of measures by
  //  defining an interval of $[-3 \sigma, +3 \sigma]$ around the population
  //  mean.
  //
  //  \index{itk::SigmoidImageFilter!SetAlpha()}
  //  \index{itk::SigmoidImageFilter!SetBeta()}
  //
  //  Software Guide : EndLatex

  const double  alpha = atof( argv[5] );
  const double  beta  = atof( argv[6] );

  // Software Guide : BeginCodeSnippet
  sigmoidFilter->SetAlpha(  alpha  );
  sigmoidFilter->SetBeta(   beta   );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input to the SigmoidImageFilter can be taken from any other filter,
  //  such as an image file reader, for example. The output can be passed down the
  //  pipeline to other filters, like an image file writer. An \code{Update()} call on
  //  any downstream filter will trigger the execution of the Sigmoid filter.
  //
  //  \index{itk::SigmoidImageFilter!SetInput()}
  //  \index{itk::SigmoidImageFilter!GetOutput()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  sigmoidFilter->SetInput( reader->GetOutput() );
  writer->SetInput( sigmoidFilter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{SigmoidImageFilterOutput}
  // \itkcaption[Effect of the Sigmoid filter.]{Effect of the Sigmoid filter on a
  // slice from a MRI proton density brain image.}
  // \label{fig:SigmoidImageFilterOutput}
  // \end{figure}
  //
  //  Figure~\ref{fig:SigmoidImageFilterOutput} illustrates the effect of this
  //  filter on a slice of MRI brain image using the following parameters.
  //
  //  \begin{itemize}
  //  \item Minimum =  10
  //  \item Maximum = 240
  //  \item $\alpha$ =  10
  //  \item $\beta$ = 170
  //  \end{itemize}
  //
  //  As can be seen from the figure, the intensities of the white matter
  //  were expanded in their dynamic range, while intensity values lower than
  //  $\beta - 3 \alpha$ and higher than $\beta + 3\alpha$ became progressively
  //  mapped to the minimum and maximum output values. This is the way in which
  //  a Sigmoid can be used for performing smooth intensity windowing.
  //
  //  Note that both $\alpha$ and $\beta$ can be positive and negative. A
  //  negative $\alpha$ will have the effect of \emph{negating} the image. This
  //  is illustrated on the left side of Figure~\ref{fig:SigmoidParameters}. An
  //  application of the Sigmoid filter as preprocessing for segmentation is
  //  presented in Section~\ref{sec:FastMarchingImageFilter}.
  //
  //  Sigmoid curves are common in the natural world.  They represent the
  //  plot of sensitivity to a stimulus. They are also the integral curve of
  //  the Gaussian and, therefore, appear naturally as the response to signals
  //  whose distribution is Gaussian.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
