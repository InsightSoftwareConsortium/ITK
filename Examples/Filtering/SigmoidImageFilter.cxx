/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SigmoidImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The \doxygen{SigmoidImageFilter} is commonly used as an intensity
//  transform.  It maps a specific range of intensity values into a new
//  intensity range by making a very smooth and continuous transition in the
//  borders of the range.  Sigmoids are widely use as a mechanism for focusing
//  attention in a particular set of values and progressively attenuate the
//  values outside that range. In order to extend the flexibility of this
//  filter, its implementation in ITK includes four parameters that can be
//  tunned to select the input intensity range and the output intensity range
//  of the filter. The following equation represents the intensity
//  transformation applied pixel-wise by this filter.  
//
//  \begin{equation}
//  I' = (Max-Min)\cdot \frac{1}{\left(1+e^{-\left(\frac{ I - \beta }{\alpha } \right)} \right)} + Min
//  \end{equation}
//  
//  Where $I$ is the intensity of the input pixel, $I'$ the intensity of the
//  output pixel, $Min,Max$ are the minimum and maximum values of the output
//  image, $\alpha$ defines the width of the intensity range to be accepted at
//  the input and $\beta$ defines the intensity around which the range is
//  centered. The significance of each one of these parameters can be
//  better appreciated in Figure~\ref{fig:SigmoidParameters}
//
// \begin{figure}
// \center
// \includegraphics[width=0.44\textwidth]{SigmoidParameterAlpha.eps}
// \includegraphics[width=0.44\textwidth]{SigmoidParameterBeta.eps} 
// \caption[Sigmoid Parameters]{Effect of various parameters in the Sigmoid.
// The alpha parameter is associated with the width of intensity window while
// the beta parameter is associated with the center of such intensity window.}
// \label{fig:SigmoidParameters}
// \end{figure}
//
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
    return 1;
    }


  //  Software Guide : BeginLatex
  //
  //  Then the pixel types for input and output image must be defined and with
  //  them the image types can be instantiated.
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
  //  The minimum and maximum values desired at the output are defined with the
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
  //  The coefficients $\alpha$ and $\beta$ are passed with the methods
  //  \code{SetAlpha()} and \code{SetBeta()}.  Note that $\alpha$ is not
  //  exactly the width of the input intensity window but it is proportional to
  //  it. As a rule of thumb we may say that the actual window is the interval
  //  $[-3\alpha, 3\alpha]$, with the remark that the intensity window ends are
  //  not crisp but rather have a continuous fading as shown in
  //  Figure~\ref{fig:SigmoidParameters}.  You may want to think about this in
  //  the same terms you do when you take a range in a population of measures
  //  by defining and interval of $[-3 \sigma, +3 \sigma]$ around the mean of
  //  the population.
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
  //  The input to the filter can be taken from any other filter, for example a
  //  reader. The output can be passed down the pipeline to other filters, for
  //  example a writer. An update call on any downstream filter will trigger
  //  the execution of the Sigmoid filter.
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
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=0.44\textwidth]{SigmoidImageFilterOutput.eps}
  // \caption{Effect of the Sigmoid filter on a slice from a MRI
  // Proton Density brain image.}
  // \label{fig:SigmoidImageFilterOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:SigmoidImageFilterOutput} illustrate the effect of this
  //  filter on a slice of MRI brain image using the following parameters
  //
  //  \begin{itemize}
  //  \item Minimum =  10
  //  \item Maximum = 240
  //  \item $\alpha$ =  10
  //  \item $\beta$ = 170
  //  \end{itemize}
  //
  //  As can be seen from the Figure, the intensities on the white matter
  //  became expanded in their dynamic range, while intensity values lower than
  //  $\beta - 3 \alpha$ and higher than $\beta + 3\alpha$ became progressively
  //  mapped to the minimum and maximum output values. This is the way in which
  //  a Sigmoid can be used for performing smooth intensity windowing.
  // 
  //  Note that both $\alpha$ and $\beta$ can be positive and negative. A
  //  negative $\alpha$ will have the effect of \emph{negating} the image. This
  //  is illustrated on the left side of Figure\ref{fig:SigmoidParameters}. An
  //  application of this filter as pre-processing for segmentation is
  //  presented in section~\ref{sec:FastMarchingImageFilter}.
  //
  //  Sigmoid are ubiquitous in nature. They represent the curve of sensitivity
  //  to any stimulus. They are also the integral curve of the Gaussian and
  //  henceforth appear naturally as the response to signals whose distribution
  //  is Gaussian.
  //
  //  Software Guide : EndLatex 


  return 0;

}

