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
//    OUTPUTS: {OtsuThresholdImageFilterOutput.png}
//    ARGUMENTS:    255 0
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// This example illustrates how to use the \doxygen{OtsuThresholdImageFilter}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkOtsuThresholdImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile ";
    std::cerr << " insideValue    outsideValue   "  << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  The next step is to decide which pixel types to use for the input and output
  //  images, and to define the image dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char  InputPixelType;
  typedef  unsigned char  OutputPixelType;
  const unsigned int      Dimension = 2;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input and output image types are now defined using their respective
  //  pixel types and dimensions.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter type can be instantiated using the input and output image
  //  types defined above.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::OtsuThresholdImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  An \doxygen{ImageFileReader} class is also instantiated in order to read
  //  image data from a file. (See Section \ref{sec:IO} on page
  //  \pageref{sec:IO} for more information about reading
  //  and writing data.)
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // An \doxygen{ImageFileWriter} is instantiated in order to write the output
  // image to a file.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Both the filter and the reader are created by invoking their \code{New()}
  //  methods and assigning the result to \doxygen{SmartPointer}s.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  The image obtained with the reader is passed as input to the
  //  \code{OtsuThresholdImageFilter}.
  //
  //  \index{itk::Otsu\-Threshold\-Image\-Filter!SetInput()}
  //  \index{itk::FileImageReader!GetOutput()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The method \code{SetOutsideValue()} defines the intensity value to be
  //  assigned to those pixels whose intensities are outside the range defined
  //  by the lower and upper thresholds. The method \code{SetInsideValue()}
  //  defines the intensity value to be assigned to pixels with intensities
  //  falling inside the threshold range.
  //
  //  \index{itk::Otsu\-Threshold\-Image\-Filter!SetOutsideValue()}
  //  \index{itk::Otsu\-Threshold\-Image\-Filter!SetInsideValue()}
  //  \index{SetOutsideValue()!itk::Otsu\-Threshold\-Image\-Filter}
  //  \index{SetInsideValue()!itk::Otsu\-Threshold\-Image\-Filter}
  //
  //  Software Guide : EndLatex

  const OutputPixelType outsideValue = atoi( argv[3] );
  const OutputPixelType insideValue  = atoi( argv[4] );

  // Software Guide : BeginCodeSnippet
  filter->SetOutsideValue( outsideValue );
  filter->SetInsideValue(  insideValue  );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Execution of the filter is triggered by invoking the \code{Update()}
  //  method, which we wrap in a \code{try/catch} block.  If the filter's
  //  output has been passed as input to subsequent filters, the \code{Update()}
  //  call on any downstream filters in the pipeline will indirectly trigger
  //  the update of this filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << excp << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We can now retrieve the internally-computed threshold value with the
  //  \code{GetThreshold()} method and print it to the console.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  int threshold = filter->GetThreshold();
  std::cout << "Threshold = " << threshold << std::endl;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{OtsuThresholdImageFilterOutput}
  // \itkcaption[OtsuThresholdImageFilter output]{Effect of the
  // OtsuThresholdImageFilter on a slice from a MRI proton density image  of
  // the brain.}
  // \label{fig:OtsuThresholdImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:OtsuThresholdImageFilterInputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain. This
  //  figure shows the limitations of this filter for performing segmentation
  //  by itself. These limitations are particularly noticeable in noisy images
  //  and in images lacking spatial uniformity as is the case with MRI due to
  //  field bias.
  //
  //  \relatedClasses
  //  \begin{itemize}
  //  \item \doxygen{ThresholdImageFilter}
  //  \end{itemize}
  //
  //  Software Guide : EndLatex

  writer->SetFileName( argv[2] );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << excp << std::endl;
    }

  return EXIT_SUCCESS;
}
