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
//    OUTPUTS: {FlipImageFilterOutput.png}
//    ARGUMENTS:    0 1
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The \doxygen{FlipImageFilter} is used for flipping the image content in any
//  of the coordinate axes. This filter must be used with \textbf{EXTREME}
//  caution. You probably don't want to appear in the newspapers as
//  responsible for a surgery mistake in which a doctor extirpates the left
//  kidney when he should have extracted the right one\footnote{\emph{Wrong
//  side} surgery accounts for $2\%$ of the reported medical errors in the United
//  States. Trivial... but equally dangerous.} . If that prospect doesn't
//  scare you, maybe it is time for you to reconsider your career in medical
//  image processing. Flipping effects which seem innocuous at first view may
//  still have dangerous consequences. For example, flipping the cranio-caudal
//  axis of a CT scan forces an observer to flip the left-right axis in order
//  to make sense of the image.
//
//  \index{itk::FlipImageFilter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::FlipImageFilter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkFlipImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile   outputImageFile   flipAxisX   flipAxisY" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Then the pixel types for input and output image must be defined and, with
  //  them, the image types can be instantiated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   unsigned char  PixelType;

  typedef itk::Image< PixelType,  2 >   ImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  //  Software Guide : BeginLatex
  //
  //  Using the image types it is now possible to instantiate the filter type
  //  and create the filter object.
  //
  //  \index{itk::FlipImageFilter!instantiation}
  //  \index{itk::FlipImageFilter!New()}
  //  \index{itk::FlipImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::FlipImageFilter< ImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The axes to flip are specified in the form of an Array. In this case we
  //  take them from the command line arguments.
  //
  //  \index{itk::FlipImageFilter!Radius}
  //  \index{itk::FlipImageFilter!Neighborhood}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef FilterType::FlipAxesArrayType     FlipAxesArrayType;

  FlipAxesArrayType flipArray;

  flipArray[0] = atoi( argv[3] );
  flipArray[1] = atoi( argv[4] );

  filter->SetFlipAxes( flipArray );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The input to the filter can be taken from any other filter, for example
  //  a reader. The output can be passed down the pipeline to other filters,
  //  for example, a writer. Invoking \code{Update()} on any downstream filter
  //  will trigger the execution of the FlipImage filter.
  //
  //  \index{itk::FlipImageFilter!SetInput()}
  //  \index{itk::FlipImageFilter!GetOutput()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.44\textwidth]{FlipImageFilterOutput}
  // \itkcaption[Effect of the FlipImageFilter]{Effect of the FlipImageFilter on a slice
  // from a MRI proton density brain image.}
  // \label{fig:FlipImageFilterOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:FlipImageFilterOutput} illustrates the effect of this
  //  filter on a slice of an MRI brain image using a flip array $[0,1]$ which
  //  means that the $Y$ axis was flipped while the $X$ axis was conserved.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
