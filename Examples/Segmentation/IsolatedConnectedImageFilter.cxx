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
//    OUTPUTS: {IsolatedConnectedImageFilterOutput1.png}
//    ARGUMENTS:    61 140 150 63 43
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// The following example illustrates the use of the
// \doxygen{IsolatedConnectedImageFilter}.  This filter is a close variant of
// the \doxygen{ConnectedThresholdImageFilter}.  In this filter two seeds and a
// lower threshold are provided by the user. The filter will grow a region
// connected to the first seed and \textbf{not connected} to the second one. In
// order to do this, the filter finds an intensity value that could be used as
// upper threshold for the first seed. A binary search is used to find the
// value that separates both seeds.
//
// This example closely follows the previous ones. Only the relevant pieces
// of code are highlighted here.
//
// Software Guide : EndLatex

//  Software Guide : BeginLatex
//
//  The header of the IsolatedConnectedImageFilter is included below.
//
//  \index{itk::Isolated\-Connected\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkIsolatedConnectedImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"
#include "itkCastImageFilter.h"
#include "itkCurvatureFlowImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main( int argc, char *argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage seedX1 seedY1";
    std::cerr << " lowerThreshold seedX2 seedY2" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We define the image type using a pixel type and a particular
  //  dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet


  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::CastImageFilter< InternalImageType, OutputImageType >
                                                   CastingFilterType;

  CastingFilterType::Pointer caster = CastingFilterType::New();


  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  typedef itk::CurvatureFlowImageFilter< InternalImageType, InternalImageType >
    CurvatureFlowImageFilterType;
  CurvatureFlowImageFilterType::Pointer smoothing =
                         CurvatureFlowImageFilterType::New();


  //  Software Guide : BeginLatex
  //
  //  The \code{IsolatedConnectedImageFilter} is instantiated in the lines below.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::IsolatedConnectedImageFilter<InternalImageType,
                                       InternalImageType> ConnectedFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  One filter of this class is constructed using the \code{New()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer isolatedConnected = ConnectedFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now it is time to connect the pipeline.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );
  isolatedConnected->SetInput( smoothing->GetOutput() );
  caster->SetInput( isolatedConnected->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  // Software Guide : EndCodeSnippet


  smoothing->SetNumberOfIterations( 5 );
  smoothing->SetTimeStep( 0.125 );


  //  Software Guide : BeginLatex
  //
  //  The \code{IsolatedConnectedImageFilter} expects the user to specify a
  //  threshold and two seeds. In this example, we take all of them from the
  //  command line arguments.
  //
  //  \index{itk::Isolated\-Connected\-Image\-Filter!SetLower()}
  //  \index{itk::Isolated\-Connected\-Image\-Filter!AddSeed1()}
  //  \index{itk::Isolated\-Connected\-Image\-Filter!AddSeed2()}
  //
  //  Software Guide : EndLatex


  InternalImageType::IndexType  indexSeed1;

  indexSeed1[0] = atoi( argv[3] );
  indexSeed1[1] = atoi( argv[4] );

  const InternalPixelType lowerThreshold = atof( argv[5] );

  InternalImageType::IndexType  indexSeed2;

  indexSeed2[0] = atoi( argv[6] );
  indexSeed2[1] = atoi( argv[7] );


  // Software Guide : BeginCodeSnippet
  isolatedConnected->SetLower(  lowerThreshold  );
  isolatedConnected->AddSeed1( indexSeed1 );
  isolatedConnected->AddSeed2( indexSeed2 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  As in the \doxygen{ConnectedThresholdImageFilter} we must now specify
  //  the intensity value to be set on the output pixels and at least one
  //  seed point to define the initial region.
  //
  //  \index{itk::Isolated\-Connected\-Image\-Filter!SetReplaceValue()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  isolatedConnected->SetReplaceValue( 255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The invocation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The intensity value allowing us to separate both regions can be
  //  recovered with the method \code{GetIsolatedValue()}.
  //
  //  \index{itk::Isolated\-Connected\-Image\-Filter!GetIsolatedValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Isolated Value Found = ";
  std::cout << isolatedConnected->GetIsolatedValue()  << std::endl;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Let's now run this example using the image
  //  \code{BrainProtonDensitySlice.png} provided in the directory
  //  \code{Examples/Data}. We can easily segment the major anatomical
  //  structures by providing seed pairs in the appropriate locations and
  //  defining values for the lower threshold. It is important to keep in
  //  mind in this and the previous examples that the segmentation is being
  //  performed using the smoothed version of the image. The selection of
  //  threshold values should therefore be performed in the smoothed image
  //  since the distribution of intensities could be quite different from
  //  that of the input image.  As a reminder of this fact, Figure
  //  \ref{fig:IsolatedConnectedImageFilterOutput} presents, from left to
  //  right, the input image and the result of smoothing with the
  //  \doxygen{CurvatureFlowImageFilter} followed by segmentation results.
  //
  //  This filter is intended to be used in cases where adjacent anatomical
  //  structures are difficult to separate. Selecting one seed in one structure
  //  and the other seed in the adjacent structure creates the appropriate
  //  setup for computing the threshold that will separate both structures.
  //  Table~\ref{tab:IsolatedConnectedImageFilterOutput} presents the
  //  parameters used to obtain the images shown in
  //  Figure~\ref{fig:IsolatedConnectedImageFilterOutput}.
  //
  //  \begin{table}
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|}
  //  \hline
  //  Adjacent Structures & Seed1 & Seed2 & Lower & Isolated value found       \\ \hline
  //  Gray matter vs White matter & $(61,140)$ & $(63,43)$ & $150$ & $183.31$  \\ \hline
  //  \end{tabular}
  //  \end{center}
  //  \itkcaption[IsolatedConnectedImageFilter example parameters]{Parameters
  //  used for separating white matter from gray matter in
  //  Figure~\ref{fig:IsolatedConnectedImageFilterOutput} using the
  //  IsolatedConnectedImageFilter.\label{tab:IsolatedConnectedImageFilterOutput}}
  //  \end{table}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.32\textwidth]{IsolatedConnectedImageFilterOutput0}
  // \includegraphics[width=0.32\textwidth]{IsolatedConnectedImageFilterOutput1}
  // \itkcaption[IsolatedConnected segmentation results]{Segmentation results of
  // the IsolatedConnectedImageFilter.}
  // \label{fig:IsolatedConnectedImageFilterOutput}
  // \end{figure}
  //
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
