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
//    INPUTS: {BrainProtonDensitySlice.png}
//    OUTPUTS: {ThresholdImageFilterOutputBelow.png}
//    OUTPUTS: {ThresholdImageFilterOutputAbove.png}
//    OUTPUTS: {ThresholdImageFilterOutputOutside.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// \begin{figure}
// \center
// \includegraphics[height=5cm]{ThresholdTransferFunctionBelow}
// \includegraphics[height=5cm]{ThresholdImageFilterOutputBelow}
// \itkcaption[ThresholdImageFilter using the threshold-below
// mode.]{ThresholdImageFilter using the threshold-below mode.}
// \label{fig:ThresholdTransferFunctionBelow}
// \end{figure}
//
// \begin{figure}
// \center
// \includegraphics[height=5cm]{ThresholdTransferFunctionAbove}
// \includegraphics[height=5cm]{ThresholdImageFilterOutputAbove}
// \itkcaption[ThresholdImageFilter using the threshold-above
// mode]{ThresholdImageFilter using the threshold-above mode.}
// \label{fig:ThresholdTransferFunctionAbove}
// \end{figure}
//
// \begin{figure}
// \center
// \includegraphics[height=5cm]{ThresholdTransferFunctionOutside}
// \includegraphics[height=5cm]{ThresholdImageFilterOutputOutside}
// \itkcaption[ThresholdImageFilter using the threshold-outside
// mode]{ThresholdImageFilter using the threshold-outside mode.}
// \label{fig:ThresholdTransferFunctionOutside}
// \end{figure}
//
// This example illustrates the use of the \doxygen{ThresholdImageFilter}.
// This filter can be used to transform the intensity levels of an image in
// three different ways.
//
// \begin{itemize}
//
// \item First, the user can define a single threshold.  Any pixels with values
// below this threshold will be replaced by a user defined value, called here the
// \code{OutsideValue}.  Pixels with values above the threshold remain
// unchanged. This type of thresholding is illustrated in
// Figure~\ref{fig:ThresholdTransferFunctionBelow}.
//
// \item Second, the user can define a particular threshold such that all the
// pixels with values above the threshold will be replaced by the
// \code{OutsideValue}.  Pixels with values below the threshold remain
// unchanged. This is illustrated in Figure~\ref{fig:ThresholdTransferFunctionAbove}.
//
// \item Third, the user can provide two thresholds. All the pixels with
// intensity values inside the range defined by the two thresholds will remain
// unchanged. Pixels with values outside this range will be assigned to the
// \code{OutsideValue}. This is illustrated in
// Figure~\ref{fig:ThresholdTransferFunctionOutside}.
//
// \end{itemize}
//
// The following methods choose among the three operating modes of the filter.
//
// \begin{itemize}
// \item \texttt{ThresholdBelow()}
// \item \texttt{ThresholdAbove()}
// \item \texttt{ThresholdOutside()}
// \end{itemize}
//
// \index{itk::ThresholdImageFilter!Instantiation}
// \index{itk::ThresholdImageFilter!Header}
// \index{itk::ThresholdImageFilter!ThresholdAbove()}
// \index{itk::ThresholdImageFilter!ThresholdBelow()}
// \index{itk::ThresholdImageFilter!ThresholdOutside()}
//
// The first step required to use this filter is to include its header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkThresholdImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char * argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0] << " inputImageFile ";
    std::cerr << " outputImageFile1 outputImageFile2 outputImageFile3" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Then we must decide what pixel type to use for the image. This filter is
  //  templated over a single image type because the algorithm only modifies
  //  pixel values outside the specified range, passing the rest through
  //  unchanged.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef  unsigned char  PixelType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The image is defined using the pixel type and the dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType,  2 >   ImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter can be instantiated using the image type defined above.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ThresholdImageFilter< ImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  An \doxygen{ImageFileReader} class is also instantiated in order to read
  //  image data from a file.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // An \doxygen{ImageFileWriter} is instantiated in order to write the
  // output image to a file.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< ImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Both the filter and the reader are created by invoking their \code{New()}
  //  methods and assigning the result to SmartPointers.
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
  //  \doxygen{ThresholdImageFilter}.
  //
  //  \index{itk::ThresholdImageFilter!SetInput()}
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
  //  by the lower and upper thresholds.
  //
  //  \index{itk::ThresholdImageFilter!SetOutsideValue()}
  //  \index{SetOutsideValue()!itk::ThresholdImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetOutsideValue( 0 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The method \code{ThresholdBelow()} defines the intensity value below
  //  which pixels of the input image will be changed to the \code{OutsideValue}.
  //
  //  \index{itk::ThresholdImageFilter!ThresholdBelow()}
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->ThresholdBelow( 180 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The filter is executed by invoking the \code{Update()} method. If the
  //  filter is part of a larger image processing pipeline, calling
  //  \code{Update()} on a downstream filter will also trigger update of this
  //  filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  writer->SetFileName( argv[2] );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  //  The output of this example is shown in
  //  Figure~\ref{fig:ThresholdTransferFunctionBelow}.  The second operating mode of
  //  the filter is now enabled by calling the method
  //  \code{ThresholdAbove()}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->ThresholdAbove( 180 );
  filter->Update();
  // Software Guide : EndCodeSnippet


  writer->SetFileName( argv[3] );
  writer->Update();

  //  Software Guide : BeginLatex
  //
  //  Updating the filter with this new setting produces the output shown in
  //  Figure~\ref{fig:ThresholdTransferFunctionAbove}.  The third operating
  //  mode of the filter is enabled by calling \code{ThresholdOutside()}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->ThresholdOutside( 170,190 );
  filter->Update();
  // Software Guide : EndCodeSnippet


  writer->SetFileName( argv[4] );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  //  The output of this third, ``band-pass'' thresholding mode is shown in
  //  Figure~\ref{fig:ThresholdTransferFunctionOutside}.
  //
  //  The examples in this
  //  section also illustrate the limitations of the thresholding filter for performing
  //  segmentation by itself. These limitations are particularly noticeable
  //  in noisy images and in images lacking spatial uniformity, as is the case
  //  with MRI due to field bias.
  //
  //  \relatedClasses
  //  \begin{itemize}
  //  \item \doxygen{BinaryThresholdImageFilter}
  //  \end{itemize}
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
