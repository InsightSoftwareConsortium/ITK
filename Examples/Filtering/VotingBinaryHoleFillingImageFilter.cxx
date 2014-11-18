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

// Software Guide : BeginCommandLineArgs
//   INPUTS:  {BinaryThresholdImageFilterOutput.png}
//   OUTPUTS: {VotingBinaryHoleFillingImageFilterOutput1.png}
//   ARGUMENTS:    1 1
// Software Guide : EndCommandLineArgs

// Software Guide : BeginCommandLineArgs
//   INPUTS:  {BinaryThresholdImageFilterOutput.png}
//   OUTPUTS: {VotingBinaryHoleFillingImageFilterOutput2.png}
//   ARGUMENTS:    2 2
// Software Guide : EndCommandLineArgs

// Software Guide : BeginCommandLineArgs
//   INPUTS:  {BinaryThresholdImageFilterOutput.png}
//   OUTPUTS: {VotingBinaryHoleFillingImageFilterOutput3.png}
//   ARGUMENTS:    3 3
// Software Guide : EndCommandLineArgs


//  Software Guide : BeginLatex
//
//  The \doxygen{VotingBinaryHoleFillingImageFilter} applies a voting operation
//  in order to fill in cavities. This can be used for smoothing contours and
//  for filling holes in binary images.
//
//  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


//  Software Guide : BeginLatex
//
//  The header file corresponding to this filter should be included first.
//
//  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!header}
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkVotingBinaryHoleFillingImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile radiusX radiusY" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Then the pixel and image types of the input and output must be defined.
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
  //  Using the image types, it is now possible to define the filter type
  //  and create the filter object.
  //
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!instantiation}
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!New()}
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::VotingBinaryHoleFillingImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The size of the neighborhood is defined along every dimension by
  //  passing a \code{SizeType} object with the corresponding values. The
  //  value on each dimension is used as the semi-size of a rectangular
  //  box. For example, in $2D$ a size of \(1,2\) will result in a $3 \times
  //  5$ neighborhood.
  //
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!Radius}
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!Neighborhood}
  //
  //  Software Guide : EndLatex

  const unsigned int radiusX = atoi( argv[3] );
  const unsigned int radiusY = atoi( argv[4] );

  // Software Guide : BeginCodeSnippet
  InputImageType::SizeType indexRadius;

  indexRadius[0] = radiusX; // radius along x
  indexRadius[1] = radiusY; // radius along y

  filter->SetRadius( indexRadius );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Since the filter is expecting a binary image as input, we must specify
  //  the levels that are going to be considered background and foreground. This
  //  is done with the \code{SetForegroundValue()} and
  //  \code{SetBackgroundValue()} methods.
  //
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!SetForegroundValue()}
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!SetBackgroundValue()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetBackgroundValue(   0 );
  filter->SetForegroundValue( 255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We must also specify the majority threshold that is going to be used as
  //  the decision criterion for converting a background pixel into a
  //  foreground pixel. The rule of conversion is that a background pixel will
  //  be converted into a foreground pixel if the number of foreground
  //  neighbors surpass the number of background neighbors by the majority
  //  value. For example, in a 2D image, with neighborhood of radius 1, the
  //  neighborhood will have size $3 \times 3$. If we set the majority value to
  //  2, then we are requiring that the number of foreground neighbors should
  //  be at least (3x3 -1 )/2 + majority. This is done with the
  //  \code{SetMajorityThreshold()} method.
  //
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!SetMajorityThreshold()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetMajorityThreshold( 2 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input to the filter can be taken from any other filter, for example
  //  a reader. The output can be passed down the pipeline to other filters,
  //  for example, a writer. An update call on any downstream filter will
  //  trigger the execution of the median filter.
  //
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!SetInput()}
  //  \index{itk::Voting\-Binary\-Hole\-Filling\-Image\-Filter!GetOutput()}
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
  // \includegraphics[width=0.44\textwidth]{BinaryThresholdImageFilterOutput}
  // \includegraphics[width=0.44\textwidth]{VotingBinaryHoleFillingImageFilterOutput1}
  // \includegraphics[width=0.44\textwidth]{VotingBinaryHoleFillingImageFilterOutput2}
  // \includegraphics[width=0.44\textwidth]{VotingBinaryHoleFillingImageFilterOutput3}
  // \itkcaption[Effect of the VotingBinaryHoleFilling filter.]{Effect of the
  // VotingBinaryHoleFillingImageFilter on a slice from a MRI proton density brain image
  // that has been thresholded in order to produce a binary image. The output
  // images have used radius 1,2 and 3 respectively.}
  // \label{fig:VotingBinaryHoleFillingImageFilterOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:VotingBinaryHoleFillingImageFilterOutput} illustrates the effect of
  //  the VotingBinaryHoleFillingImageFilter filter on a thresholded slice of MRI brain
  //  image using neighborhood radii of \(1,1\), \(2,2\) and \(3,3\) that
  //  correspond respectively to neighborhoods of size $ 3 \times 3 $,  $ 5
  //  \times 5 $, $ 7 \times 7 $.  The filtered image demonstrates the
  //  capability of this filter for reducing noise both in the background and
  //  foreground of the image, as well as smoothing the contours of the regions.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
