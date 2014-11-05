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
//    OUTPUTS: {GradientMagnitudeImageFilterOutput.png}
//  Software Guide : EndCommandLineArgs
//
//  Software Guide : BeginLatex
//
//  The magnitude of the image gradient is extensively used in image analysis,
//  mainly to help in the determination of object contours and the
//  separation of homogeneous regions. The
//  \doxygen{GradientMagnitudeImageFilter} computes the magnitude of the
//  image gradient at each pixel location using a simple finite differences
//  approach. For example, in the case of $2D$ the computation is equivalent
//  to convolving the image with masks of type
//
//  \begin{center}
//  \begin{picture}(200,50)
//  \put( 5.0,32.0){\framebox(30.0,15.0){-1}}
//  \put(35.0,32.0){\framebox(30.0,15.0){0}}
//  \put(65.0,32.0){\framebox(30.0,15.0){1}}
//  \put(105.0,17.0){\framebox(20.0,15.0){1}}
//  \put(105.0,32.0){\framebox(20.0,15.0){0}}
//  \put(105.0,47.0){\framebox(20.0,15.0){-1}}
//  \end{picture}
//  \end{center}
//
//  then adding the sum of their squares and computing the square root of the sum.
//
//  This filter will work on images of any dimension thanks to the internal
//  use of \doxygen{NeighborhoodIterator} and \doxygen{NeighborhoodOperator}.
//
//  \index{itk::GradientMagnitudeImageFilter}
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
//  \index{itk::GradientMagnitudeImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientMagnitudeImageFilter.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be chosen for the pixels of the input and output images.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input and output image types can be defined using the pixel types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The type of the gradient magnitude filter is defined by the
  //  input image and the output image types.
  //
  //  \index{itk::GradientMagnitudeImageFilter!instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientMagnitudeImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::GradientMagnitudeImageFilter!New()}
  //  \index{itk::GradientMagnitudeImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  the source is an image reader.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally, the filter is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::GradientMagnitudeImageFilter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters in a
  //  pipeline, updating any of the downstream filters will also trigger an
  //  update of this filter. For example, the gradient magnitude filter may be
  //  connected to an image writer.
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
  // \includegraphics[width=0.44\textwidth]{GradientMagnitudeImageFilterOutput}
  // \itkcaption[GradientMagnitudeImageFilter output]{Effect of the
  // GradientMagnitudeImageFilter on a slice from a MRI proton density image
  // of the brain.}
  // \label{fig:GradientMagnitudeImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:GradientMagnitudeImageFilterInputOutput} illustrates the
  //  effect of the gradient magnitude filter on a MRI proton density image of
  //  the brain. The figure shows the sensitivity of this filter to noisy data.
  //
  //  Attention should be paid to the image type chosen to represent the output
  //  image since the dynamic range of the gradient magnitude image is usually
  //  smaller than the dynamic range of the input image. As always, there are
  //  exceptions to this rule, for example, synthetic images that contain high
  //  contrast objects.
  //
  //  This filter does not apply any smoothing to the image before computing the
  //  gradients. The results can therefore be very sensitive to noise and may
  //  not be the best choice for scale-space analysis.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
