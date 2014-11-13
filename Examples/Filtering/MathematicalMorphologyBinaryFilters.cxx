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
//    OUTPUTS: {MathematicalMorphologyBinaryErosionOutput.png}
//    OUTPUTS: {MathematicalMorphologyBinaryDilationOutput.png}
//    ARGUMENTS:    150 180
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The following section illustrates the use of filters that perform basic
//  mathematical morphology operations on binary images. The
//  \doxygen{BinaryErodeImageFilter} and \doxygen{BinaryDilateImageFilter} are
//  described here. The filter names clearly specify the type of image on which
//  they operate.  The header files required to construct a simple example of
//  the use of the mathematical morphology filters are included below.
//
//  \index{itk::BinaryDilateImageFilter!header}
//  \index{itk::BinaryErodeImageFilter!header}
//
//  Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Software Guide : BeginCodeSnippet
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
// Software Guide : EndCodeSnippet

#include "itkBinaryThresholdImageFilter.h"


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFileErosion  outputImageFileDilation";
    std::cerr << " lowerThreshold upperThreshold " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

  typedef unsigned char   InputPixelType;
  typedef unsigned char   OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  typedef itk::BinaryThresholdImageFilter< InputImageType, InputImageType >  ThresholdFilterType;


  //  Software Guide : BeginLatex
  //
  //  Mathematical morphology operations are implemented by applying an
  //  operator over the neighborhood of each input pixel. The combination of
  //  the rule and the neighborhood is known as \emph{structuring
  //  element}. Although some rules have become de facto standards for image
  //  processing, there is a good deal of freedom as to what kind of
  //  algorithmic rule should be applied to the neighborhood. The
  //  implementation in ITK follows the typical rule of minimum for
  //  erosion and maximum for dilation.
  //
  //  The structuring element is implemented as a NeighborhoodOperator. In
  //  particular, the default structuring element is the
  //  \doxygen{BinaryBallStructuringElement} class. This class is instantiated
  //  using the pixel type and dimension of the input image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryBallStructuringElement<
                      InputPixelType,
                      Dimension  >             StructuringElementType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The structuring element type is then used along with the input and output
  //  image types for instantiating the type of the filters.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::BinaryErodeImageFilter<
                            InputImageType,
                            OutputImageType,
                            StructuringElementType >  ErodeFilterType;

  typedef itk::BinaryDilateImageFilter<
                            InputImageType,
                            OutputImageType,
                            StructuringElementType >  DilateFilterType;
  // Software Guide : EndCodeSnippet


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writerDilation = WriterType::New();
  WriterType::Pointer writerErosion  = WriterType::New();

  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  //  Software Guide : BeginLatex
  //
  //  The filters can now be created by invoking the \code{New()} method and
  //  assigning the result to \doxygen{SmartPointer}s.
  //
  //  \index{itk::BinaryDilateImageFilter!New()}
  //  \index{itk::BinaryErodeImageFilter!New()}
  //  \index{itk::BinaryDilateImageFilter!Pointer}
  //  \index{itk::BinaryErodeImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ErodeFilterType::Pointer  binaryErode  = ErodeFilterType::New();
  DilateFilterType::Pointer binaryDilate = DilateFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The structuring element is not a reference counted class. Thus it is
  //  created as a C++ stack object instead of using \code{New()} and
  //  SmartPointers. The radius of the neighborhood associated with the
  //  structuring element is defined with the \code{SetRadius()} method and the
  //  \code{CreateStructuringElement()} method is invoked in order to
  //  initialize the operator.  The resulting structuring element is passed to
  //  the mathematical morphology filter through the \code{SetKernel()} method,
  //  as illustrated below.
  //
  //  \index{itk::BinaryBallStructuringElement!SetRadius()}
  //  \index{itk::BinaryBallStructuringElement!CreateStructuringElement()}
  //  \index{itk::BinaryDilateImageFilter!SetKernel()}
  //  \index{itk::BinaryErodeImageFilter!SetKernel()}
  //  \index{SetRadius()!itk::BinaryBallStructuringElement}
  //  \index{SetKernel()!itk::BinaryDilateImageFilter}
  //  \index{SetKernel()!itk::BinaryErodeImageFilter}
  //  \index{SetRadius()!itk::BinaryBallStructuringElement}
  //  \index{CreateStructuringElement()!itk::BinaryBallStructuringElement}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  StructuringElementType  structuringElement;

  structuringElement.SetRadius( 1 );  // 3x3 structuring element

  structuringElement.CreateStructuringElement();

  binaryErode->SetKernel(  structuringElement );
  binaryDilate->SetKernel( structuringElement );
  // Software Guide : EndCodeSnippet


  reader->SetFileName( argv[1] );

  writerErosion->SetFileName(  argv[2] );
  writerDilation->SetFileName( argv[3] );


  //  Software Guide : BeginLatex
  //
  //  A binary image is provided as input to the filters. This image might be,
  //  for example, the output of a binary threshold image filter.
  //
  //  Software Guide : EndLatex

  const InputPixelType lowerThreshold = atoi( argv[4] );
  const InputPixelType upperThreshold = atoi( argv[5] );

  // Software Guide : BeginCodeSnippet
  thresholder->SetInput( reader->GetOutput() );

  InputPixelType background =   0;
  InputPixelType foreground = 255;

  thresholder->SetOutsideValue( background );
  thresholder->SetInsideValue(  foreground );

  thresholder->SetLowerThreshold( lowerThreshold );
  thresholder->SetUpperThreshold( upperThreshold );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  binaryErode->SetInput( thresholder->GetOutput() );
  binaryDilate->SetInput( thresholder->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The values that correspond to ``objects'' in the binary image are
  //  specified with the methods \code{SetErodeValue()} and
  //  \code{SetDilateValue()}. The value passed to these methods will be
  //  considered the value over which the dilation and erosion rules will
  //  apply.
  //
  //  \index{itk::BinaryDilateImageFilter!SetDilateValue()}
  //  \index{itk::BinaryErodeImageFilter!SetErodeValue()}
  //  \index{SetDilateValue()!itk::BinaryDilateImageFilter}
  //  \index{SetErodeValue()!itk::BinaryErodeImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  binaryErode->SetErodeValue( foreground );
  binaryDilate->SetDilateValue( foreground );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter is executed by invoking its \code{Update()} method, or by
  //  updating any downstream filter, such as an image writer.
  //
  //  \index{itk::BinaryDilateImageFilter!Update()}
  //  \index{itk::BinaryErodeImageFilter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  writerDilation->SetInput( binaryDilate->GetOutput() );
  writerDilation->Update();
  // Software Guide : EndCodeSnippet

  writerErosion->SetInput( binaryErode->GetOutput() );
  writerErosion->Update();

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{BinaryThresholdImageFilterOutput}
  // \includegraphics[width=0.32\textwidth]{MathematicalMorphologyBinaryErosionOutput}
  // \includegraphics[width=0.32\textwidth]{MathematicalMorphologyBinaryDilationOutput}
  // \itkcaption[Effect of erosion and dilation in a binary image.]{Effect of
  // erosion and dilation in a binary image.}
  // \label{fig:MathematicalMorphologyBinaryFilters}
  // \end{figure}
  //
  //  Figure \ref{fig:MathematicalMorphologyBinaryFilters} illustrates the
  //  effect of the erosion and dilation filters on a binary image from a MRI
  //  brain slice. The figure shows how these operations can be used to remove
  //  spurious details from segmented images.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
