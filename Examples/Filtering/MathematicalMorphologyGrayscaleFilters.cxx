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
//    OUTPUTS: {MathematicalMorphologyGrayscaleErosionOutput.png}
//    OUTPUTS: {MathematicalMorphologyGrayscaleDilationOutput.png}
//    ARGUMENTS:    150 180
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The following section illustrates the use of filters for performing basic
//  mathematical morphology operations on grayscale images. The
//  \doxygen{GrayscaleErodeImageFilter} and
//  \doxygen{GrayscaleDilateImageFilter} are covered in this example. The
//  filter names clearly specify the type of image on which they operate.
//  The header files required for a simple example of the use of
//  grayscale mathematical morphology filters are presented below.
//
//  \index{itk::GrayscaleDilateImageFilter!header}
//  \index{itk::GrayscaleErodeImageFilter!header}
//
//  Software Guide : EndLatex

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Software Guide : BeginCodeSnippet
#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFileErosion  outputImageFileDilation" << std::endl;
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


  //  Software Guide : BeginLatex
  //
  //  Mathematical morphology operations are based on the application of an
  //  operator over a neighborhood of each input pixel. The combination of
  //  the rule and the neighborhood is known as \emph{structuring
  //  element}. Although some rules have become the de facto standard in image
  //  processing there is a good deal of freedom as to what kind of
  //  algorithmic rule should be applied on the neighborhood. The
  //  implementation in ITK follows the typical rule of minimum for
  //  erosion and maximum for dilation.
  //
  //  The structuring element is implemented as a
  //  \doxygen{NeighborhoodOperator}. In particular, the default structuring
  //  element is the \doxygen{BinaryBallStructuringElement} class. This class
  //  is instantiated using the pixel type and dimension of the input image.
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
  typedef itk::GrayscaleErodeImageFilter<
                            InputImageType,
                            OutputImageType,
                            StructuringElementType >  ErodeFilterType;

  typedef itk::GrayscaleDilateImageFilter<
                            InputImageType,
                            OutputImageType,
                            StructuringElementType >  DilateFilterType;
  // Software Guide : EndCodeSnippet


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writerDilation = WriterType::New();
  WriterType::Pointer writerErosion  = WriterType::New();


  //  Software Guide : BeginLatex
  //
  //  The filters can now be created by invoking the \code{New()} method and
  //  assigning the result to SmartPointers.
  //
  //  \index{itk::GrayscaleDilateImageFilter!New()}
  //  \index{itk::GrayscaleErodeImageFilter!New()}
  //  \index{itk::GrayscaleDilateImageFilter!Pointer}
  //  \index{itk::GrayscaleErodeImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ErodeFilterType::Pointer  grayscaleErode  = ErodeFilterType::New();
  DilateFilterType::Pointer grayscaleDilate = DilateFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The structuring element is not a reference counted class. Thus it is
  //  created as a C++ stack object instead of using \code{New()} and
  //  SmartPointers. The radius of the neighborhood associated with the
  //  structuring element is defined with the \code{SetRadius()} method and the
  //  \code{CreateStructuringElement()} method is invoked in order to initialize the
  //  operator.  The resulting structuring element is passed to the
  //  mathematical morphology filter through the \code{SetKernel()} method, as
  //  illustrated below.
  //
  //  \index{itk::BinaryBallStructuringElement!SetRadius()}
  //  \index{itk::BinaryBallStructuringElement!CreateStructuringElement()}
  //  \index{itk::GrayscaleDilateImageFilter!SetKernel()}
  //  \index{itk::GrayscaleErodeImageFilter!SetKernel()}
  //  \index{SetRadius()!itk::BinaryBallStructuringElement}
  //  \index{SetKernel()!itk::GrayscaleDilateImageFilter}
  //  \index{SetKernel()!itk::GrayscaleErodeImageFilter}
  //  \index{SetRadius()!itk::BinaryBallStructuringElement}
  //  \index{CreateStructuringElement()!itk::BinaryBallStructuringElement}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  StructuringElementType  structuringElement;

  structuringElement.SetRadius( 1 );  // 3x3 structuring element

  structuringElement.CreateStructuringElement();

  grayscaleErode->SetKernel(  structuringElement );
  grayscaleDilate->SetKernel( structuringElement );
  // Software Guide : EndCodeSnippet


  reader->SetFileName( argv[1] );

  writerErosion->SetFileName(  argv[2] );
  writerDilation->SetFileName( argv[3] );


  //  Software Guide : BeginLatex
  //
  //  A grayscale image is provided as input to the filters. This image might be,
  //  for example, the output of a reader.
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  grayscaleErode->SetInput(  reader->GetOutput() );
  grayscaleDilate->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The filter is executed by invoking its \code{Update()} method, or by
  //  updating any downstream filter, such as an image writer.
  //
  //  \index{itk::GrayscaleDilateImageFilter!Update()}
  //  \index{itk::GrayscaleErodeImageFilter!Update()}
  //
  //  Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  writerDilation->SetInput( grayscaleDilate->GetOutput() );
  writerDilation->Update();
  // Software Guide : EndCodeSnippet

  writerErosion->SetInput( grayscaleErode->GetOutput() );
  writerErosion->Update();

  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.32\textwidth]{MathematicalMorphologyGrayscaleErosionOutput}
  // \includegraphics[width=0.32\textwidth]{MathematicalMorphologyGrayscaleDilationOutput}
  // \itkcaption[Effect of erosion and dilation in a grayscale image.]{Effect of
  // erosion and dilation in a grayscale image.}
  // \label{fig:MathematicalMorphologyGrayscaleFilters}
  // \end{figure}
  //
  //  Figure \ref{fig:MathematicalMorphologyGrayscaleFilters} illustrates the
  //  effect of the erosion and dilation filters on a binary image from a MRI
  //  brain slice. The figure shows how these operations can be used to remove
  //  spurious details from segmented images.
  //
  //  Software Guide : EndLatex


  return EXIT_SUCCESS;
}
