/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MathematicalMorphologyBinaryFilters.cxx
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
//  The following section illustrates the use of filters for performing basic
//  Mathematical Morphology operaions on binary images. The
//  \doxygen{BinaryErodeImageFilter} and \doxygen{BinaryDilateImageFilter} are
//  described here. The filter names clearly specify the type of image on which
//  it operates.  The header files required for a minimal example on the use of
//  mathematical morphology filters are presented below.
//
//  \index{itk::BinaryDilateImageFilter!header|textbf}
//  \index{itk::BinaryErodeImageFilter!header|textbf}
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
    return 1;
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
  //  Mathematical morphology operations are based on the application of an
  //  operator over a neighborhood of each input pixel. The combination of the
  //  rule and the neighborhood is known as \emph{Structuring Element}. Altough
  //  some rules have become the \emph{de facto} standard on image processing
  //  there is a good deal of freedom as to what kind of algorithmic rule
  //  should be applied on the neighborhood. The implementation on Insight
  //  follows the typical rule of minimum for erosion and maximum for dilation. 
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
  //  The structuring element type is then used along the input and output
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
  //  The structuring element is not a reference counted class. It is then
  //  created as a static C++ object instead of using \code{New()} and
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
  //  A binary image is provided as input to the filters. This image can be,
  //  for example, the the output of a binary threshold image filter.
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
  //  It is necessary to define what could be considered objects on the binary
  //  images. This is specified with the methods \code{SetErodeValue()} and
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
  //  The execution of the filter can be triggered by the invokation of the
  //  \code{Update()} method on the filter or on other downstream filter, for
  //  example a writer.
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
  // \includegraphics[width=4cm]{BinaryThresholdImageFilterOutput.eps}
  // \includegraphics[width=4cm]{MathematicalMorphologyBinaryErosionOutput.eps}
  // \includegraphics[width=4cm]{MathematicalMorphologyBinaryDilationOutput.eps}
  // \caption{Effect of Erosion and Dilation in a binary image.}
  // \label{fig:MathematicalMorphologyBinaryFilters}
  // \end{figure}
  //
  //  Figure \ref{fig:MathematicalMorphologyBinaryFilters} illustrates the
  //  effect of the erosion and dilation filters on a bianry image from a MRI
  //  brain slice. The figure shows how these operations can be used to remove
  //  spurious details from segmented images.
  //
  //  Software Guide : EndLatex 


  return 0;

}

