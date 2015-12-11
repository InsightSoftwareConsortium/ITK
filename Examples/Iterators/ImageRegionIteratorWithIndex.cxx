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
//     INPUTS:  {VisibleWomanEyeSlice.png}
//     OUTPUTS: {ImageRegionIteratorWithIndexOutput.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// \index{Iterators!speed}
// The ``WithIndex'' family of iterators was designed for algorithms that
// use both the value and the location of image pixels in calculations.  Unlike
// \doxygen{ImageRegionIterator}, which calculates an index only when
// asked for, \doxygen{ImageRegionIteratorWithIndex} maintains its
// index location as a member variable that is updated during the increment or
// decrement process. Iteration speed is penalized, but the index queries are
// more efficient.
//
// \index{itk::ImageRegionIteratorWithIndex!example of using|(}
//
// The following example illustrates the use of
// ImageRegionIteratorWithIndex.  The algorithm mirrors
// a 2D image across its $x$-axis (see \doxygen{FlipImageFilter} for an ND
// version).  The algorithm makes extensive use of the \code{GetIndex()}
// method.
//
// We start by including the proper header file.
//
// Software Guide : EndLatex

#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : BeginCodeSnippet
#include "itkImageRegionIteratorWithIndex.h"
// Software Guide : EndCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char *argv[] )
{
  // Verify the number of parameters on the command line.
  if ( argc < 3 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " inputImageFile outputImageFile"
              << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // For this example, we will use an RGB pixel type so that we can process color
  // images. Like most other ITK image iterator,
  // ImageRegionIteratorWithIndex class expects the image type as its
  // single template parameter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;

  typedef itk::RGBPixel< unsigned char >        RGBPixelType;
  typedef itk::Image< RGBPixelType, Dimension > ImageType;

  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  // Software Guide : EndCodeSnippet

  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  ImageType::ConstPointer inputImage;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update();
    inputImage = reader->GetOutput();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // An \code{ImageType} smart pointer called \code{inputImage} points to the
  // output of the image reader.  After updating the image reader, we can
  // allocate an output image of the same size, spacing, and origin as the
  // input image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( inputImage->GetRequestedRegion() );
  outputImage->CopyInformation( inputImage );
  outputImage->Allocate();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Next we create the iterator that walks the output image.  This algorithm
  // requires no iterator for the input image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  IteratorType outputIt( outputImage, outputImage->GetRequestedRegion() );
  // Software Guide : EndCodeSnippet

  // Software Guide: BeginLatex
  //
  // This axis flipping algorithm works by iterating through the output image,
  // querying the iterator for its index, and copying the value from the input
  // at an index mirrored across the $x$-axis.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::IndexType requestedIndex =
                outputImage->GetRequestedRegion().GetIndex();
  ImageType::SizeType requestedSize =
                outputImage->GetRequestedRegion().GetSize();

  for ( outputIt.GoToBegin(); !outputIt.IsAtEnd(); ++outputIt)
    {
    ImageType::IndexType idx = outputIt.GetIndex();
    idx[0] =  requestedIndex[0] + requestedSize[0] - 1 - idx[0];
    outputIt.Set( inputImage->GetPixel(idx) );
    }
  // Software Guide : EndCodeSnippet

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(outputImage);
  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
}

  // Software Guide : BeginLatex
  //
  // Let's run this example on the image \code{VisibleWomanEyeSlice.png} found in
  // the \code{Examples/Data} directory.
  // Figure~\ref{fig:ImageRegionIteratorWithIndexExample} shows how the original
  // image has been mirrored across its $x$-axis in the output.
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{VisibleWomanEyeSlice}
  // \includegraphics[width=0.44\textwidth]{ImageRegionIteratorWithIndexOutput}
  // \itkcaption[Using the ImageRegionIteratorWithIndex]{Results of using
  // ImageRegionIteratorWithIndex to mirror an image across an axis. The original
  // image is shown at left.  The mirrored output is shown at right.}
  // \label{fig:ImageRegionIteratorWithIndexExample}
  // \end{figure}
  //
  // \index{itk::ImageRegionIteratorWithIndex!example of using|)}
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
