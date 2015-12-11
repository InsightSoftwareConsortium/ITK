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
//    INPUTS:  {BrainT1Slice.png}
//    OUTPUTS: {NeighborhoodIterators1a.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// This example uses the \doxygen{NeighborhoodIterator} to implement a simple
// Sobel edge detection algorithm \cite{Gonzalez1993}.  The algorithm uses the
// neighborhood iterator to iterate through an input image and calculate a
// series of finite difference derivatives.  Since the derivative results
// cannot be written back to the input image without affecting later
// calculations, they are written instead to a second, output image.  Most
// neighborhood processing algorithms follow this read-only model on their
// inputs.
//
// We begin by including the proper header files.  The
// \doxygen{ImageRegionIterator} will be used to write the results of
// computations to the output image.  A const version of the neighborhood
// iterator is used because the input image is read-only.
//
// Software Guide : EndLatex

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

// Software Guide : BeginCodeSnippet
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
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
  // The finite difference calculations
  // in this algorithm require floating point values.  Hence, we define the image
  // pixel type to be \code{float} and the file reader will
  // automatically cast fixed-point data to \code{float}.
  //
  // We declare the iterator types using the image type as
  // the template parameter. The second template parameter of the
  // neighborhood iterator, which specifies
  // the boundary condition, has been omitted because the default condition is
  // appropriate for this algorithm.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                             PixelType;
  typedef itk::Image< PixelType, 2 >        ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;

  typedef itk::ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType>        IteratorType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The following code creates and executes the ITK image reader.
  // The \code{Update}
  // call on the reader object is surrounded by the standard \code{try/catch}
  // blocks to handle any exceptions that may be thrown by the reader.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  //  We can now create a neighborhood iterator to range over the output of the
  //  reader. For Sobel edge-detection in 2D, we need a square iterator that
  //  extends one pixel away from the neighborhood center in every dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType it( radius, reader->GetOutput(),
                               reader->GetOutput()->GetRequestedRegion() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The following code creates an output image and iterator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::Pointer output = ImageType::New();
  output->SetRegions(reader->GetOutput()->GetRequestedRegion());
  output->Allocate();

  IteratorType out(output, reader->GetOutput()->GetRequestedRegion());
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Sobel edge detection uses weighted finite difference calculations to
  // construct an edge magnitude image.  Normally the edge magnitude is the
  // root sum of squares of partial derivatives in all directions, but for
  // simplicity this example only calculates the $x$ component. The result is a
  // derivative image biased toward maximally vertical edges.
  //
  // The finite differences are computed from pixels at six locations in the
  // neighborhood.  In this example, we use the iterator \code{GetPixel()}
  // method to query the values from their offsets in the neighborhood.
  // The example in Section~\ref{sec:NeighborhoodExample2} uses convolution
  // with a Sobel kernel instead.
  //
  // Six positions in the neighborhood are necessary for the finite difference
  // calculations. These positions are recorded in \code{offset1} through
  // \code{offset6}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::OffsetType offset1 = {{-1,-1}};
  NeighborhoodIteratorType::OffsetType offset2 = {{1,-1}};
  NeighborhoodIteratorType::OffsetType offset3 = {{-1,0 }};
  NeighborhoodIteratorType::OffsetType offset4 = {{1,0}};
  NeighborhoodIteratorType::OffsetType offset5 = {{-1,1}};
  NeighborhoodIteratorType::OffsetType offset6 = {{1,1}};
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // It is equivalent to use the six corresponding integer array indices instead.
  // For example, the offsets \code{(-1,-1)} and \code{(1, -1)} are
  // equivalent to the integer indices \code{0} and \code{2}, respectively.
  //
  // The calculations are done in a \code{for} loop that moves the input and
  // output iterators synchronously across their respective images.  The
  // \code{sum} variable is used to sum the results of the finite differences.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
    {
    float sum;
    sum = it.GetPixel(offset2) - it.GetPixel(offset1);
    sum += 2.0 * it.GetPixel(offset4) - 2.0 * it.GetPixel(offset3);
    sum += it.GetPixel(offset6) - it.GetPixel(offset5);
    out.Set(sum);
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The last step is to write the output buffer to an image file.  Writing is
  // done inside a \code{try/catch} block to handle any exceptions.  The output
  // is rescaled to intensity range $[0, 255]$ and cast to unsigned char so that
  // it can be saved and visualized as a PNG image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char                          WritePixelType;
  typedef itk::Image< WritePixelType, 2 >        WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;

  typedef itk::RescaleIntensityImageFilter<
               ImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput(output);

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(rescaler->GetOutput());
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
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The center image of Figure~\ref{fig:NeighborhoodExamples1} shows the
  // output of the Sobel algorithm applied to
  // \code{Examples/Data/BrainT1Slice.png}.
  //
  // \begin{figure} \centering
  // \includegraphics[width=0.3\textwidth]{BrainT1Slice}
  // \includegraphics[width=0.3\textwidth]{NeighborhoodIterators1a}
  // \includegraphics[width=0.3\textwidth]{NeighborhoodIterators1b}
  // \itkcaption[Sobel edge detection results]{Applying the Sobel operator in
  // different orientations to an MRI image (left) produces $x$ (center) and $y$
  // (right) derivative images.}
  // \protect\label{fig:NeighborhoodExamples1}
  // \end{figure}
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;
}
