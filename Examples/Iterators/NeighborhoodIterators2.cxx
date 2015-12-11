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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"

// Software Guide : BeginLatex
//
// In this example, the Sobel edge-detection routine is rewritten using
// convolution filtering.  Convolution filtering is a standard image processing
// technique that can be implemented numerically as the inner product of all
// image neighborhoods with a convolution kernel \cite{Gonzalez1993}
// \cite{Castleman1996}.  In ITK, we use a class of objects called
// \emph{neighborhood operators} as convolution kernels and a special function
// object called \doxygen{NeighborhoodInnerProduct} to calculate inner
// products.
//
// The basic ITK convolution filtering routine is to step through the image
// with a neighborhood iterator and use NeighborhoodInnerProduct to
// find the inner product of each neighborhood with the desired kernel. The
// resulting values are written to an output image.  This example uses a
// neighborhood operator called the \doxygen{SobelOperator}, but all
// neighborhood operators can be convolved with images using this basic
// routine.  Other examples of neighborhood operators include derivative
// kernels, Gaussian kernels, and morphological
// operators. \doxygen{NeighborhoodOperatorImageFilter} is a generalization of
// the code in this section to ND images and arbitrary convolution kernels.
//
// We start writing this example by including the header files for the Sobel
// kernel and the inner product function.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSobelOperator.h"
#include "itkNeighborhoodInnerProduct.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
      std::cerr << "Missing parameters. " << std::endl;
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0]
                << " inputImageFile outputImageFile direction"
                << std::endl;
      return EXIT_FAILURE;
    }

  typedef float                             PixelType;
  typedef itk::Image< PixelType, 2 >        ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;

  typedef itk::ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType>        IteratorType;

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

  ImageType::Pointer output = ImageType::New();
  output->SetRegions(reader->GetOutput()->GetRequestedRegion());
  output->Allocate();

  IteratorType out(output, reader->GetOutput()->GetRequestedRegion());

  // Software Guide : BeginLatex
  //
  // \index{convolution!kernels}
  // \index{convolution!operators}
  // \index{iterators!neighborhood!and convolution}
  //
  // Refer to the previous example for a description of reading the input image and
  // setting up the output image and iterator.
  //
  // The following code creates a Sobel operator.  The Sobel operator requires
  // a direction for its partial derivatives.  This direction is read from the command line.
  // Changing the direction of the derivatives changes the bias of the edge
  // detection, i.e. maximally vertical or maximally horizontal.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itk::SobelOperator<PixelType, 2> sobelOperator;
  sobelOperator.SetDirection( ::atoi(argv[3]) );
  sobelOperator.CreateDirectional();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The neighborhood iterator is initialized as before, except that now it takes
  // its radius directly from the radius of the Sobel operator.  The inner
  // product function object is templated over image type and requires no
  // initialization.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius = sobelOperator.GetRadius();
  NeighborhoodIteratorType it( radius, reader->GetOutput(),
                               reader->GetOutput()->GetRequestedRegion() );

  itk::NeighborhoodInnerProduct<ImageType> innerProduct;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Using the Sobel operator, inner product, and neighborhood iterator objects,
  // we can now write a very simple \code{for} loop for performing convolution
  // filtering.  As before, out-of-bounds pixel values are supplied automatically
  // by the iterator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
    {
    out.Set( innerProduct( it, sobelOperator ) );
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The output is rescaled and written as in the previous example.  Applying
  // this example in the $x$ and $y$ directions produces the images at the center
  // and right of Figure~\ref{fig:NeighborhoodExamples1}. Note that x-direction
  // operator produces the same output image as in the previous example.
  //
  // Software Guide : EndLatex

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

  return EXIT_SUCCESS;
}
