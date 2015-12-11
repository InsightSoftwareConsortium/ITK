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
#include "itkNeighborhoodAlgorithm.h"
#include "itkGaussianOperator.h"
#include "itkNeighborhoodInnerProduct.h"

// Software Guide : BeginLatex
//
// This example introduces slice-based neighborhood processing.  A slice, in
// this context, is a 1D path through an ND neighborhood. Slices are defined
// for generic arrays by the \code{std::slice} class as a start index, a step
// size, and an end index.  Slices simplify the implementation of certain
// neighborhood calculations.  They also provide a mechanism for taking inner
// products with subregions of neighborhoods.
//
// Suppose, for example, that we want to take partial derivatives in the $y$
// direction of a neighborhood, but offset those derivatives by one pixel
// position along the positive $x$ direction.  For a $3\times3$, 2D
// neighborhood iterator, we can construct an \code{std::slice}, \code{(start =
// 2, stride = 3, end = 8)}, that represents the neighborhood offsets $(1,
// -1)$, $(1, 0)$, $(1, 1)$ (see Figure~\ref{fig:NeighborhoodIteratorFig2}). If we
// pass this slice as an extra argument to the
// \doxygen{NeighborhoodInnerProduct} function, then the inner product is taken
// only along that slice.  This ``sliced'' inner product with a 1D
// \doxygen{DerivativeOperator} gives the desired derivative.
//
// The previous separable Gaussian filtering example can be rewritten using
// slices and slice-based inner products.  In general, slice-based processing
// is most useful when doing many different calculations on the same
// neighborhood, where defining multiple iterators as in
// Section~\ref{sec:NeighborhoodExample4} becomes impractical or inefficient.
// Good examples of slice-based neighborhood processing can be found in any of
// the ND anisotropic diffusion function objects, such as
// \doxygen{CurvatureNDAnisotropicDiffusionFunction}.
//
// Software Guide : EndLatex

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " inputImageFile outputImageFile sigma"
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

  itk::NeighborhoodInnerProduct<ImageType> innerProduct;

  typedef itk::NeighborhoodAlgorithm
    ::ImageBoundaryFacesCalculator< ImageType > FaceCalculatorType;

  FaceCalculatorType faceCalculator;
  FaceCalculatorType::FaceListType faceList;
  FaceCalculatorType::FaceListType::iterator fit;

  IteratorType out;
  NeighborhoodIteratorType it;

// Software Guide: BeginLatex
//
// The first difference between this example and the previous example is that
// the Gaussian operator is only initialized once.  Its direction is not
// important because it is only a 1D array of coefficients.
//
// Software Guide: EndLatex

// Software Guide : BeginCodeSnippet
  itk::GaussianOperator< PixelType, 2 > gaussianOperator;
  gaussianOperator.SetDirection(0);
  gaussianOperator.SetVariance( ::atof(argv[3]) * ::atof(argv[3]) );
  gaussianOperator.CreateDirectional();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we need to define a radius for the iterator.  The radius in all
// directions matches that of the single extent of the Gaussian operator,
// defining a square neighborhood.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( gaussianOperator.GetRadius()[0] );
// Software Guide EndCodeSnippet

// Software Guide : BeginLatex
//
// The inner product and face calculator are defined for the main processing
// loop as before, but now the iterator is reinitialized each iteration with
// the square \code{radius} instead of the radius of the operator.  The
// inner product is taken using a slice along the axial direction corresponding
// to the current iteration.  Note the use of \code{GetSlice()} to return the
// proper slice from the iterator itself.  \code{GetSlice()} can only be used
// to return the slice along the complete extent of the axial direction of a
// neighborhood.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer input = reader->GetOutput();
  faceList = faceCalculator(input, output->GetRequestedRegion(), radius);

  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
    {
    for ( fit=faceList.begin(); fit != faceList.end(); ++fit )
      {
      it = NeighborhoodIteratorType( radius, input, *fit );
      out = IteratorType( output, *fit );
      for (it.GoToBegin(), out.GoToBegin(); ! it.IsAtEnd(); ++it, ++out)
        {
        out.Set( innerProduct(it.GetSlice(i), it, gaussianOperator) );
        }
      }

    // Swap the input and output buffers
    if (i != ImageType::ImageDimension - 1)
      {
      ImageType::Pointer tmp = input;
      input = output;
      output = tmp;
      }
    }
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// This technique produces exactly the same results as the previous example.  A
// little experimentation, however, will reveal that it is less efficient since
// the neighborhood iterator is keeping track of extra, unused pixel locations
// for each iteration, while the previous example only references those pixels
// that it needs.  In cases, however, where an algorithm takes multiple
// derivatives or convolution products over the same neighborhood, slice-based
// processing can increase efficiency and simplify the implementation.
//
// Software Guide : EndLatex

  typedef unsigned char                          WritePixelType;
  typedef itk::Image< WritePixelType, 2 >        WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;

  typedef itk::RescaleIntensityImageFilter< ImageType,
    WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput(output);

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
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
