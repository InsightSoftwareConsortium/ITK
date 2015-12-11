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
// This example illustrates a technique for improving the efficiency of
// neighborhood calculations by eliminating unnecessary bounds checking. As
// described in Section~\ref{sec:NeighborhoodIterators}, the neighborhood
// iterator automatically enables or disables bounds checking based on the
// iteration region in which it is initialized. By splitting our image into
// boundary and non-boundary regions, and then processing each region using a
// different neighborhood iterator, the algorithm will only perform
// bounds-checking on those pixels for which it is actually required.  This
// trick can provide a significant speedup for simple algorithms such as our
// Sobel edge detection, where iteration speed is a critical.
//
// Splitting the image into the necessary regions is an easy task when you use
// the \doxygen{ImageBoundaryFacesCalculator}.  The face
// calculator is so named because it returns a list of the ``faces'' of the ND
// dataset.  Faces are those regions whose pixels all lie within a distance $d$
// from the boundary, where $d$ is the radius of the neighborhood stencil used
// for the numerical calculations. In other words, faces are those regions
// where a neighborhood iterator of radius $d$ will always overlap the boundary
// of the image. The face calculator also returns the single \emph{inner}
// region, in which out-of-bounds values are never required and bounds checking
// is not necessary.
//
// The face calculator object is defined in \code{itkNeighborhoodAlgorithm.h}.
// We include this file in addition to those from the previous two examples.
//
// Software Guide : EndLatex

#include "itkSobelOperator.h"
#include "itkNeighborhoodInnerProduct.h"

// Software Guide : BeginCodeSnippet
#include "itkNeighborhoodAlgorithm.h"
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

  itk::SobelOperator<PixelType, 2> sobelOperator;
  sobelOperator.SetDirection( ::atoi(argv[3]) );
  sobelOperator.CreateDirectional();

  itk::NeighborhoodInnerProduct<ImageType> innerProduct;

  // Software Guide : BeginLatex
  //
  // First we load the input image and create the output image and inner product
  // function as in the previous examples.  The image iterators will be created
  // in a later step.  Next we create a face calculator object.  An empty list is
  // created to hold the regions that will later on be returned by the face
  // calculator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::NeighborhoodAlgorithm
    ::ImageBoundaryFacesCalculator< ImageType > FaceCalculatorType;

  FaceCalculatorType faceCalculator;
  FaceCalculatorType::FaceListType faceList;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The face calculator function is invoked by passing it an image pointer, an
  // image region, and a neighborhood radius.  The image pointer is the same
  // image used to initialize the neighborhood iterator, and the image region is
  // the region that the algorithm is going to process.  The radius is the radius
  // of the iterator.
  //
  // Notice that in this case the image region is given as the region of the
  // \emph{output} image and the image pointer is given as that of the
  // \emph{input} image.  This is important if the input and output images differ
  // in size, i.e. the input image is larger than the output image.  ITK image
  // filters, for example, operate on data from the input image but only generate
  // results in the \code{RequestedRegion} of the output image, which may be
  // smaller than the full extent of the input.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  faceList = faceCalculator(reader->GetOutput(), output->GetRequestedRegion(),
                            sobelOperator.GetRadius());
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The face calculator has returned a list of $2N+1$ regions. The first element
  // in the list is always the inner region, which may or may not be important
  // depending on the application.  For our purposes it does not matter because
  // all regions are processed the same way.  We use an iterator to traverse the
  // list of faces.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FaceCalculatorType::FaceListType::iterator fit;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We now rewrite the main loop of the previous example so that each region in the
  // list is processed by a separate iterator.  The iterators \code{it} and
  // \code{out} are reinitialized over each region in turn.  Bounds checking is
  // automatically enabled for those regions that require it, and disabled for
  // the region that does not.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  IteratorType out;
  NeighborhoodIteratorType it;

  for ( fit=faceList.begin(); fit != faceList.end(); ++fit)
    {
    it = NeighborhoodIteratorType( sobelOperator.GetRadius(),
                                  reader->GetOutput(), *fit );
    out = IteratorType( output, *fit );

    for (it.GoToBegin(), out.GoToBegin(); ! it.IsAtEnd(); ++it, ++out)
      {
      out.Set( innerProduct(it, sobelOperator) );
      }
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The output is written as before.  Results for this example are the same as
  // the previous example.  You may not notice the speedup except on larger
  // images.  When moving to 3D and higher dimensions, the effects are greater
  // because the volume to surface area ratio is usually larger.  In other
  // words, as the number of interior pixels increases relative to the number of
  // face pixels, there is a corresponding increase in efficiency from disabling
  // bounds checking on interior pixels.
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
