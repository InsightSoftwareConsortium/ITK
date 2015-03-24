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

// Software Guide : BeginLatex
//
//This example illustrates how the \doxygen{ImageAdaptor} can be used to cast
// an image from one pixel type to another. In particular, we will
// \emph{adapt} an \code{unsigned char} image to make it appear as an image of
// pixel type \code{float}.
//
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
//
// We begin by including the relevant headers.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageAdaptor.h"
// Software Guide : EndCodeSnippet


#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageFileReader.h"


//  Software Guide : BeginLatex
//
// First, we need to define a \emph{pixel accessor} class that does the actual
// conversion. Note that in general, the only valid operations for pixel
// accessors are those that only require the value of the input pixel. As
// such, neighborhood type operations are not possible. A pixel accessor must
// provide methods \code{Set()} and \code{Get()}, and define the types of
// \code{InternalPixelType} and \code{ExternalPixelType}. The
// \code{InternalPixelType} corresponds to the pixel type of the image to be
// adapted (\code{unsigned char} in this example). The \code{ExternalPixelType}
// corresponds to the pixel type we wish to emulate with the ImageAdaptor
// (\code{float} in this case).
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
class CastPixelAccessor
{
public:
  typedef unsigned char InternalType;
  typedef float         ExternalType;

  static void Set(InternalType & output, const ExternalType & input)
    {
    output = static_cast<InternalType>( input );
    }

  static ExternalType Get( const InternalType & input )
    {
    return static_cast<ExternalType>( input );
    }
};
// Software Guide : EndCodeSnippet


//-------------------------
//
//   Main code
//
//-------------------------

int main( int argc, char *argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << "ImageAdaptor1   inputFileName" << std::endl;
    return EXIT_FAILURE;
    }


//  Software Guide : BeginLatex
//
//  The CastPixelAccessor class simply applies a
//  \code{static\_cast} to the pixel values. We now use this pixel accessor
//  to define the image adaptor type and create an instance using
//  the standard \code{New()} method.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef unsigned char  InputPixelType;
  const   unsigned int   Dimension = 2;
  typedef itk::Image< InputPixelType, Dimension >   ImageType;

  typedef itk::ImageAdaptor< ImageType, CastPixelAccessor > ImageAdaptorType;
  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We also create an image reader templated over the input image type and
// read the input image from file.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
// Software Guide : EndCodeSnippet


  reader->SetFileName( argv[1] );
  reader->Update();


//  Software Guide : BeginLatex
//
//  The output of the reader is then connected as the input to the image
//  adaptor.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  adaptor->SetImage( reader->GetOutput() );
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  In the following code, we visit the image using an iterator
//  instantiated using the adapted image type and compute the
//  sum of the pixel values.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef itk::ImageRegionIteratorWithIndex< ImageAdaptorType >  IteratorType;
  IteratorType  it( adaptor, adaptor->GetBufferedRegion() );

  double sum = 0.0;
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    float value = it.Get();
    sum += value;
    ++it;
    }
  // Software Guide : EndCodeSnippet


  std::cout << "Sum of pixels is: " << sum << std::endl;


  //  Software Guide : BeginLatex
  //
  // Although in this example, we are just performing a simple summation, the key
  // concept is that access to pixels is performed as if the pixel is of type
  //  \code{float}. Additionally, it should be noted that the adaptor is used
  // as if it was an actual image and not as a filter. ImageAdaptors conform
  // to the same API as the  \doxygen{Image} class.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
