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

#include "itkImage.h"
#include "itkImageFileReader.h"

//  Software Guide : BeginLatex
//
//  Thanks to the flexibility offered by the
//  \href{http://www.boost.org/more/generic_programming.html}{Generic
//  Programming} style on which ITK is based, it is possible to
//  instantiate images of arbitrary pixel type.  The following example
//  illustrates how a color image with RGB pixels can be defined.
//
//  A class intended to support the RGB pixel type is available in ITK.  You
//  could also define your own pixel class and use it to instantiate a
//  custom image type. In order to use the \doxygen{RGBPixel} class, it is
//  necessary to include its header file.
//
//  \index{itk::RGBPixel}
//  \index{itk::RGBPixel!Image}
//  \index{itk::RGBPixel!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
// Software Guide : EndCodeSnippet

int main( int , char * argv[] )
{
  // Software Guide : BeginLatex
  //
  // The RGB pixel class is templated over a type used to represent each one
  // of the red, green and blue pixel components. A typical instantiation of the
  // templated class is as follows.
  //
  //  \index{itk::RGBPixel!Instantiation}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RGBPixel< unsigned char >    PixelType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The type is then used as the pixel template parameter of the image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType, 3 >   ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The image type can be used to instantiate other filter, for example,
  // an \doxygen{ImageFileReader} object that will read the image from a
  // file.
  //
  // \index{itk::ImageFileReader!RGB Image}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();
  const char * const filename = argv[1];
  reader->SetFileName( filename );
  reader->Update();

  ImageType::Pointer image = reader->GetOutput();
  const ImageType::IndexType pixelIndex = {{25,35,0}};

  // Software Guide : BeginLatex
  //
  // Access to the color components of the pixels can now be performed using
  // the methods provided by the RGBPixel class.
  //
  // \index{itk::Image!GetPixel()}
  // \index{itk::RGBPixel!GetRed()}
  // \index{itk::RGBPixel!GetGreen()}
  // \index{itk::RGBPixel!GetBlue()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PixelType onePixel = image->GetPixel( pixelIndex );

  PixelType::ValueType red   = onePixel.GetRed();
  PixelType::ValueType green = onePixel.GetGreen();
  PixelType::ValueType blue  = onePixel.GetBlue();
  // Software Guide : EndCodeSnippet

  std::cout << "Pixel values from GetRed,GetGreen,GetBlue:" << std::endl;
  std::cout << "Red = "
            << itk::NumericTraits<PixelType::ValueType>::PrintType(red)
            << std::endl;
  std::cout << "Green = "
            << itk::NumericTraits<PixelType::ValueType>::PrintType(green)
            << std::endl;
  std::cout << "Blue = "
            << itk::NumericTraits<PixelType::ValueType>::PrintType(blue)
            << std::endl;

  // Software Guide : BeginLatex
  //
  // The subindex notation can also be used since the \doxygen{RGBPixel}
  // inherits the \code{[]} operator from the \doxygen{FixedArray} class.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  red   = onePixel[0];  // extract Red   component
  green = onePixel[1];  // extract Green component
  blue  = onePixel[2];  // extract Blue  component

  std::cout << "Pixel values:" << std::endl;
  std::cout << "Red = "
            << itk::NumericTraits<PixelType::ValueType>::PrintType(red)
            << std::endl;
  std::cout << "Green = "
            << itk::NumericTraits<PixelType::ValueType>::PrintType(green)
            << std::endl;
  std::cout << "Blue = "
            << itk::NumericTraits<PixelType::ValueType>::PrintType(blue)
            << std::endl;

  // Software Guide : EndCodeSnippet

  // Lets repeat that both \code{SetPixel()} and \code{GetPixel()} are
  // inefficient and should only be used for debugging purposes or for
  // implementing interactions with a graphical user interface such as
  // querying pixel value by clicking with the mouse.
  //

  return EXIT_SUCCESS;
}
