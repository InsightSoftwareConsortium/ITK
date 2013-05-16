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
// Many image processing tasks require images of non-scalar pixel type. A
// typical example is an image of vectors. This is the image type required to
// represent the gradient of a scalar image. The following code illustrates
// how to instantiate and use an image whose pixels are of vector type.
//
// For convenience we use the \doxygen{Vector} class to define the pixel
// type.  The Vector class is intended to represent a geometrical vector in
// space. It is not intended to be used as an array container like the
// \href{http://www.sgi.com/tech/stl/Vector.html}{\code{std::vector}} in
// \href{http://www.sgi.com/tech/stl/}{STL}.  If you are interested in
// containers, the \doxygen{VectorContainer} class may provide the
// functionality you want.
//
// \index{itk::Vector}
// \index{itk::Vector!header}
//
//
// The first step is to include the header file of the Vector class.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"

int main(int, char *[])
{
  // Software Guide : BeginLatex
  //
  // The Vector class is templated over the type used to represent
  // the coordinate in space and over the dimension of the space.  In this example,
  // we want the vector dimension to match the image dimension, but this is by
  // no means a requirement. We could have defined a four-dimensional image
  // with three-dimensional vectors as pixels.
  //
  // \index{itk::Vector!Instantiation}
  // \index{itk::Vector!itk::Image}
  // \index{itk::Image!Vector pixel}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 3 >       PixelType;
  typedef itk::Image< PixelType, 3 >    ImageType;
  // Software Guide : EndCodeSnippet

  // Then the image object can be created
  ImageType::Pointer image = ImageType::New();

  // The image region should be initialized
  const ImageType::IndexType start = {{0,0,0}}; //First index at {X,Y,Z}
  const ImageType::SizeType  size = {{200,200,200}}; //Size of {X,Y,Z}

  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  // Pixel data is allocated
  image->SetRegions( region );
  image->Allocate();

  // The image buffer is initialized to a particular value
  ImageType::PixelType  initialValue;

  // A vector can initialize all its components to the
  // same value by using the Fill() method.
  initialValue.Fill( 0.0 );

  // Now the image buffer can be initialized with this
  // vector value.
  image->FillBuffer( initialValue );

  const ImageType::IndexType pixelIndex = {{27,29,37}}; //Position {X,Y,Z}

  // Software Guide : BeginLatex
  //
  // The Vector class inherits the operator \code{[]} from the
  // \doxygen{FixedArray} class. This makes it possible to access the
  // Vector's components using index notation.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::PixelType   pixelValue;
  pixelValue[0] =  1.345;   // x component
  pixelValue[1] =  6.841;   // y component
  pixelValue[2] =  3.295;   // x component
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We can now store this vector in one of the image pixels by defining an
  // index and invoking the \code{SetPixel()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  image->SetPixel(   pixelIndex,   pixelValue  );
  // Software Guide : EndCodeSnippet

  // The GetPixel method can also be used to read Vectors
  // pixels from the image
  ImageType::PixelType value = image->GetPixel( pixelIndex );

  std::cout << value << std::endl;

  // Lets repeat that both \code{SetPixel()} and \code{GetPixel()} are
  // inefficient and should only be used for debugging purposes or for
  // implementing interactions with a graphical user interface such as
  // querying pixel value by clicking with the mouse.

  return EXIT_SUCCESS;
}
