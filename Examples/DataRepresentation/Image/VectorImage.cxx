/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    VectorImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
// href{http://www.sgi.com/tech/stl/}{STL}.  If you are interested in
// containers, the \doxygen{VectorContainer} class may provide the
// functionalities you are looking for.
//
// \index{itk::Vector}
// \index{itk::Vector!header}
//
//
// The first step is then to include the header file of the vector class.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"

int main()
{
  // Software Guide : BeginLatex
  // 
  // The \doxygen{Vector} class is templated over the type used to represent
  // the coordinate in space and over the space dimension.  In this example
  // we want the vector dimension to match the image dimension but this is by
  // no means a requirement. We could have defined a four dimensional image
  // with three dimensional vectors as pixels.
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
  ImageType::IndexType start;
  ImageType::SizeType  size;

  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z

  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z

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

  ImageType::IndexType pixelIndex;
 
  pixelIndex[0] = 27;   // x position
  pixelIndex[1] = 29;   // y position
  pixelIndex[2] = 37;   // z position


  // Software Guide : BeginLatex
  //
  // The \doxygen{Vector} inherits the operator \code{[]} from the
  // \doxygen{FixedArray} class. It is then possible to access its components
  // using index notation as follows.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::PixelType   pixelValue;

  pixelValue[0] =  1.345;   // x component
  pixelValue[0] =  6.841;   // y component
  pixelValue[0] =  3.295;   // x component
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


  // Lets repeat that both \code{SetPixel()} and \code{GetPixel()} are
  // inefficient and should only be used for debugging purposes or for
  // implementing interactions with a graphical user interface such as
  // querying pixel value by clicking with the mouse.

  return 0;
}

