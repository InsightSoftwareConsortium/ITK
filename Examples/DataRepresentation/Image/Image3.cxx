/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Image3.cxx
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
// This example illustrates the use of the \code{SetPixel()} and
// \code{GetPixel()} methods.  These two methods provide direct access to the
// pixel data contained in the image. Note that these two methods are
// relatively slow and should not be used in situations where
// higher-performing access is required. Image iterators are the appropriate
// mechanism to efficiently access image pixel data.
//
// Software Guide : EndLatex 


#include "itkImage.h"

int main()
{
  // First the image type should be declared
  typedef itk::Image< unsigned short, 3 > ImageType;

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
  ImageType::PixelType  initialValue = 0;
  image->FillBuffer( initialValue );


  // Software Guide : BeginLatex
  //
  // The individual position of a pixel inside the image is identified by a
  // unique index. An index is an array of integers that define the position
  // of the pixel along each coordinate dimension of the image. The IndexType
  // is automatically defined by the image and can be accessed using the
  // scope operator like \doxygen{Index}. The length of the array will match
  // the dimension of the associated image.
  //
  // The following code illustrates the declaration of an index variable and
  // assigning values to each one of its components.  Please note that
  // \code{Index} does not use SmartPointers to access it. The reason being
  // that \code{Index} is a light-weight object not intended to be shared
  // between objects. It is more efficient to produce multiple copies of
  // these small objects than to share them using the SmartPointer
  // mechanism.
  // 
  // The following lines declare an instance of the index type and initialize
  // its content in order to associate it with a pixel position in the image.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::IndexType pixelIndex;
 
  pixelIndex[0] = 27;   // x position
  pixelIndex[1] = 29;   // y position
  pixelIndex[2] = 37;   // z position
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Having defined a pixel position with an index it is then possible to
  // access the content of the pixel in the image.  The \code{SetPixel()}
  // method allows to set the value of the pixels.
  //
  // \index{itk::Image!SetPixel()}
  // 
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::PixelType   pixelValue = 149;

  image->SetPixel(   pixelIndex,   pixelValue  );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // While the \code{GetPixel()} method allows to read the
  // value of the pixel.
  //
  // \index{itk::Image!GetPixel()}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::PixelType value = image->GetPixel( pixelIndex );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Please note that \code{GetPixel()} returns the pixel value using copy
  // and not reference semantics. Hence, the method cannot be used to
  // modify image data values.
  //
  // Reminder: both \code{SetPixel()} and \code{GetPixel()} are inefficient
  // and should only be used or debugging or for supporing interactions like
  // querying pixel values by clicking with the mouse.
  //
  // Software Guide : EndLatex 


  return 0;

}

