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
// This example illustrates the use of the SetPixel and GetPixel methods.
// These two methods are intended to provide direct access to pixels
// data contained in the image. Be warned that these two methods have
// a very low performance and should not be used to access massive 
// amounts of data on the image. Image iterators are the appropriate
// mechanism for accessing efficiently the image pixel data.
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
  
  // Pixel data is allocated
  image->SetRegions( region );
  image->Allocate();

  // The image buffer is initialized to a particular value
  ImageType::PixelType  initialValue = 0;
  image->FillBuffer( initialValue );


  // Software Guide : BeginLatex
  //
  // The individual position of a pixel inside the image
  // is identified by a unique index. An index is an array
  // of integers that define the position of the pixel along
  // each dimension of the image. The IndexType is automatically
  // defined in the ImageType and can be accessed using the 
  // scope operator like \texttt{::IndexType}. The length of
  // the array will match the dimension of the associated image.
  //
  // The following code illustrates the declaration of an index
  // variable and the assignment to each one of its components.
  // Please note that indices do not use SmartPointers for its
  // representation. The reason being that indices are pretty
  // small objects not intended to be shared. It results more
  // efficient to produce multiple copies of these small objects
  // than trying to share them using the SmartPointer mechanisms. 
  // 
  // The following lines declare an instance of the index type
  // and initialize its content in order to associate it with
  // a pixel position in the image.
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
  // Having defined a pixel position with an index it is then
  // possible to access the content of the pixel in the image.
  // The SetPixel() method allows to set the value of the pixels.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::PixelType   pixelValue = 149;

  image->SetPixel(   pixelIndex,   pixelValue  );
  // Software Guide : EndCodeSnippet






  // Software Guide : BeginLatex
  //
  // While the \texttt{GetPixel()} method allows to read the
  // value of the pixel.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::PixelType value = image->GetPixel( pixelIndex );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Please note that \texttt{GetPixel() } returns the pixel value
  // by copy not by reference. Hence, the method can not be used
  // for indirectly modifying the image content.
  //
  // Lets repeat that both SetPixel and GetPixel are extremly 
  // inefficient and should only be used for debugging purposes
  // or for implementing interactions with a graphical user interface
  // for supporting features like quering the content of a pixels
  // by clicking with the mouse.
  //
  // Software Guide : EndLatex 


  return 0;

}

