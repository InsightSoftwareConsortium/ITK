/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Image1.cxx
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
// This example illustrates the use of the \code{itk::Image} class.
// The following code is the minimal procedure required to 
// instantiate, declare and create an image class.\\
//
// \index{itk::Image!Instantiation|textbf}
// \index{itk::Image!Header|textbf}
//
// First, the header file of the Image class must be included.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
// Software Guide : EndCodeSnippet



int main()
{

  
  // Software Guide : BeginLatex
  // 
  // Then we must decide what type to use for representing the pixels
  // and what the dimension of the image will be. With these two 
  // parameters we can instantiate the image type.
  //
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  typedef itk::Image< unsigned short, 3 > ImageType;
  // Software Guide : EndCodeSnippet 

  
  // Software Guide : BeginLatex
  //
  // The image can then be created by invoking the \code{New()} operator
  // from the corresponding image type and assigning the result
  // to a \code{SmartPointer}. 
  //
  // \index{itk::Image!Pointer|textbf}
  // \index{itk::Image!New()|textbf}
  // 
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  ImageType::Pointer image = ImageType::New();      
  // Software Guide : EndCodeSnippet 
 

  // Software Guide : BeginLatex
  //
  // When an image is created manualy the user is responsible
  // for defining the image size and the index in which the
  // image grid starts. These two parameters make possible 
  // later to manage partitions of an image in order to process
  // selected regions. 
  //   
  // The starting point of the image is defined by an N-Dimensional 
  // array where each component is an integer indicating the
  // grid coordinates of the initial pixel of the image.
  //
  // \index{itk::Image!Size|textbf}
  // \index{itk::Image!SizeType}
  //
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  ImageType::SizeType  size;

  size[0]  = 200;  // size along X
  size[1]  = 200;  // size along Y
  size[2]  = 200;  // size along Z
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // The image size is defined by an SizeType which is simply
  // an array of the same dimension of the image. The components 
  // of the array are integeres indicating the extent in pixels
  // of the image along every dimension.
  //
  // \index{itk::Image!Index|textbf}
  // \index{itk::Image!IndexType}
  //
  // Software Guide : EndLatex 
  // 
  // Software Guide : BeginCodeSnippet 
  ImageType::IndexType start;

  start[0] =   0;  // first index on X
  start[1] =   0;  // first index on Y
  start[2] =   0;  // first index on Z
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // Having defined the starting index and the image size, these
  // two parameters are used to create an ImageRegion object which
  // basically handles both concepts together. The region is initalized
  // with the starting index and size of the image.
  //
  // \index{itk::Image!itk::ImageRegion}
  // \index{itk::Image!RegionType}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  ImageType::RegionType region;
  
  region.SetSize( size );
  region.SetIndex( start );
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex
  // Finally the region is passed to the image object in order to
  // define its extent and origin. So far, no memory allocation has
  // been made for the pixel data. It is then necessary to invoke
  // the \code{Allocate()} method on the image. Allocate does not require
  // any arguments since all the information needed for memory 
  // allocation has already been provided by the region.
  //
  // \index{itk::Image!Allocate()|textbf}
  // \index{itk::Image!SetRegions()}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  image->SetRegions( region );
  image->Allocate();
  // Software Guide : EndCodeSnippet 

  return 0;

}

