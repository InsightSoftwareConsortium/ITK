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
// This example illustrates how to manually construct an ITK \doxygen{Image}
// class.  The following code is the minimal needed to instantiate, declare
// and create the image class.
//
// \index{itk::Image!Instantiation}
// \index{itk::Image!Header}
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
  // Then we must decide with what type to represent the pixels
  // and what the dimension of the image will be. With these two 
  // parameters we can instantiate the image class. Here we create
  // a 3D image with \code{unsigned short} pixel data.
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
  // to a \doxygen{SmartPointer}. 
  //
  // \index{itk::Image!Pointer}
  // \index{itk::Image!New()}
  // 
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  ImageType::Pointer image = ImageType::New();      
  // Software Guide : EndCodeSnippet 
 

  // Software Guide : BeginLatex
  //
  // In ITK images exist in combination with one or more \emph{regions}. A
  // region is a subset of the image and indicates a portion of the image
  // that may be processed by other classes in the system. One of the most
  // common regions is known as the the \emph{LargestPossibleRegion} that
  // defines the image in its entirety. Other important regions found in ITK
  // are the \emph{BufferedRegion}, which is the portion of the image
  // actually maintained in memory, and the \emph{RequestedRegion}, which
  // is the region requested by a filter or other class when operating on
  // the image.
  //
  // In ITK, manually creating an image requires that the image is
  // instantiated as previously, and then regions describing the image are
  // associated with it.
  //
  // A region is defined by two classes: the \doxygen{Index} and
  // \doxygen{Size} classes. The origin of the region in the context of the
  // image with which it is associated is defined by \code{Index}. The
  // extent, or size, of the region is defined by \code{Size}. \code{Index}
  // is represented by a N-Dimensional array where each component is an
  // integer indicating---in topological image coordinates---the initial
  // pixel of the image. When an image is created manualy the user is
  // responsible for defining the image size and the index in which the image
  // grid starts. These two parameters make it possible to process selected
  // regions.
  //   
  // The starting point of the image is defined by an \doxygen{Index} class
  // that is an N-Dimensional array where each component is an integer
  // indicating the grid coordinates of the initial pixel of the image.
  //
  // \index{itk::Image!Size}
  // \index{itk::Image!SizeType}
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
  //
  // The region size is represented by an array of the same dimension of the
  // image (using the \doxygen{Size} class). The components of the array are
  // unsigned integers indicating the extent in pixels of the image along
  // every dimension.
  //
  // \index{itk::Image!Index}
  // \index{itk::Image!IndexType}
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
  //
  // Having defined the starting index and the image size, these two
  // parameters are used to create an ImageRegion object which basically
  // encapsulates both concepts together. The region is initalized with the
  // starting index and size of the image.
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
  //
  // Finally the region is passed to the image object in order to define its
  // extent and origin. The \code{SetRegions} method sets the
  // LargestPossibleRegion, BufferedRegion, and RequestedRegion
  // simultaneously. Note that none of the operations performed to this point
  // have allocated memory for the image pixel data. It is necessary to
  // invoke the \code{Allocate()} method to do this. Allocate does not
  // require any arguments since all the information needed for memory
  // allocation has already been provided by the region.
  //
  // \index{itk::Image!Allocate()}
  // \index{itk::Image!SetRegions()}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet 
  image->SetRegions( region );
  image->Allocate();
  // Software Guide : EndCodeSnippet 

  return 0;
}

