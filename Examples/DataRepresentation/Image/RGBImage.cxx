/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    RGBImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkImage.h"
#include "itkImageFileReader.h"


//  Software Guide : BeginLatex
//
//  Thanks to the flexibility provided by the
//  \href{http://www.boost.org/more/generic_programming.html}{Generic
//  Programming} style on which ITK has been based, it is possible to
//  instantiate images of arbitrary pixel type.  The following example
//  illustrates how a color image with RGB pixels can be defined.
//
//  A class intended to support the RGB pixel type is available in ITK.  You
//  could as well define your own pixel class and use it to instantiate a
//  custom image type. In order to use the \doxygen{RGBPixel} class it is
//  necessary to include its header file.
//
//  \index{itk::RGBPixel|textbf}
//  \index{itk::RGBPixel!Image}
//  \index{itk::RGBPixel!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkRGBPixel.h"
// Software Guide : EndCodeSnippet



int main( int argc, char ** argv )
{

  // Software Guide : BeginLatex
  //
  // The RGB pixel class is templated over the type used to represent each
  // one of the Red, Green and Blue components. A typical instantiation of 
  // the templated class could be as follows.
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
  // The image type can be used for instantiating other filters. For example an
  // \doxygen{ImageFileReader} object that will read the image from a file.
  //
  // \index{itk::ImageFileReader!RGB Image}
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet




  ReaderType::Pointer reader = ReaderType::New();

  const char * filename = argv[1];

  reader->SetFileName( filename );


  reader->Update();

  ImageType::Pointer image = reader->GetOutput();



  ImageType::IndexType pixelIndex;

  pixelIndex[0] = 25;  
  pixelIndex[1] = 35;  
  pixelIndex[2] =  5;  



  // Software Guide : BeginLatex
  //
  // Access to the color components on the pixels can now be performed 
  // using the native methods provided by the RGBPixel class. For example.
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




  // Software Guide : BeginLatex
  //
  // Or using the subindex notation since the \doxygen{RGBPixel} inherits the
  // \code{[]} operator from the \doxygen{FixedArray} class.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  red   = onePixel[0];  // extract Red   component
  green = onePixel[1];  // extract Green component
  blue  = onePixel[2];  // extract Blue  component
  // Software Guide : EndCodeSnippet

  //
  // Lets repeat that both \code{SetPixel()} and \code{GetPixel()} are extremely
  // inefficient and should only be used for debugging purposes or for
  // implementing interactions with a graphical user interface for supporting
  // features like quering the content of a pixels by clicking with the mouse.
  //
 
  return 0;


}

