/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Image2.cxx
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


//  Software Guide : BeginLatex
//
//  The first thing required for reading images from files is to include
//  the header file of the itk::ImageFileReader class
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
// Software Guide : EndCodeSnippet


int main( int argc, char ** argv )
{

  // Software Guide : BeginLatex
  //
  // Then, the image type should be instantiated by specifying the
  // type to be used for representing pixels and the dimension of 
  // the space represented by the image. 
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned char          PixelType;
  const unsigned int             Dimension = 3;

  typedef itk::Image< PixelType, Dimension >   ImageType;
  // Software Guide : EndCodeSnippet

 


  // Software Guide : BeginLatex
  //
  // Using the image type it is now possible to instantiate
  // the type of the image reader class. The type of the image
  // used as template parameter defines how the file data will
  // be represented once it gets loaded into memory. The type
  // does not have to correspond directly to what it is stored 
  // on the file. However, a straight forward conversion based
  // on C-style type casting should be valid. Readers do not
  // apply any transformation to the pixel data other than casting
  // from the pixel type on the file to the pixel type of the 
  // ImageFile Reader. The following line illustrates the typical
  // instantiation of the ImageFileReader type.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // The reader type can now be used to create one reader object.
  // A smart pointer defined by the \code{::Pointer} notation
  // is used to receive the reference to the newly created reader.
  // The \code{New()} method of the reader type is invoked 
  // to create the reader object.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // The minimum information required by the reader is the filename
  // of the image to be loaded in memory. This is provided through
  // the \code{::SetFileName()} method.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const char * filename = argv[1];

  reader->SetFileName( filename );
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // Reader objects perform as sources on the pipeline. They respond
  // to the update mechanisms. This means that the image file is only 
  // read into memory when an update request is made to the reader. 
  // In the present example we are explicitly invoking the \code{Update()} 
  // method because the output of the reader is not connected to any 
  // other filter. On a normal application the output of the reader will
  // be connected as input of another image filter and the update calls
  // on this filter will trigger the update of the reader. The following
  // line illustrates how an explicit update can be requested on the reader.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  reader->Update();
  // Software Guide : EndCodeSnippet




  // Software Guide : BeginLatex
  //
  // Access to the newly read image can be gained by calling the 
  // \code{GetOutput()} method on the reader. This method can 
  // also be called before the update request is sent to the reader.
  // The reference to the image will be equally valid even though
  // the image will lack any pixel data content.
  //
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ImageType::Pointer image = reader->GetOutput();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Any attempt to access image data before the first update call
  // has been made on the reader will result in unpredictable results. 
  // Most likely the result will be a program crash since the image 
  // object will not be properly initialized yet.
  //
  // Software Guide : EndLatex 

  return 0;


}

