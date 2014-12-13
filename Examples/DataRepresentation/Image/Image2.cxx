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

//  Software Guide : BeginLatex
//
//  The first thing required to read an image from a file is to include
//  the header file of the \doxygen{ImageFileReader} class.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
// Software Guide : EndCodeSnippet

int main( int , char * argv[])
{
  // Software Guide : BeginLatex
  //
  // Then, the image type should be defined by specifying the
  // type used to represent pixels and the dimensions of the image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char          PixelType;
  const unsigned int             Dimension = 3;

  typedef itk::Image< PixelType, Dimension >   ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Using the image type, it is now possible to instantiate the image reader
  // class. The image type is used as a template parameter to define how the
  // data will be represented once it is loaded into memory. This type does
  // not have to correspond exactly to the type stored in the file. However,
  // a conversion based on C-style type casting is used, so the type chosen
  // to represent the data on disk must be sufficient to characterize it
  // accurately. Readers do not apply any transformation to the pixel data
  // other than casting from the pixel type of the file to the pixel type of
  // the ImageFileReader. The following illustrates a typical
  // instantiation of the ImageFileReader type.
  //
  // \index{itk::ImageFileReader!Instantiation}
  // \index{itk::Image!read}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The reader type can now be used to create one reader object.  A
  // \doxygen{SmartPointer} (defined by the \code{::Pointer} notation) is used
  // to receive the reference to the newly created reader.  The \code{New()}
  // method is invoked to create an instance of the image reader.
  //
  // \index{itk::ImageFileReader!New()}
  // \index{itk::ImageFileReader!Pointer}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The minimal information required by the reader is the filename
  // of the image to be loaded in memory. This is provided through
  // the \code{SetFileName()} method. The file format here is inferred
  // from the filename extension. The user may also explicitly specify
  // the data format using the \doxygen{ImageIOBase} class (a list
  // of possibilities can be found in the inheritance diagram of this
  // class.).
  //
  // \index{itk::ImageFileReader!SetFileName()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const char * filename = argv[1];
  reader->SetFileName( filename );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Reader objects are referred to as pipeline source objects; they
  // respond to pipeline update requests and initiate the data flow in the
  // pipeline. The pipeline update mechanism ensures that the reader only
  // executes when a data request is made to the reader and the reader has
  // not read any data.  In the current example we explicitly invoke the
  // \code{Update()} method because the output of the reader is not connected
  // to other filters. In normal application the reader's output is connected
  // to the input of an image filter and the update invocation on the filter
  // triggers an update of the reader. The following line illustrates how an
  // explicit update is invoked on the reader.
  //
  // \index{itk::ImageFileReader!Update()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Access to the newly read image can be gained by calling the
  // \code{GetOutput()} method on the reader. This method can also be called
  // before the update request is sent to the reader.  The reference to the
  // image will be valid even though the image will be empty until the reader
  // actually executes.
  //
  // \index{itk::ImageFileReader!GetOutput()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::Pointer image = reader->GetOutput();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Any attempt to access image data before the reader executes will yield
  // an image with no pixel data. It is likely that a program crash will
  // result since the image will not have been properly initialized.
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
