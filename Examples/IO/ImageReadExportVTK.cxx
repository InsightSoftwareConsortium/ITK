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

//  Software Guide : BeginLatex
//
//  In cases where the user knows what file format to use and wants to
//  indicate this explicitly, a specific \doxygen{ImageIO} class can be
//  instantiated and assigned to the image file reader or writer. This
//  circumvents the \doxygen{ImageIOFactory} mechanism which tries to find
//  the appropriate ImageIO class for performing the IO operations. Explicit
//  selection of the ImageIO also allows the user to invoke specialized
//  features of a particular class which may not be available from the
//  general API provided by ImageIO.
//
//  The following example illustrates explicit instantiation of an IO class
//  (in this case a VTK file format), setting its parameters and then
//  connecting it to the \doxygen{ImageFileWriter}.
//
//  The example begins by including the appropriate headers.
//
//  \index{itk::ImageFileReader!header}
//  \index{itk::ImageFileWriter!header}
//  \index{itk::VTKImageIO!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVTKImageIO.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"


int main( int argc, char ** argv )
{
  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Then, as usual, we select the pixel types and the image
  //  dimension. Remember, if the file format represents pixels with a
  //  particular type, C-style casting will be performed to convert the data.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned short      PixelType;
  const   unsigned int        Dimension = 2;
  typedef itk::Image< PixelType, Dimension >    ImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We can now instantiate the reader and writer. These two classes are
  //  parameterized over the image type. We instantiate the
  //  \doxygen{VTKImageIO} class as well. Note that the ImageIO objects are
  //  not templated.
  //
  //  \index{itk::ImageFileReader!Instantiation}
  //  \index{itk::ImageFileWriter!Instantiation}
  //  \index{itk::VTKImageIO!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;
  typedef itk::VTKImageIO                    ImageIOType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, we create one object of each type using the New() method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::ImageFileReader!New()}
  //  \index{itk::ImageFileWriter!New()}
  //  \index{itk::ImageFileReader!SmartPointer}
  //  \index{itk::ImageFileWriter!SmartPointer}
  //  \index{itk::VTKImageIO!New()}
  //  \index{itk::VTKImageIO!SmartPointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  ImageIOType::Pointer vtkIO = ImageIOType::New();
  // Software Guide : EndCodeSnippet


  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];


  //  Software Guide : BeginLatex
  //
  //  The name of the file to be read or written is passed with the
  //  SetFileName() method.
  //
  //  \index{itk::ImageFileReader!SetFileName()}
  //  \index{itk::ImageFileWriter!SetFileName()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We can now connect these readers and writers to filters in a
  //  pipeline. For example, we can create a short pipeline by passing the
  //  output of the reader directly to the input of the writer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Explicitly declaring the specific VTKImageIO allow users to
  //  invoke methods specific to a particular IO class. For example, the
  //  following line specifies to the writer to use ASCII format when writing
  //  the pixel data.
  //
  //  \index{itk::VTKImageIO!SetFileTypeToASCII()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  vtkIO->SetFileTypeToASCII();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The VTKImageIO object is then connected to the
  //  ImageFileWriter.  This will short-circuit the action of the
  //  ImageIOFactory mechanism. The ImageFileWriter will
  //  not attempt to look for other ImageIO objects capable of
  //  performing the writing tasks. It will simply invoke the one provided by
  //  the user.
  //
  //  \index{itk::ImageFileWriter!SetImageIO()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  writer->SetImageIO( vtkIO );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally we invoke Update() on the ImageFileWriter and
  //  place this call inside a try/catch block in case any errors occur during
  //  the writing process.
  //
  //  \index{itk::ImageFileWriter!SetImageIO()}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Although this example only illustrates how to use an explicit
  //  ImageIO class with the ImageFileWriter, the same
  //  can be done with the ImageFileReader. The typical case in
  //  which this is done is when reading raw image files with the
  //  \doxygen{RawImageIO} object. The drawback of this approach is that the
  //  parameters of the image have to be explicitly written in the code.  The
  //  direct use of raw files is \textbf{strongly discouraged} in medical
  //  imaging.  It is always better to create a header for a raw file by
  //  using any of the file formats that combine a text header file and a raw
  //  binary file, like \doxygen{MetaImageIO}, \doxygen{GiplImageIO} and
  //  \doxygen{VTKImageIO}.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
