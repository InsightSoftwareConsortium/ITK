/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadWrite.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


//  Software Guide : BeginLatex
//
//  The classes responsible for reading and writing images are typically set at
//  the ends of the data processing pipeline. These classes can be seen as data
//  sources and data sinks.  For practical purposes you may think of them as
//  being yet another filter.
//
//  Image reading is managed by the class \doxygen{ImageFileReader} while
//  writing is done by the class \doxygen{ImageFileWriter}. These two classes
//  are totally independent of any particular file format. The actual low level
//  task of reading and writing specific file formats will be done behind the
//  scenes by a family of objects called \doxygen{ImageIO}. 
//
//  The first step for performing reading and writing is to include the
//  following headers.
//
//  \index{itk::ImageFileReader|textbf}
//  \index{itk::ImageFileReader!header}
//
//  \index{itk::ImageFileWriter|textbf}
//  \index{itk::ImageFileWriter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet



#include "itkImage.h"



int main( int argc, char ** argv )
{

  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile " << std::endl;
    return -1;
    }



  //  Software Guide : BeginLatex
  //
  //  Then, as usual, a decision should be made about the type of pixel used to
  //  represent the images. Note that, when reading and image, this pixel type
  //  \textbf{is not necessarily} the pixel type of the image stored in the file.
  //  Instead, it is the type that will be used to store the image as soon as
  //  it is read into memory. Your choice of the pixel type should be driven
  //  mainly by two considerations
  //
  //  \begin{itemize}
  //  \item It should be possible to cast the file pixel type  to the pixel
  //  type you select. This casting will be performed using the standard
  //  C-language rules, so you will have to make sure that the conversion does
  //  not result in information being lost.
  //  \item The pixel type in memory should be appropriate for the type of
  //  processing you intended to apply on the images. 
  //  \end{itemize}
  //
  //  A typical selection for medical images could be the one illustrated in
  //  the following lines.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned short      PixelType;
  const   unsigned int        Dimension = 2;

  typedef itk::Image< PixelType, Dimension >    ImageType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  Note that the dimension of the image in memory should match the one of
  //  the image in file. There are a couple of special cases in which this
  //  condition may be relaxed, but in general it is better to ensure that both
  //  dimensions match.
  //
  //  We can now instantiate the types of the reader and writer. These two
  //  classes are parameterized over the image type.
  //
  //  \index{itk::ImageFileReader!Instantiation}
  //  \index{itk::ImageFileWriter!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //
  //  Then, we create one object of each type using the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::ImageFileReader!New()}
  //  \index{itk::ImageFileWriter!New()}
  //  \index{itk::ImageFileReader!SmartPointer}
  //  \index{itk::ImageFileWriter!SmartPointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  // Software Guide : EndCodeSnippet




  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];




  //  Software Guide : BeginLatex
  //
  //  The name of the file to be read or written is passed with the
  //  \code{SetFileName()} method. 
  //
  //  \index{itk::ImageFileReader!SetFileName()}
  //  \index{itk::ImageFileWriter!SetFileName()}
  //  \index{SetFileName()!itk::ImageFileReader}
  //  \index{SetFileName()!itk::ImageFileWriter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( inputFilename  );
  writer->SetFileName( outputFilename );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We can now connect these source and sink objects to filters in a
  //  pipeline. For example, we can create the shortest pipeline by passing the
  //  output of the reader directly to the input of the writer. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  At first view, this may seem as a quite useless program, but it is
  //  actually implementing a powerful file format conversion tool !. The
  //  execution of the pipeline is triggered by the invokation of the
  //  \code{Update()} methods in one of the final objects. In this case, the
  //  final data pipeline object is the writer. It is a wise practice of
  //  defensive programming to insert any \code{Update()} call inside a
  //  \code{try/catch} block in case exceptions are thrown during the execution
  //  of the pipeline. 
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  try 
    { 
    writer->Update(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that exceptions should only be caught by pieces of code that know
  //  what to do with them. In a typical application this catch block should
  //  probably reside on the GUI code. The action on the catch block could show
  //  a message to inform the user that the IO operation have failed.
  //
  //  The IO architecture of the toolkit makes possible to avoid any mention of
  //  the specific file format used to read or write the images. By default,
  //  file formats are choosen based on the extension of the filenames. This
  //  behavior can be overriden by providing the reader or writer with specific
  //  \doxygen{ImageIO} objects.
  //
  //  For historical reasons, the \doxygen{ImageFileWriter} also has a
  //  \code{Write()} method which performs the exact same action than the
  //  \code{Update()} method. You could in principle use any of them but
  //  \code{Update()} is the recommended one since \code{Write()} will likely
  //  be deprecated in the future. 
  //
  //  Software Guide : EndLatex 


  return 0;


}



