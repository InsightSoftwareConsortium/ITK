/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadExportVTK.cxx
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
//  In the cases where the user knows what file format to use and want to
//  define it at compile time, it is possible to explicitly instantiate the
//  \doxygen{ImageIO} class capable of managing this format. This prevents the
//  \doxygen{ImageIOFactory} mechanism from trying to find an ImageIO object
//  appropriate for this image type. It also facilitates that the user
//  customize the read/write options to be used, since many of these options
//  are file format specific and can only be selected directly in the
//  corresponding ImageIO class.
//
//  The following example illustrates this way of performing IO by explicitly
//  instantiating an \doxygen{VTKImageIO} class, setting its parameters and then
//  connecting it to the \doxygen{ImageFileWriter}.
//
//  The following headers have to be included first.
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
    return -1;
    }



  //  Software Guide : BeginLatex
  //
  //  Then, as usual, we select the pixel types and the image dimension. Still
  //  keeping in mind that if the file happens to contain an image in another
  //  pixel type representation, casting will be performed with the simple
  //  C-language rules.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef unsigned short      PixelType;
  const   unsigned int        Dimension = 2;

  typedef itk::Image< PixelType, Dimension >    ImageType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //
  //  We can now instantiate the types of the reader and writer. These two
  //  classes are parameterized over the image type. We instantiate the
  //  \doxygen{VTKImageIO} class as well. Note that the ImageIO objects are not
  //  templated.
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
  //  Then, we create one object of each type using the \code{New()} method and
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
  //  Explicitly declaring the specific \doxygen{VTKImageIO} allow users to
  //  change the settings. For example, the following line force the writer to
  //  use ASCII format when writing the pixel data.
  // 
  //  \index{itk::VTKImageIO!SetFileTypeToASCII()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  vtkIO->SetFileTypeToASCII();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The \doxygen{VTKImageIO} object is then connected to the
  //  \doxygen{ImageFileWriter}.  This will short-circuit the action of the
  //  \doxygen{ImageIOFactory} mechanism. The \doxygen{ImageFileWriter} will
  //  not attempt to look for other \doxygen{ImageIO} objects capable of
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
  //  Finally we invoke \code{Update()} on the \doxygen{ImageFileWriter} and
  //  place this call inside a try/catch block in case any errors ocurr during
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
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Although this example only illustrates how to use an explicit
  //  \doxygen{ImageIO} class with the \doxygen{ImageFileWriter}, the same can
  //  be done with the \doxygen{ImageFileReader}. The typical case in which
  //  this is done is when reading raw image files with the
  //  \doxygen{RawImageIO} object. The drawback of this approach is that the
  //  parameters of the image have to be explicitly written in the code.  The
  //  direct use of raw file is \textbf{strongly discouraged} in medical
  //  imaging.  In case you have a raw file, it is always better to create a
  //  header for it using any of the file formats that combine a text header
  //  file and a raw binary file, like \doxygen{MetaImgaIO},
  //  \doxygen{GiplImageIO} and \doxygen{VTKImageIO}.
  // 
  //  Software Guide : EndLatex 


  return 0;


}



