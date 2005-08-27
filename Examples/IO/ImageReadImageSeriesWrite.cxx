/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadImageSeriesWrite.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

//  Software Guide : BeginLatex
//  
//  This example illustrates how to save an image using the
//  \doxygen{ImageSeriesWriter}. This class allows to save a 3D volume as a set
//  of files containting one 2D slice per file.
//
//  \index{itk::ImageFileReader!header}
//  \index{itk::ImageSeriesWriter!header}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"

int main( int argc, char *argv[] )
{
  if (argc < 4 )
    {
    std::cerr << "Usage: ImageReadImageSeriesWrite inputFile outputPrefix outputExtension" << std::endl;
    return EXIT_FAILURE;
    }


//  Software Guide : BeginLatex
//
//  The type of the input image is declared here and it is used for declaring
//  the type of the reader. This will be a conventional 3D image reader. 
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Image< unsigned char, 3 >      ImageType;
  typedef itk::ImageFileReader< ImageType >   ReaderType;
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  The reader object is constructed using the \code{New()} operator and
//  assigning the result to a \code{SmartPointer}. The filename of the 3D
//  volume to be read is taken from the command line arguments and passed to
//  the reader using the \code{SetFileName()} method.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
// Software Guide : EndCodeSnippet
  


//  Software Guide : BeginLatex
//
//  The type of the series writer must be instantiated taking into account that
//  the input file is a 3D volume and the output files are 2D images.
//  Additionally, the output of the reader is connected as input to the writer.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::Image< unsigned char, 2 >     Image2DType;

  typedef itk::ImageSeriesWriter< ImageType, Image2DType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  The writer requires a list of filenames to be generated. This list can be
//  produced with the help of the \doxygen{NumericSeriesFileNames} class.
//
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  The \code{NumericSeriesFileNames} class requires an input string in order
//  to have a template for generating the filenames of all the output slices.
//  Here we compose this string using a prefix taken from the command line
//  arguments and adding the extension for PNG files.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  std::string format = argv[2];
  format += "%03d.";
  format += argv[3];   // filename extension

  nameGenerator->SetSeriesFormat( format.c_str() );
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  The input string is going to be used for generating filenames by setting
//  the values of the first and last slice. This can be done by collecting
//  information from the input image. Note that before attempting to take any
//  image information from the reader, its execution must be triggered with
//  the invokation of the \code{Update()} method, and since this invokation
//  can potentially throw exceptions, it must be put inside a
//  \code{try/catch} block.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the image" << std::endl;
    std::cerr << excp << std::endl;
    }
// Software Guide : EndCodeSnippet

 


// Software Guide : BeginLatex
//
// Now that the image has been read we can query its largest possible region
// and recover information about the number of pixels along every dimension.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  ImageType::ConstPointer inputImage = reader->GetOutput();
  ImageType::RegionType   region     = inputImage->GetLargestPossibleRegion();
  ImageType::IndexType    start      = region.GetIndex(); 
  ImageType::SizeType     size       = region.GetSize(); 
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// With this information we can find the number that will identify the first
// and last slices of the 3D data set. This numerical values are then passed to
// the filenames generator object that will compose the names of the files
// where the slices are going to be stored.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const unsigned int firstSlice = start[2];
  const unsigned int lastSlice  = start[2] + size[2] - 1;

  nameGenerator->SetStartIndex( firstSlice );
  nameGenerator->SetEndIndex( lastSlice );
  nameGenerator->SetIncrementIndex( 1 );
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  The list of filenames is taken from the names generator and it is passed to
//  the series writer. 
//  
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  writer->SetFileNames( nameGenerator->GetFileNames() );
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  Finally we trigger the execution of the pipeline with the Update() method
//  on the writer. At this point the slices of the image will be saved in
//  individual files containing a single slice per file. The filenames used for
//  these slices are those produced by the filenames generator.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the image" << std::endl;
    std::cerr << excp << std::endl;
    }
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Note that by saving data into isolated slices we are lossing information
// that may be significant for medical applications, such as the interslice
// spacing in millimeters.
//
// Software Guide : EndLatex 



  return EXIT_SUCCESS;
}


