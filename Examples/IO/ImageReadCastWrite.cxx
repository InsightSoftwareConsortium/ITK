/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadCastWrite.cxx
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
//  Given that \href{http://www.itk.org}{ITK} is based on a Generic Programming
//  paradigm, most of the types are defined at comipilation time. It is
//  sometimes important to anticipate conversion from different types of
//  images. The following example illustres the common case of reading an image
//  of one pixel type and writing it on a different pixel type. This process
//  not only involve casting but also rescaling the image intensity since the
//  dynamic range of the input and output pixel types can be quite different.
//  The \doxygen{RescaleIntensityImageFilter} is used here to linearly rescale
//  the image values.
//
//  The first step for performing reading and writing is to include the
//  following headers.
//
//  \index{itk::ImageFileReader!header}
//  \index{itk::ImageFileWriter!header}
//  \index{itk::RescaleIntensityImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
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
  //  represent the images. Note that, when reading an image, this pixel type
  //  \textbf{is not necessarily} the pixel type of the image stored in the file.
  //  Instead, it is the type that will be used to store the image as soon as
  //  it is read into memory. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef float               InputPixelType;
  typedef unsigned char       OutputPixelType;

  const   unsigned int        Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
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
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  Below we instantiate the type of the
  //  \doxygen{RescaleIntensityImageFilter} that will linearly scale the image
  //  intensities.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter< 
                                  InputImageType, 
                                  OutputImageType >    FilterType;
  // Software Guide : EndCodeSnippet

  


  //  Software Guide : BeginLatex
  //  
  //  A filter object is constructed and the minimum and maximum values of the
  //  output are selected using the \code{SetOutputMinimum()} and
  //  \code{SetOutputMaximum()} methods.
  //
  //  \index{itk::RescaleIntensityImageFilter!SetOutputMinimum()}
  //  \index{itk::RescaleIntensityImageFilter!SetOutputMaximum()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();

  filter->SetOutputMinimum(   0 );
  filter->SetOutputMaximum( 255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, we create the reader and writer and connect the pipeline.
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

  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  // Software Guide : EndCodeSnippet




  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];




  //  Software Guide : BeginLatex
  //
  //  The name of the files to be read and written are passed with the
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
  //  Finally we trigger the execution of the pipeline with the \code{Update()}
  //  method on the writer. The output image will then be the scaled and casted
  //  version of the input image.
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



  return 0;


}



