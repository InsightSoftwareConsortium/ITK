/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadExtractWrite.cxx
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
//  This example should arguably be placed in the Filtering section. However
//  its usefulness for typical IO operations makes interesting to mention it
//  here. The purpose of this example is to read and image, extract a subregion
//  and write this subregion to a file. This is a common task when we want to
//  apply first a computaionally intensive method to the region of interest of 
//  an image.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet

#include "itkExtractImageFilter.h"


#include "itkImage.h"



int main( int argc, char ** argv )
{

  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputImageFile " << std::endl;
    std::cerr << " startX startY sizeX sizeY" << std::endl;
    return -1;
    }



  //  Software Guide : BeginLatex
  //
  //  Image types are defined below.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef signed short        InputPixelType;
  typedef signed short        OutputPixelType;

  const   unsigned int        Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  // Software Guide : EndCodeSnippet



  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  typedef itk::ExtractImageFilter< InputImageType, OutputImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  // Software Guide : BeginCodeSnippet
  OutputImageType::IndexType start;

  start[0] = atoi( argv[3] );
  start[1] = atoi( argv[4] );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  OutputImageType::SizeType size;
  
  size[0] = atoi( argv[5] );
  size[1] = atoi( argv[6] );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  OutputImageType::RegionType wantedRegion;

  wantedRegion.SetSize(  size  );
  wantedRegion.SetIndex( start );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  filter->SetExtractionRegion( wantedRegion );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, we create one object of each type using the \code{New()} method and
  //  assigning the result to a \code{SmartPointer}.
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
  //  Below we connect the reader, filter and writer to form the data
  //  processing pipeline.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );

  writer->SetInput( filter->GetOutput() );
  // Software Guide : EndCodeSnippet


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



