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
//  This example illustrates the commonly used functionality of extracting a 2D
//  slice from a 3D volume. This is typically used for display purposes and for
//  expediting user feedback in interactive programs. Here we simply read a 3D 
//  volume, extract one of its slices and save it as a 2D image. A lot of caution
//  is in order when introducing the output 2D slice in a processing pipeline, since 
//  for most image processing operations, the application of a filter on a extracted
//  slice is not equivalent to first applying the filter in the volume and then 
//  extracting the slice.
//
//  In this example we start by including the appropriate header files.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//  
//  The filter used to extract a region from an image is the
//  \doxygen{ExtractImageFilter}. Its header is included below.  The
//  particularity of this filter is that it is capable of extracting
//  $(N-1)$-dimensional images from $N$-dimensional ones.
//
//  \index{itk::ExtractImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkExtractImageFilter.h"
// Software Guide : EndCodeSnippet



#include "itkImage.h"



int main( int argc, char ** argv )
{

  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " input3DImageFile  output2DImageFile " << std::endl;
    std::cerr << " sliceNumber " << std::endl;
    return -1;
    }



  //  Software Guide : BeginLatex
  //
  //  Image types are defined below. Note that the input image type is $3D$ and
  //  the output image type is $2D$.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef signed short        InputPixelType;
  typedef signed short        OutputPixelType;

  typedef itk::Image< InputPixelType,  3 >    InputImageType;
  typedef itk::Image< OutputPixelType, 2 >    OutputImageType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The types for the \doxygen{ImageFileReader} and \doxygen{ImageFileWriter}
  //  are instantiated using the image types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet



  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[2];


  //  Software Guide : BeginLatex
  //
  //  Below, we create the reader and writer  using the \code{New()} method and
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
  //  The \doxygen{ExtractImageFilter} type is instantiated using the input and
  //  output image types. A filter object is created with the \code{New()}
  //  method and assigned to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ExtractImageFilter< InputImageType, OutputImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The \doxygen{ExtractImageFilter} requires a region to be defined by the
  //  user. The region is specified by an \doxygen{Index} indicating the pixel
  //  where the region starts and an \doxygen{Size} indication how many pixels
  //  the region has along each dimension. In order to extract a $2D$ image from
  //  a $3D$ data set, it is enough to set of the dimensions of the region to $0$.
  //  This dimension will be suppressed by the \doxygen{ExtractImageFilter} during
  //  its execution. Here we take the region from the largest possible region of
  //  the input image. Note that \code{Update()} is being called first on the reader,
  //  since otherwise the output would have invalid data.
  //  
  //  Software Guide : EndLatex 

  
  // Software Guide : BeginCodeSnippet
  reader->Update();

  InputImageType::RegionType inputRegion =
           reader->GetOutput()->GetLargestPossibleRegion();

  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  We take the size from the region and collapse the size in the $Z$
  //  component by setting its value to $0$. This will indicate to the
  //  \doxygen{ExtractImageFilter} that the output image should have a
  //  dimension less than the input image.
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  InputImageType::SizeType size = inputRegion.GetSize();
  
  size[2] = 0;
  // Software Guide : EndCodeSnippet
 
  //  Software Guide : BeginLatex
  //  
  //  Note that in this case we are extracting a $Z$ slice, and for that reason,
  //  the dimension to be collapsed in the one with index $2$. You may keep in
  //  mind the association of index components $\{X=0,Y=1,Z=2\}$. If we were
  //  interested in extracting a slice perpendicular to the $Y$ axis we would
  //  have set \code{size[1]=0;}.
  //  
  //  Software Guide : EndLatex 



  //  Software Guide : BeginLatex
  //  
  //  Then, we take the index from the region and set its $Z$ value to the
  //  slice number we want to extract. In this example we obtain the slice
  //  number from the command line arguments.
  //  
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  InputImageType::IndexType start = inputRegion.GetIndex();
  
  const unsigned int sliceNumber = atoi( argv[3] );

  start[2] = sliceNumber;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  Finally, an \doxygen{ImageRegion} object is created and initialized with
  //  the start and size we just prepared using the slice information.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  InputImageType::RegionType wantedRegion;

  wantedRegion.SetSize(  size  );
  wantedRegion.SetIndex( start );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  Then the region is passed to the filter using the
  //  \code{SetExtractionRegion()} method.
  //
  //  \index{itk::ExtractImageFilter!SetExtractionRegion()}
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  filter->SetExtractionRegion( wantedRegion );
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



  //  Software Guide : BeginLatex
  //  
  //  Finally we execute the pipeline by invoking \code{Update()} on the
  //  writer. The call is placed in a \code{try/catch} block in case exceptions
  //  are thrown.
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



