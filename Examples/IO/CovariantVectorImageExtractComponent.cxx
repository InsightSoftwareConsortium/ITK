/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CovariantVectorImageExtractComponent.cxx
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
//  This example illustrates how to read an image whose pixel type is
//  \code{CovariantVector}, extract one of its components to form an scalar
//  image and save this image into a file. 
//
//  The \doxygen{VectorIndexSelectionCastImageFilter} is used to extract the
//  scalar component from the vector image. It is possible to perform and
//  additional casting on the component type when using this filter, it is up
//  to the user to make sure that the type castin will not incurr in
//  information losses.
//
//  Let's start by including the relevant header files.
//
//  \index{ImageFileRead!Vector images}
//  \index{VectorIndexSelectionCastImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
// Software Guide : EndCodeSnippet




#include "itkImage.h"



int main( int argc, char ** argv )
{

  // Verify the number of parameters in the command line
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputVectorImageFile  outputScalarImageFile";
    std::cerr << " outupNormalizedScalarImageFile";
    std::cerr << " componentToExtract" << std::endl;
    return -1;
    }





  //  Software Guide : BeginLatex
  //
  //  We select to read an image of \doxygen{CovariantVector} pixels and
  //  extract on of its components to generate a scalar image of a consistent
  //  pixel type.  Then, we rescale the intensities of this scalar image and
  //  write it as a image of \code{unsigned short}pixels.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef float                 ComponentType;

  const   unsigned int          Dimension = 2;

  typedef itk::CovariantVector< ComponentType, 
                                    Dimension  >      InputPixelType;

  typedef unsigned short                              OutputPixelType;
  
  typedef itk::Image< InputPixelType,      Dimension >    InputImageType;
  typedef itk::Image< ComponentType,       Dimension >    ComponentImageType;
  typedef itk::Image< OutputPixelType,     Dimension >    OutputImageType;
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



  //  Software Guide : BeginLatex
  //  
  //  The \doxygen{VectorIndexSelectionCastImageFilter} type is instantiated
  //  using the input and output image types. A filter object is created with
  //  the \code{New()} method and assigned to a \doxygen{SmartPointer}.
  //
  //  \index{VectorIndexSelectionCastImageFilter!Instantiation}
  //  \index{VectorIndexSelectionCastImageFilter!New()}
  //  \index{VectorIndexSelectionCastImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::VectorIndexSelectionCastImageFilter< 
                                          InputImageType,
                                          ComponentImageType    > FilterType;

  FilterType::Pointer componentExtractor = FilterType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The \doxygen{VectorIndexSelectionCastImageFilter} require us to specify
  //  which of the vector components is to be extracted from the vector image.
  //  This is done with the \code{SetIndex()} method. In this example we obtain
  //  this value from the command line arguments.
  //
  //  \index{VectorIndexSelectionCastImageFilter!SetIndex()}
  //
  //  Software Guide : EndLatex 

  const unsigned int indexOfComponentToExtract = atoi( argv[4] );

  //  Software Guide : BeginCodeSnippet
  componentExtractor->SetIndex( indexOfComponentToExtract );
  //  Software Guide : EndCodeSnippet 





  //  Software Guide : BeginLatex
  //  
  //  The \doxygen{RescaleIntensityImageFilter} type is instantiated here and
  //  one of its object is created.
  //
  //  \index{RescaleIntensityImageFilter!Instantiation}
  //  \index{RescaleIntensityImageFilter!New()}
  //  \index{RescaleIntensityImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter< 
                                  ComponentImageType, 
                                  OutputImageType >      RescaleFilterType; 

  RescaleFilterType::Pointer  rescaler = RescaleFilterType::New();                                  
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //  
  //  We set below the minumum and maximum values for the output image. Note
  //  the use of the \doxygen{NumericTraits} class which allows to define a
  //  number of type-related constant in a generic way. The use of traits is a
  //  fundamental characteristic of Generic Programming.
  //
  //  \index{RescaleIntensityImageFilter!SetOutputMinimum()}
  //  \index{RescaleIntensityImageFilter!SetOutputMaximum()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  rescaler->SetOutputMinimum( itk::NumericTraits< OutputPixelType >::min() );
  rescaler->SetOutputMaximum( itk::NumericTraits< OutputPixelType >::max() );
  //  Software Guide : EndCodeSnippet 




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




  //
  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[3];




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
  componentExtractor->SetInput( reader->GetOutput() );
  
  rescaler->SetInput( componentExtractor->GetOutput() );

  writer->SetInput( rescaler->GetOutput() );
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



  //
  // We add here another writer that will produce the non-normalized output file
  //
  typedef itk::ImageFileWriter< ComponentImageType >  ComponentWriterType;

  ComponentWriterType::Pointer componentWriter = ComponentWriterType::New();

  componentWriter->SetInput( componentExtractor->GetOutput() );
  componentWriter->SetFileName( argv[3] );
  componentWriter->Update();


  return 0;


}



