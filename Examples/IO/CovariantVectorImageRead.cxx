/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CovariantVectorImageRead.cxx
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
//  \code{CovariantVector}. For practical purposes all the content in this
//  example is also applicable to images of pixel type \doxygen{Vector},
//  \doxygen{Point} and \doxygen{FixedArray}. These pixel types have in common
//  that they are all arrays of fixed size in which the components have the
//  same representation type.
//
//  As processing in this example we are reading an gradient image from a file
//  and computing its magnitude using the
//  \doxygen{GradientToMagnitudeImageFilter}. Note that this filter is
//  different from the \doxygen{GradientMagnitudeImageFilter} which actually
//  takes a scalar image as input and compute the magnitude of its gradient.
//  The \doxygen{GradientToMagnitudeImageFilter} take an image of vector pixel
//  type as input and computes pixel-wise the magnitue of each vector.
//
//
//  Let's start by including the relevant header files.
//
//  \index{ImageFileRead!Vector images}
//  \index{GradientToMagnitudeImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkGradientToMagnitudeImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
// Software Guide : EndCodeSnippet




#include "itkImage.h"



int main( int argc, char ** argv )
{

  // Verify the number of parameters in the command line
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile  outputVectorImageFile " << std::endl;
    return -1;
    }





  //  Software Guide : BeginLatex
  //
  //  We select to read an image of \doxygen{CovariantVector} pixels and
  //  compute its magnitudes to produce an image where each pixel is of type
  //  \code{unsigned short}. The componets of the \doxygen{CovariantVector} are
  //  selected to be \code{float} here. Notice that a renormalization is
  //  required here in order to map the dynamic range of the magnitude values
  //  into the range of the output pixel type.  The
  //  \doxygen{RescaleIntensityImageFilter} is used to that end.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef float                 ComponentType;

  const   unsigned int          Dimension = 2;

  typedef itk::CovariantVector< ComponentType, 
                                    Dimension  >      InputPixelType;

  typedef float                                       MagnitudePixelType;

  typedef unsigned short                              OutputPixelType;
  
  typedef itk::Image< InputPixelType,      Dimension >    InputImageType;
  typedef itk::Image< MagnitudePixelType,  Dimension >    MagnitudeImageType;
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
  //  The \doxygen{GradientToMagnitudeImageFilter} type is instantiated
  //  using the input and output image types. A filter object is created with
  //  the \code{New()} method and assigned to a \doxygen{SmartPointer}.
  //
  //  \index{GradientToMagnitudeImageFilter!Instantiation}
  //  \index{GradientToMagnitudeImageFilter!New()}
  //  \index{GradientToMagnitudeImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientToMagnitudeImageFilter< 
                                          InputImageType,
                                          MagnitudeImageType    > FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet



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
                                  MagnitudeImageType, 
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
  
  rescaler->SetInput( filter->GetOutput() );

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



  return 0;


}



