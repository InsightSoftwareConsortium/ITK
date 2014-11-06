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
//  This example illustrates how to read an image whose pixel type is
//  \code{CovariantVector}, extract one of its components to form a scalar
//  image and finally save this image into a file.
//
//  The \doxygen{VectorIndexSelectionCastImageFilter} is used to extract
//  a scalar from the vector image. It is also possible to cast the component
//  type when using this filter. It is the user's responsibility to make sure
//  that the cast will not result in any information loss.
//
//  Let's start by including the relevant header files.
//
//  \index{itk::ImageFileRead!Vector images}
//  \index{itk::Vector\-Index\-Selection\-Cast\-Image\-Filter!header}
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
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We read an image of \doxygen{CovariantVector} pixels and extract one of
  //  its components to generate a scalar image of a consistent pixel type.
  //  Then, we rescale the intensities of this scalar image and write it as an
  //  image of \code{unsigned short} pixels.
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
  //  The \doxygen{ImageFileReader} and \doxygen{ImageFileWriter}
  //  are instantiated using the image types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The VectorIndexSelectionCastImageFilter is instantiated
  //  using the input and output image types. A filter object is created with
  //  the \code{New()} method and assigned to a \doxygen{SmartPointer}.
  //
  //  \index{itk::Vector\-Index\-Selection\-Cast\-Image\-Filter!Instantiation}
  //  \index{itk::Vector\-Index\-Selection\-Cast\-Image\-Filter!New()}
  //  \index{itk::Vector\-Index\-Selection\-Cast\-Image\-Filter!Pointer}
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
  //  The VectorIndexSelectionCastImageFilter class requires us to specify
  //  which of the vector components is to be extracted from the vector image.
  //  This is done with the \code{SetIndex()} method. In this example we obtain
  //  this value from the command line arguments.
  //
  //  \index{itk::Vector\-Index\-Selection\-Cast\-Image\-Filter!SetIndex()}
  //
  //  Software Guide : EndLatex

  const unsigned int indexOfComponentToExtract = atoi( argv[4] );

  if( indexOfComponentToExtract >= Dimension )
    {
    std::cerr << "You are requesting an index out of the range for the Vector dimension" << std::endl;
    std::cerr << "Vector dimension is = " << Dimension << std::endl;
    std::cerr << "but your requested index = " << indexOfComponentToExtract << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginCodeSnippet
  componentExtractor->SetIndex( indexOfComponentToExtract );
  //  Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \doxygen{RescaleIntensityImageFilter} filter is instantiated here.
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
  //  The minimum and maximum values for the output image are specified in
  //  the following. Note the use of the \doxygen{NumericTraits} class which
  //  allows us to define a number of type-related constants in a generic
  //  way. The use of traits is a fundamental characteristic of generic
  //  programming~\cite{Austern1999,Alexandrescu2001}.
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
  //  Below, we create the reader and writer using the \code{New()} method and
  //  assign the result to a \doxygen{SmartPointer}.
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


  // Here we recover the file names from the command line arguments
  //
  const char * inputFilename  = argv[1];
  const char * outputFilename = argv[3];


  //  Software Guide : BeginLatex
  //
  //  The name of the file to be read or written is passed to the
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
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  // Here We add another writer that will produce the non-normalized output
  // file
  //
  typedef itk::ImageFileWriter< ComponentImageType >  ComponentWriterType;
  ComponentWriterType::Pointer componentWriter = ComponentWriterType::New();
  componentWriter->SetInput( componentExtractor->GetOutput() );
  componentWriter->SetFileName( argv[2] );
  componentWriter->Update();

  return EXIT_SUCCESS;
}
