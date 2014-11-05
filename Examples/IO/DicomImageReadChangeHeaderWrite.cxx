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
//  This example illustrates how to read a single DICOM slice and write it back
//  with some changed header information as another DICOM slice. Header
//  Key/Value pairs can be specified on the command line. The keys are defined
//  in the file
//
//  \code{Insight/Utilities/gdcm/Dicts/dicomV3.dic}.
//
//  Please note that modifying the content of a DICOM header is a very risky
//  operation. The header contains fundamental information about the patient
//  and therefore its consistency must be protected from any data corruption.
//  Before attempting to modify the DICOM headers of your files, you must make
//  sure that you have a very good reason for doing so, and that you can ensure
//  that this information change will not result in a lower quality of health
//  care being delivered to the patient.
//
//  \index{DICOM!Changing Headers}
//
//  Software Guide : EndLatex

// Software Guide : BeginLatex
//
// We must start by including the relevant header files. Here we include the
// image reader, image writer, the image, the metadata dictionary and its
// entries, the metadata objects and the GDCMImageIO. The metadata dictionary
// is the data container that stores all the entries from the DICOM header once
// the DICOM image file is read into an ITK image.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkMetaDataObject.h"
#include "itkGDCMImageIO.h"
// Software Guide : EndCodeSnippet

#include <list>
#include <fstream>

int main(int argc, char* argv[])
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomImage OutputDicomImage Entry Value\n";
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// We declare the image type by selecting a particular pixel type and image
// dimension.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef signed short InputPixelType;
  const unsigned int   Dimension = 2;
  typedef itk::Image< InputPixelType, Dimension > InputImageType;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We instantiate the reader type by using the image type as template
// parameter. An instance of the reader is created and the file name to be read
// is taken from the command line arguments.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The GDCMImageIO object is created in order to provide the services for
// reading and writing DICOM files. The newly created image IO class is
// connected to the reader.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO           ImageIOType;
  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();
  reader->SetImageIO( gdcmImageIO );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The reading of the image is triggered by invoking \code{Update()} in the
// reader.
//
// Software Guide : EndLatex


  try
    {
// Software Guide : BeginCodeSnippet
    reader->Update();
// Software Guide : EndCodeSnippet
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// We take the metadata dictionary from the image that the reader had loaded
// in memory.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  InputImageType::Pointer inputImage = reader->GetOutput();
  typedef itk::MetaDataDictionary   DictionaryType;
  DictionaryType & dictionary = inputImage->GetMetaDataDictionary();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Now we access the entries in the metadata dictionary, and for particular
// key values we assign a new content to the entry. This is done here by taking
// \{key,value\} pairs from the command line arguments. The relevant method is
// \code{EncapsulateMetaData} that takes the dictionary and for a given key
// provided by \code{entryId}, replaces the current value with the content of
// the \code{value} variable. This is repeated for every potential pair present
// in the command line arguments.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  for (int i = 3; i < argc; i+=2)
    {
    std::string entryId( argv[i] );
    std::string value( argv[i+1] );
    itk::EncapsulateMetaData<std::string>( dictionary, entryId, value );
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Now that the dictionary has been updated, we proceed to save the image. This
// output image will have the modified data associated with its DICOM header.
//
// Using the image type, we instantiate a writer type and construct a writer.
// A short pipeline between the reader and the writer is connected. The
// filename to write is taken from the command line arguments. The image IO
// object is connected to the writer.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< InputImageType >  Writer1Type;

  Writer1Type::Pointer writer1 = Writer1Type::New();

  writer1->SetInput( reader->GetOutput() );
  writer1->SetFileName( argv[2] );
  writer1->SetImageIO( gdcmImageIO );
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// Execution of the writer is triggered by invoking the \code{Update()} method.
//
// Software Guide : EndLatex

  try
    {
// Software Guide : BeginCodeSnippet
    writer1->Update();
// Software Guide : EndCodeSnippet
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Remember again, that modifying the header entries of a DICOM file involves
  // very serious risks for patients and therefore must be done with extreme
  // caution.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;

}
