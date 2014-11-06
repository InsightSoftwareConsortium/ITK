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
//  It is often valuable to be able to query the entries from the header of a
//  DICOM file. This can be used for consistency checking, or simply for
//  verifying that we have the correct dataset in our hands.  This example
//  illustrates how to read a DICOM file and then print out most of the DICOM
//  header information. The binary fields of the DICOM header are skipped.
//
//  \index{DICOM!Header}
//  \index{DICOM!Tags}
//  \index{DICOM!Printing Tags}
//  \index{DICOM!Dictionary}
//  \index{DICOM!GDCM}
//  \index{GDCM!Dictionary}
//
//  Software Guide : EndLatex

// Software Guide : BeginLatex
//
// The headers of the main classes involved in this example are specified
// below. They include the image file reader, the GDCMImageIO object, the
// MetaDataDictionary and its entry element, the MetaDataObject.
//
// \index{MetaDataDictionary!header}
// \index{MetaDataObject!header}
// \index{GDCMImageIO!header}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkGDCMImageIO.h"
#include "itkMetaDataObject.h"
// Software Guide : EndCodeSnippet

#include "gdcmGlobal.h"

// Software Guide : BeginLatex
// Software Guide : EndLatex

int main( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomFile [user defined dict]" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  //  We instantiate the type to be used for storing the image once it is read
  //  into memory.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef signed short       PixelType;
  const unsigned int         Dimension = 2;

  typedef itk::Image< PixelType, Dimension >      ImageType;
  // Software Guide : EndCodeSnippet


  if( argc == 3 )
    {
    // Specify a path where XML dicts can be found (Part 3/4 & 6)
    gdcm::Global::GetInstance().Prepend( itksys::SystemTools::GetFilenamePath(argv[2]).c_str() );
    // Load them !
    gdcm::Global::GetInstance().LoadResourcesFiles();
    }

  // Software Guide : BeginLatex
  //
  // Using the image type as a template parameter we instantiate the type of the
  // image file reader and construct one instance of it.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >     ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The GDCM image IO type is declared and used for constructing one image IO
  // object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO       ImageIOType;
  ImageIOType::Pointer dicomIO = ImageIOType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We pass to the reader the filename of the image to be read and connect the
  // ImageIO object to it too.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  reader->SetFileName( argv[1] );
  reader->SetImageIO( dicomIO );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The reading process is triggered with a call to the \code{Update()} method.
  // This call should be placed inside a \code{try/catch} block because its
  // execution may result in exceptions being thrown.
  //
  // Software Guide : EndLatex

  try
    {
    // Software Guide : BeginCodeSnippet
    reader->Update();
    // Software Guide : EndCodeSnippet
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Now that the image has been read, we obtain the MetaDataDictionary from
  // the ImageIO object using the \code{GetMetaDataDictionary()} method.
  //
  // \index{MetaDataDictionary}
  // \index{GetMetaDataDictionary()!ImageIOBase}
  // \index{ImageIOBase!GetMetaDataDictionary()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MetaDataDictionary   DictionaryType;

  const  DictionaryType & dictionary = dicomIO->GetMetaDataDictionary();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Since we are interested only in the DICOM tags that can be expressed in
  // strings, we declare a MetaDataObject suitable for managing strings.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::MetaDataObject< std::string > MetaDataStringType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We instantiate the iterators that will make possible to walk through all the
  // entries of the MetaDataDictionary.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  DictionaryType::ConstIterator itr = dictionary.Begin();
  DictionaryType::ConstIterator end = dictionary.End();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // For each one of the entries in the dictionary, we check first if its element
  // can be converted to a string, a \code{dynamic\_cast} is used for this purpose.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  while( itr != end )
    {
    itk::MetaDataObjectBase::Pointer  entry = itr->second;

    MetaDataStringType::Pointer entryvalue =
      dynamic_cast<MetaDataStringType *>( entry.GetPointer() );
    // Software Guide : EndCodeSnippet


    // Software Guide : BeginLatex
    //
    // For those entries that can be converted, we take their DICOM tag and pass
    // it to the \code{GetLabelFromTag()} method of the GDCMImageIO class. This
    // method checks the DICOM dictionary and returns the string label
    // associated with the tag that we are providing in the \code{tagkey}
    // variable. If the label is found, it is returned in \code{labelId}
    // variable. The method itself returns false if the tagkey is not found in
    // the dictionary.  For example "$0010|0010$" in \code{tagkey} becomes
    // "Patient's Name" in \code{labelId}.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    if( entryvalue )
      {
      std::string tagkey   = itr->first;
      std::string labelId;
      bool found =  itk::GDCMImageIO::GetLabelFromTag( tagkey, labelId );
      // Software Guide : EndCodeSnippet

      // Software Guide : BeginLatex
      //
      // The actual value of the dictionary entry is obtained as a string with the
      // \code{GetMetaDataObjectValue()} method.
      //
      // \index{MetaDataObject!GetMetaDataObjectValue()}
      //
      // Software Guide : EndLatex

      // Software Guide : BeginCodeSnippet
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      // Software Guide : EndCodeSnippet

      // Software Guide : BeginLatex
      //
      // At this point we can print out an entry by concatenating the DICOM Name or
      // label, the numeric tag and its actual value.
      //
      // Software Guide : EndLatex

      // Software Guide : BeginCodeSnippet
      if( found )
        {
        std::cout << "(" << tagkey << ") " << labelId;
        std::cout << " = " << tagvalue.c_str() << std::endl;
        }
      // Software Guide : EndCodeSnippet
      else
        {
        std::cout << "(" << tagkey <<  ") " << "Unknown";
        std::cout << " = " << tagvalue.c_str() << std::endl;
        }
      }

    // Software Guide : BeginLatex
    //
    // Finally we just close the loop that will walk through all the Dictionary
    // entries.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    ++itr;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  It is also possible to read a specific tag. In that case the string of the
  //  entry can be used for querying the MetaDataDictionary.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::string entryId = "0010|0010";
    DictionaryType::ConstIterator tagItr = dictionary.Find( entryId );
  // Software Guide : EndCodeSnippet
  // Software Guide : BeginLatex
  //
  // If the entry is actually found in the Dictionary, then we can attempt to
  // convert it to a string entry by using a \code{dynamic\_cast}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  if( tagItr != end )
    {
    MetaDataStringType::ConstPointer entryvalue =
     dynamic_cast<const MetaDataStringType *>(
                                 tagItr->second.GetPointer() );
    // Software Guide : EndCodeSnippet


    // Software Guide : BeginLatex
    //
    // If the dynamic cast succeeds, then we can print out the values of the label,
    // the tag and the actual value.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    if( entryvalue )
      {
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << "Patient's Name (" << entryId <<  ") ";
      std::cout << " is: " << tagvalue.c_str() << std::endl;
      }
    // Software Guide : EndCodeSnippet
    }

  // Software Guide : BeginLatex
  //
  //  Another way to read a specific tag is to use the encapsulation above
  //  MetaDataDictionary. Note that this is stricly equivalent to the above
  //  code.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::string tagkey = "0008|1050";
  std::string labelId;
  if( itk::GDCMImageIO::GetLabelFromTag( tagkey, labelId ) )
    {
    std::string value;
    std::cout << labelId << " (" << tagkey << "): ";
    if( dicomIO->GetValueFromTag(tagkey, value) )
      {
      std::cout << value;
      }
    else
      {
      std::cout << "(No Value Found in File)";
      }
    std::cout << std::endl;
    }
  else
    {
    std::cerr << "Trying to access inexistant DICOM tag." << std::endl;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // For a full description of the DICOM dictionary please look at the file.
  //
  // \code{Insight/Utilities/gdcm/Dicts/dicomV3.dic}
  //
  // Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  // The following piece of code will print out the proper pixel type /
  // component for instantiating an \doxygen{ImageFileReader} that can properly
  // import the printed DICOM file.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itk::ImageIOBase::IOPixelType pixelType
                                       = reader->GetImageIO()->GetPixelType();
  itk::ImageIOBase::IOComponentType componentType
                                   = reader->GetImageIO()->GetComponentType();
  std::cout << "PixelType: " << reader->GetImageIO()
                               ->GetPixelTypeAsString(pixelType) << std::endl;
  std::cout << "Component Type: " << reader->GetImageIO()
                       ->GetComponentTypeAsString(componentType) << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
