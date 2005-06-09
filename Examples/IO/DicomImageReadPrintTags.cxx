/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomImageReadPrintTags.cxx
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

//  Software Guide : BeginLatex
//
//  It is often valuable to be able to query the entries from the header of a
//  DICOM file. This can be used for checking for consistency, or simply for
//  verifying that we have the correct dataset in our hands.  This example
//  illustrates how to read a DICOM file and then print out most of the DICOM
//  header information. The binary field of the DICOM header are skipped.
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
// below. They include the image file reader, the GDCM image IO object, the
// Meta data dictionary and its entry element the Meta data object. 
//
// \index{MetaDataDictionary!header}
// \index{MetaDataObject!header}
// \index{GDCMImageIO!header}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkGDCMImageIO.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
// Software Guide : EndLatex





int main( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomFile " << std::endl;
    return EXIT_FAILURE;
    }





// Software Guide : BeginLatex
// 
//  We instantiate then the type to be used for storing the image once it is
//  read into memory.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef signed short       PixelType;
  const unsigned int         Dimension = 2;
  
  typedef itk::Image< PixelType, Dimension >      ImageType;
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
// 
// Using the image type as template parameter we instantiate the type of the
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
// Now that the image has been read, we obtain the Meta data dictionary from
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


  DictionaryType::ConstIterator itr = dictionary.Begin();
  DictionaryType::ConstIterator end = dictionary.End();

  
  typedef itk::MetaDataObject< std::string > MetaDataStringType;




//  Software Guide : BeginLatex
//
// Do some gdcm low level access to translate the DICOM Tag into the 
// associated DICOM name. For example "0010|0010" becomes "Patient's Name"
//
//  Software Guide : EndLatex 
  while( itr != end )
    {
    itk::MetaDataObjectBase::Pointer  entry = itr->second;

    MetaDataStringType::Pointer entryvalue = 
      dynamic_cast<MetaDataStringType *>( entry.GetPointer() ) ;

    if( entryvalue )
      {

      std::string tagkey   = itr->first;
      std::string labelId;

      bool found =  itk::GDCMImageIO::GetLabelFromTag( tagkey, labelId );
      
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      // If tagkey was found (ie DICOM tag from public dictionary), 
      // then display the name:
      if( found )
        {
        std::cout << "(" << tagkey << ") " << labelId;
        std::cout << " = " << tagvalue.c_str() << std::endl;
        }
      else
        {
        std::cout << "(" << tagkey <<  ") " << "Unknown";
        std::cout << " = " << tagvalue.c_str() << std::endl;
        }
      }
    ++itr;
    }
  



//  Software Guide : BeginLatex
//
//  In order to read a specific tag, the string of the entry can be used for
//  quering the MetaDataDictionary.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
    std::string entryId = "0010|0010";
    itk::MetaDataObjectBase::ConstPointer  entry = dictionary[entryId];

    MetaDataStringType::ConstPointer entryvalue = 
      dynamic_cast<const MetaDataStringType *>( entry.GetPointer() ) ;

    if( entryvalue )
      {
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << "Patient's Name (" << entryId <<  ") ";
      std::cout << " is: " << tagvalue << std::endl;
      }
// Software Guide : EndCodeSnippet


  return EXIT_SUCCESS;

}

