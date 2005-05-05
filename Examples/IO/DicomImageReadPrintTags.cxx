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
//  This example illustrates how to read a DICOM file and then print
//  most of the DICOM header information (binary field are skipped).
//
//  Software Guide : EndLatex 


#include "itkImageFileReader.h"
#include "itkGDCMImageIO.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
// In order to manipulate gdcm dictionary
#include "gdcm/src/gdcmGlobal.h"
#include "gdcm/src/gdcmDictSet.h"

int main( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomFile " << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,3>          ImageType;
  typedef itk::ImageFileReader< ImageType >     ReaderType;

  typedef itk::GDCMImageIO                      ImageIOType;
  ImageIOType::Pointer dicomIO = ImageIOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetImageIO( dicomIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  
  typedef itk::MetaDataDictionary   DictionaryType;

  const  DictionaryType & dictionary = dicomIO->GetMetaDataDictionary();

  DictionaryType::ConstIterator itr = dictionary.Begin();
  DictionaryType::ConstIterator end = dictionary.End();

  
  typedef itk::MetaDataObject< std::string > MetaDataStringType;

//  Software Guide : BeginLatex
//
// Do some gdcm low level access to translate the DICOM Tag into the 
// associated DICOM name. For example "0010|0010" becomes "Patient's Name"
//
//  Software Guide : EndLatex 
  gdcm::Dict *pubDict = gdcm::Global::GetDicts()->GetDefaultPubDict();
  while( itr != end )
    {
    itk::MetaDataObjectBase::Pointer  entry = itr->second;

    MetaDataStringType::Pointer entryvalue = 
      dynamic_cast<MetaDataStringType *>( entry.GetPointer() ) ;

    if( entryvalue )
      {
      std::string tagkey   = itr->first;
      gdcm::DictEntry *dictentry = pubDict->GetEntry(tagkey);
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << dictentry->GetName() <<  " = " << tagvalue.c_str() << std::endl;
      }
    ++itr;
    }
  
//  Software Guide : BeginLatex
//
//  In order to read a specific tag, the string of the entry can be used
//  for quering the MetaDataDictionary.
//
//  Software Guide : EndLatex 


    std::string entryId = "0010|0010";
    itk::MetaDataObjectBase::ConstPointer  entry = dictionary[entryId];

    MetaDataStringType::ConstPointer entryvalue = 
      dynamic_cast<const MetaDataStringType *>( entry.GetPointer() ) ;

    if( entryvalue )
      {
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << "Patient's Name (" << entryId <<  ") is: " << tagvalue << std::endl;
      }


  return EXIT_SUCCESS;

}

