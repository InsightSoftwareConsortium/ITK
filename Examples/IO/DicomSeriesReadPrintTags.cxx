/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomSeriesReadPrintTags.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a DICOM series into a volume
//  and then save this volume in another file format.
//
//  Software Guide : EndLatex 


#include "itkImageSeriesReader.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"

int main( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomDirectory " << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;

  typedef itk::GDCMImageIO                        ImageIOType;
    
  ImageIOType::Pointer dicomIO = ImageIOType::New();

  // Get the DICOM filenames from the directory
  typedef itk::GDCMSeriesFileNames NamesGeneratorType;
  NamesGeneratorType::Pointer nameGenerator = NamesGeneratorType::New();
  nameGenerator->SetInputDirectory( argv[1] );
  
  typedef std::vector<std::string> fileNamesContainer;
  fileNamesContainer fileNames;
  fileNames = nameGenerator->GetInputFileNames();
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileNames( fileNames );
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

  while( itr != end )
    {
    itk::MetaDataObjectBase::Pointer  entry = itr->second;

    MetaDataStringType::Pointer entryvalue = dynamic_cast<MetaDataStringType *>( entry.GetPointer() ) ;

    if( entryvalue )
      {
      std::string tagkey   = itr->first;
      std::string tagvalue = entryvalue->GetMetaDataObjectValue();
      std::cout << tagkey <<  " = " << tagvalue << std::endl;
      }
    ++itr;
    }


  return EXIT_SUCCESS;

}

