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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMetaDataObject.h"
#include "itkTestingMacros.h"


int itkTIFFImageIOInfoTest( int argc, char * argv[] )
{

  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " input" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int                          Dimension = 2;
  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  reader->Update();

  typedef itk::MetaDataDictionary            DictionaryType;
  typedef itk::MetaDataObject< std::string > MetaDataStringType;

  const DictionaryType & dictionary = reader->GetImageIO()->GetMetaDataDictionary();
  DictionaryType::ConstIterator itr = dictionary.Begin();
  DictionaryType::ConstIterator end = dictionary.End();

  std::cout << "MetaDataDictionary" << std::endl;
  while( itr != end )
    {
    itk::MetaDataObjectBase::Pointer  entry   = itr->second;
    const std::string                 tagkey  = itr->first;

    MetaDataStringType::Pointer entryvalue =
      dynamic_cast<MetaDataStringType *>( entry.GetPointer() );

    if( entryvalue )
      {
      std::cout << tagkey << ": " << entryvalue->GetMetaDataObjectValue() << std::endl;
      }
    else
      {
      std::cout << tagkey << ": " << entry << std::endl;
      }

    ++itr;
    }

  return EXIT_SUCCESS;
}
