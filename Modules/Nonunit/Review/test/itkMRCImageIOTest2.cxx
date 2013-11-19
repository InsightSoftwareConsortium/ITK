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
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkMRCImageIO.h"
#include "itkMRCImageIOFactory.h"
#include "itkTestingMacros.h"
#include "itkVectorImage.h"
#include "itkMetaDataObject.h"
#include "itkTestingHashImageFilter.h"

namespace {

template <typename TImageType>
bool Test( const std::string &inFileName, const std::string &outFileName, const std::string &md5 )
{
  typedef TImageType                      ImageType;
  typedef itk::ImageFileReader<ImageType> ImageFileReaderType;

  typename ImageFileReaderType::Pointer reader = ImageFileReaderType::New();
  reader->SetFileName( inFileName );
  reader->UpdateLargestPossibleRegion();

  typename ImageType::Pointer image = reader->GetOutput();

  typedef itk::MetaDataDictionary            DictionaryType;
  typedef itk::MetaDataObject< std::string > MetaDataStringType;

  // prepare to iterate over the dictionary
  DictionaryType &dic= image->GetMetaDataDictionary();

  DictionaryType::ConstIterator itr = dic.Begin();
  DictionaryType::ConstIterator end = dic.End();

  while( itr != end )
    {
    std::string key  = itr->first;

    // check to see if we have a MRC Header
    if ( itr->first == itk::MRCImageIO::m_MetaDataHeaderName)
      {
      itk::MRCHeaderObject::ConstPointer header;
      if (itk::ExposeMetaData(dic, itk::MRCImageIO::m_MetaDataHeaderName, header) )
        {
        std::cout << "MRC Header: " << std::endl;
        std::cout << header;

        // Use DeepCopy method to improve coverage
        itk::MRCHeaderObject::Pointer copyHeader = itk::MRCHeaderObject::New();
        copyHeader->DeepCopy( header );

        }

      }
    else
      {
      // just print the strings now
      itk::MetaDataObjectBase::Pointer entry = itr->second;

      MetaDataStringType::Pointer entryvalue =  dynamic_cast<MetaDataStringType *>( entry.GetPointer() );
      if ( entryvalue )
        {

        std::string tagvalue = entryvalue->GetMetaDataObjectValue();

        std::cout << "(" << key <<  ") ";
        std::cout << " = " << tagvalue << std::endl;

        }
      }
    ++itr;
    }

  typedef itk::Testing::HashImageFilter<ImageType> HashFilter;
  typename HashFilter::Pointer hasher = HashFilter::New();
  hasher->SetInput( image );
  hasher->Update();

  TEST_EXPECT_EQUAL( md5, hasher->GetHash() );

  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput( image );
  writer->SetFileName( outFileName );
  writer->Update();

  return EXIT_SUCCESS;

}

}

int itkMRCImageIOTest2( int argc, char *argv[] )
{

  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0] << " inputFileName outputFilename md5hash" << std::endl;
    return EXIT_FAILURE;
    }

  itk::MRCImageIOFactory::RegisterOneFactory();

  const std::string inputFileName = argv[1];
  const std::string outputFileName = argv[2];
  const std::string md5hash = argv[3];

  return Test<itk::VectorImage< unsigned char, 3> >( inputFileName, outputFileName, md5hash );

}
