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

#include "itkMetaDataObject.h"
#include "itkImage.h"
#include "itkMath.h"

template< typename TMetaData >
int
testMetaData( const TMetaData & value )
{
  typedef TMetaData MetaDataType;

  typedef itk::MetaDataObject< MetaDataType > MetaDataObjectType;

  typename MetaDataObjectType::Pointer metaDataObject = MetaDataObjectType::New();

  metaDataObject->SetMetaDataObjectValue( value );
  if( itk::Math::NotExactlyEquals(metaDataObject->GetMetaDataObjectValue(), value) )
    {
    std::cerr << "Set value does not equal original value!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "The metadata's type name is: " << metaDataObject->GetMetaDataObjectTypeName() << std::endl;
  std::cout << "The metadata object: " << std::endl;
  metaDataObject->Print( std::cout );

  std::cout << std::endl << std::endl;

  return EXIT_SUCCESS;
}

int itkMetaDataObjectTest( int , char * [] )
{

  int result = EXIT_SUCCESS;
  result += testMetaData< unsigned char >( 24 );
  result += testMetaData< char >( -24 );
  result += testMetaData< unsigned short >( 24 );
  result += testMetaData< short >( -24 );
  result += testMetaData< unsigned int >( 24 );
  result += testMetaData< int >( -24 );
  result += testMetaData< unsigned long >( 24 );
  result += testMetaData< long >( -24 );
  result += testMetaData< unsigned long long >( 24 );
  result += testMetaData< long long >( -24 );
  result += testMetaData< float >( -24 );
  result += testMetaData< double >( -24 );
  result += testMetaData< std::string >( "I T K" );

  typedef itk::Image< unsigned short, 3 > ImageType;
  ImageType::Pointer image = ITK_NULLPTR;
  result += testMetaData< ImageType::Pointer >( image );

  return result;
}
