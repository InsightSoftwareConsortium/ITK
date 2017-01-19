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

#include "ClientTestLibraryA.h"
#include "ClientTestLibraryB.h"
#include "ClientTestLibraryC.h"

#include "itkConfigure.h"


#include "itkDynamicLoader.h"

typedef itk::Object * ( *PRODUCER_FUNCTION )();
typedef int ( *DYNAMIC_DOWNCAST_FUNCTION )( const char * type, const char * instanceSource, itk::Object const * base );

int itkDownCastTest( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " <LibraryBFilePath>" << std::endl;
    return EXIT_SUCCESS;
    }

  int dynamic_castFailures = 0;

#ifdef ITK_DYNAMIC_LOADING

  const itk::LibHandle libraryB = itk::DynamicLoader::OpenLibrary( argv[1] );
  if( !libraryB )
    {
    std::cerr << "Could not load LibraryB!" << std::endl;
    return EXIT_FAILURE;
    }


  LibraryA::ITKObjectProducer libraryAProducer;
  LibraryC::ITKObjectProducer libraryCProducer;


  DYNAMIC_DOWNCAST_FUNCTION libraryBdynamic_castDownCastEquivalencyTable = ( DYNAMIC_DOWNCAST_FUNCTION ) itk::DynamicLoader::GetSymbolAddress( libraryB, "dynamic_castDownCastEquivalencyTable" );
  if( !libraryBdynamic_castDownCastEquivalencyTable )
    {
    std::cerr << "Could not get dynamic_castDownCastEquivalencyTable function symbol." << std::endl;
    return EXIT_FAILURE;
    }
  DYNAMIC_DOWNCAST_FUNCTION libraryBdynamic_castDownCastImage = ( DYNAMIC_DOWNCAST_FUNCTION ) itk::DynamicLoader::GetSymbolAddress( libraryB, "dynamic_castDownCastImage" );
  if( !libraryBdynamic_castDownCastImage )
    {
    std::cerr << "Could not get dynamic_castDownCastImage function symbol." << std::endl;
    return EXIT_FAILURE;
    }


  itk::Object const * equivalencyTableA = libraryAProducer.EquivalencyTable();
  dynamic_castFailures += LibraryA::dynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library A ", equivalencyTableA );
  dynamic_castFailures += libraryBdynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library A ", equivalencyTableA );
  dynamic_castFailures += LibraryC::dynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library A ", equivalencyTableA );

  PRODUCER_FUNCTION equivalencyTableFunction = ( PRODUCER_FUNCTION ) itk::DynamicLoader::GetSymbolAddress( libraryB, "EquivalencyTable" );
  if( !equivalencyTableFunction )
    {
    std::cerr << "Could not get the EquivalencyTable function symbol." << std::endl;
    return EXIT_FAILURE;
    }
  itk::Object const * equivalencyTableB = ( *equivalencyTableFunction )();
  dynamic_castFailures += LibraryA::dynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library B ", equivalencyTableB );
  dynamic_castFailures += libraryBdynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library B ", equivalencyTableB );
  dynamic_castFailures += LibraryC::dynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library B ", equivalencyTableB );

  itk::Object const * equivalencyTableC = libraryCProducer.EquivalencyTable();
  dynamic_castFailures += LibraryA::dynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library C ", equivalencyTableC );
  dynamic_castFailures += libraryBdynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library C ", equivalencyTableC );
  dynamic_castFailures += LibraryC::dynamic_castDownCastEquivalencyTable( "EquivalencyTable", "library C ", equivalencyTableC );

  itk::Object const * imageA = libraryAProducer.Image();
  dynamic_castFailures += LibraryA::dynamic_castDownCastImage( "Image           ", "library A ", imageA );
  dynamic_castFailures += libraryBdynamic_castDownCastImage( "Image           ", "library A ", imageA );
  dynamic_castFailures += LibraryC::dynamic_castDownCastImage( "Image           ", "library A ", imageA );

  PRODUCER_FUNCTION imageFunction = ( PRODUCER_FUNCTION ) itk::DynamicLoader::GetSymbolAddress( libraryB, "Image" );
  if( !imageFunction )
    {
    std::cerr << "Could not get the Image function symbol." << std::endl;
    return EXIT_FAILURE;
    }
  itk::Object const * imageB = ( *imageFunction )();
  dynamic_castFailures += LibraryA::dynamic_castDownCastImage( "Image           ", "library B ", imageB );
  dynamic_castFailures += libraryBdynamic_castDownCastImage( "Image           ", "library B ", imageB );
  dynamic_castFailures += LibraryC::dynamic_castDownCastImage( "Image           ", "library B ", imageB );

  itk::Object const * imageC = libraryCProducer.Image();
  dynamic_castFailures += LibraryA::dynamic_castDownCastImage( "Image           ", "library C ", imageC );
  dynamic_castFailures += libraryBdynamic_castDownCastImage( "Image           ", "library C ", imageC );
  dynamic_castFailures += LibraryC::dynamic_castDownCastImage( "Image           ", "library C ", imageC );


  itk::DynamicLoader::CloseLibrary( libraryB );

#endif // ITK_DYNAMIC_LOADING
  return dynamic_castFailures;
}
