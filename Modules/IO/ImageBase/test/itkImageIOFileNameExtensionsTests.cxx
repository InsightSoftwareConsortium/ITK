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

#include "itkImageIOBase.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkImageIOFileNameExtensionsTests( int , char * [] )
{
  typedef itk::ImageIOBase                        IOBaseType;
  typedef std::list<itk::LightObject::Pointer>    ArrayOfImageIOType;
  typedef IOBaseType::ArrayOfExtensionsType       ArrayOfExtensionsType;

  ArrayOfImageIOType allobjects = itk::ObjectFactoryBase::CreateAllInstance("itkImageIOBase");

  ArrayOfImageIOType::iterator itr = allobjects.begin();

  while( itr != allobjects.end() )
    {

    IOBaseType * io = dynamic_cast< IOBaseType * >( itr->GetPointer() );

    if( ! io )
      {
      std::cerr << "Got a null pointer in the array" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "---------------------------------" << std::endl;
      std::cout << "ImageIO: " << io->GetNameOfClass() << std::endl;

      const ArrayOfExtensionsType & readExtensions  = io->GetSupportedReadExtensions();
      const ArrayOfExtensionsType & writeExtensions = io->GetSupportedWriteExtensions();

      ArrayOfExtensionsType::const_iterator readItr  = readExtensions.begin();
      ArrayOfExtensionsType::const_iterator writeItr = writeExtensions.begin();

      std::cout << "Supported Read Extensions" << std::endl;
      while( readItr != readExtensions.end() )
        {
        std::cout << *readItr << std::endl;
        ++readItr;
        }

      std::cout << "Supported Write Extensions" << std::endl;
      while( writeItr != writeExtensions.end() )
        {
        std::cout << *writeItr << std::endl;
        ++writeItr;
        }

      }
    ++itr;
    }

  return EXIT_SUCCESS;
}
