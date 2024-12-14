/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkImageIOBase.h"


// Specific ImageIO test

int
itkImageIOFileNameExtensionsTests(int, char *[])
{
  using IOBaseType = itk::ImageIOBase;
  using ArrayOfImageIOType = std::list<itk::LightObject::Pointer>;
  using ArrayOfExtensionsType = IOBaseType::ArrayOfExtensionsType;

  ArrayOfImageIOType allobjects = itk::ObjectFactoryBase::CreateAllInstance("itkImageIOBase");

  auto itr = allobjects.begin();

  while (itr != allobjects.end())
  {

    auto * io = dynamic_cast<IOBaseType *>(itr->GetPointer());

    if (!io)
    {
      std::cerr << "Got a null pointer in the array" << '\n';
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "---------------------------------" << '\n';
      std::cout << "ImageIO: " << io->GetNameOfClass() << '\n';

      const ArrayOfExtensionsType & readExtensions = io->GetSupportedReadExtensions();
      const ArrayOfExtensionsType & writeExtensions = io->GetSupportedWriteExtensions();

      auto readItr = readExtensions.begin();
      auto writeItr = writeExtensions.begin();

      std::cout << "Supported Read Extensions" << '\n';
      while (readItr != readExtensions.end())
      {
        std::cout << *readItr << '\n';
        ++readItr;
      }

      std::cout << "Supported Write Extensions" << '\n';
      while (writeItr != writeExtensions.end())
      {
        std::cout << *writeItr << '\n';
        ++writeItr;
      }
    }
    ++itr;
  }

  return EXIT_SUCCESS;
}
