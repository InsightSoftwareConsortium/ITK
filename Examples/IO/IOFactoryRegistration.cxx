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

#include "itkImageFileReader.h"

// This example is a convenience program that lists all of the registered
// Image IO factories in the ITK build. If an ImageIO module is enabled, the
// corresponding IO factory will be automatically registered.
int
main()
{
  const std::list<itk::ObjectFactoryBase *> & factories =
    itk::ObjectFactoryBase::GetRegisteredFactories();
  const size_t numFactories = factories.size();

  std::cout << numFactories << " Image IO factories registered:" << std::endl;

  if (!factories.empty())
  {
    for (const auto & factory : factories)
    {
      std::istringstream iss(factory->GetDescription());
      std::string        IOType;
      iss >> IOType; // the first word of the description
      std::cout << IOType << " ";
    }
    std::cout << std::endl;
  }
  else
  {
    std::cout << "Failed to load any factories" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
