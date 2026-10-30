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

#include "itkImportImageContainer.h"
#include "itkObjectFactoryBase.h"
#include "itksys/SystemTools.hxx"

#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <string>

// Usage: itkObjectFactoryAutoloadPathTest <env|api|envdup> <FactoryLibraryDirectory>
//
// env: ITK_AUTOLOAD_PATH is set by the test harness before the process starts;
//      nothing is configured in main().
// api: ITK_AUTOLOAD_PATH is removed from the environment, then the autoload
//      paths are set with ObjectFactoryBase::SetAutoloadPaths() before any
//      factory is used.
// envdup: like env, but ITK_AUTOLOAD_PATH has a bogus entry followed by
//         consecutive separators before the real directory; regression test
//         for consecutive separators hiding the entry that follows them.
//
// In both cases the factory library must be loaded lazily on first use.
int
main(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " <env|api|envdup> <FactoryLibraryDirectory>" << std::endl;
    return EXIT_FAILURE;
  }
  const std::string            mode{ argv[1] };
  const std::string            factoryDirectory{ argv[2] };
  const std::list<std::string> expectedPaths{ factoryDirectory };
  bool                         checkExactPaths = true;

  if (mode == "api")
  {
    // Ensure the setter, not an inherited environment, provides the paths.
    itksys::SystemTools::UnPutEnv("ITK_AUTOLOAD_PATH");
    itk::ObjectFactoryBase::SetAutoloadPaths(expectedPaths);
  }
  else if (mode == "envdup")
  {
    // ITK_AUTOLOAD_PATH also contains a bogus entry before the real one.
    checkExactPaths = false;
  }
  else if (mode != "env")
  {
    std::cerr << "Unknown mode: " << mode << std::endl;
    return EXIT_FAILURE;
  }

  if (checkExactPaths && itk::ObjectFactoryBase::GetAutoloadPaths() != expectedPaths)
  {
    std::cerr << "Autoload paths do not contain exactly \"" << factoryDirectory << '"' << std::endl;
    return EXIT_FAILURE;
  }

  // Objects created by the dynamic factory must be released before ReHash() unloads its library.
  {
    const auto container = itk::ImportImageContainer<unsigned long, short>::New();
    if (std::strcmp(container->GetNameOfClass(), "TestImportImageContainer") != 0)
    {
      std::cerr << "Dynamic factory was not loaded; created class: " << container->GetNameOfClass() << std::endl;
      return EXIT_FAILURE;
    }
  }

  // An empty list of paths must not load the factory after a rehash.
  itk::ObjectFactoryBase::SetAutoloadPaths({});
  itk::ObjectFactoryBase::ReHash();
  if (std::strcmp(itk::ImportImageContainer<unsigned long, short>::New()->GetNameOfClass(),
                  "TestImportImageContainer") == 0)
  {
    std::cerr << "Dynamic factory was loaded with an empty autoload path list." << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
