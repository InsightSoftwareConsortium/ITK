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
#include "itkSingleton.h"
#include "itkThreadPool.h"
#include "itksys/DynamicLoader.hxx"
#include <iostream>

// Two dynamically loaded libraries, each with its own statically linked copy of ITKCommon,
// sharing one SingletonIndex, must observe one and the same ThreadPool.
int
itkThreadPoolSingletonSharingTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " <libraryA> <libraryB>" << std::endl;
    return EXIT_FAILURE;
  }

  using AdoptFunction = void (*)(itk::SingletonIndex *);
  using GetPoolFunction = void * (*)();

  void * pools[2] = { nullptr, nullptr };

  for (int i = 0; i < 2; ++i)
  {
    const char * const libraryName = argv[i + 1];

    itksys::DynamicLoader::LibraryHandle handle = itksys::DynamicLoader::OpenLibrary(libraryName);
    if (handle == nullptr)
    {
      std::cerr << "Failed to load " << libraryName << ": " << itksys::DynamicLoader::LastError() << std::endl;
      return EXIT_FAILURE;
    }

    const std::string suffix = (i == 0) ? "A" : "B";
    const auto        adopt = reinterpret_cast<AdoptFunction>(
      itksys::DynamicLoader::GetSymbolAddress(handle, "ThreadPoolSingletonLibrary" + suffix + "_AdoptSingletonIndex"));
    const auto getPool = reinterpret_cast<GetPoolFunction>(
      itksys::DynamicLoader::GetSymbolAddress(handle, "ThreadPoolSingletonLibrary" + suffix + "_GetThreadPool"));

    if (adopt == nullptr || getPool == nullptr)
    {
      std::cerr << "Failed to resolve symbols in " << libraryName << std::endl;
      return EXIT_FAILURE;
    }

    adopt(itk::SingletonIndex::GetInstance());
    pools[i] = getPool();
    std::cout << "Library " << suffix << " ThreadPool: " << pools[i] << std::endl;
  }

  if (pools[0] != pools[1])
  {
    std::cerr << "FAILED: each library created its own ThreadPool. The singleton guard must live in "
                 "ThreadPoolGlobals, which is shared through the SingletonIndex, not in per-binary storage."
              << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
