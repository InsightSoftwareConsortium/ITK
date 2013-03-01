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

#include "itkArchetypeSeriesFileNames.h"

int itkArchetypeSeriesFileNamesTest(int argc, char* argv[])
{

  if(argc < 2)
    {
    std::cerr << "Usage: " << argv[0]
              << "One or more filenames (with directory)";
    return EXIT_FAILURE;
    }


  std::cout << "Number of arguments: " << argc << std::endl;

  for (int i=1; i < argc; i++)
    {
    std::cout << "Testing argument " << i << std::endl;
    std::cout << "Archetype name: " << argv[i] << std::endl;

    itk::ArchetypeSeriesFileNames::Pointer fit = itk::ArchetypeSeriesFileNames::New();
    fit->SetArchetype ( argv[i] );

    std::vector<std::string> names = fit->GetFileNames();
    std::vector<std::string>::iterator nit;

    std::cout << "List of returned filenames: " << std::endl;
    for (nit = names.begin();
         nit != names.end();
         ++nit)
      {
      std::cout << "File: " << (*nit).c_str() << std::endl;
      }

    std::cout << fit;

    }

  return EXIT_SUCCESS;

}
