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

#include <fstream>
#include "itkMesh.h"

int itkMeshFstreamTest(int argc, char* argv[] )
{
  if (argc < 2)
    {
    std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
    return EXIT_FAILURE;
    }

  std::ofstream ofs;
  ofs.open(argv[1]);
  ofs << "Testing Mesh & fstream" << std::endl;
  ofs.close();

  return EXIT_SUCCESS;
}
