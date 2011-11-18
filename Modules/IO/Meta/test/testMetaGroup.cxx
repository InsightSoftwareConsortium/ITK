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
#include <cstdlib>

#include <metaGroup.h>
#include "itksys/SystemTools.hxx"

int testMetaGroup(int argc, char *argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  // Testing metaGroup
  std::cout << "--- Testing metaGroup ---" << std::endl;

  std::cout << "Testing Writing:";
  MetaGroup tGroup;
  tGroup.Write("group.meta");
  std::cout << " [PASSED] " << std::endl;

  std::cout << "Testing Reading:";
  MetaGroup groupLoad("group.meta");
  std::cout << " [PASSED] " << std::endl;

  groupLoad.PrintInfo();

  std::cout << "Testing Copy:";
  MetaGroup groupCopy(&groupLoad);
  std::cout << " [PASSED] " << std::endl;

  groupCopy.PrintInfo();

  std::cout << "[DONE]" << std::endl;
  return EXIT_SUCCESS;
}
