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

#include "itkDCMTKTransformIOFactory.h"
#include "itkTransformFileReader.h"

int
itkDCMTKTransformIOTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << argv[0] << " fixedSeriesDirectory movingSeriesDirectory transform" << std::endl;
    return EXIT_FAILURE;
  }
  const char * fixedSeriesDirectory = argv[1];
  const char * movingSeriesDirectory = argv[2];
  const char * transformFileName = argv[3];

  return EXIT_SUCCESS;
}
