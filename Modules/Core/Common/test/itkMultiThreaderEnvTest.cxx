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

#include "itkMultiThreader.h"

int itkMultiThreaderEnvTest(int argc, char* argv[])
{
  // Choose a number of threads.
  if( argc != 2 )
    {
    std::cout << "ERROR: KNOWN VALUE REQUIRED" << std::endl;
    return EXIT_FAILURE;
    }
  const unsigned int requiredValue = static_cast<unsigned int>( atoi( argv[1] ) );

  itk::MultiThreader::Pointer    threader = itk::MultiThreader::New();
  if(threader.IsNull())
    {
    return EXIT_FAILURE;
    }
  if( itk::MultiThreader::GetGlobalDefaultNumberOfThreads() != requiredValue )
    {
    std::cout << "ERROR: Wrong number of maximum number of threads set from environment. "
      << requiredValue << " != " << itk::MultiThreader::GetGlobalDefaultNumberOfThreads() << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
