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
#include <iostream>
#include <fstream>
#include "CoherenceEnhancingDiffusionCommandLine.h"

int CoherenceEnhancingDiffusionTest( int argc, char * argv[] )
{
  try
    {
    CoherenceEnhancingDiffusionCommandLine::Execute(argc, argv);
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "ITK Exception : " << e.GetDescription() << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

/*
int LinearAnisotropicDiffusionTest(int argc, char **argv)
{
    try {
        LinearAnisotropicDiffusionCommandLine::Execute(argc, argv);
    } catch (itk::ExceptionObject& e) {
        std::cerr << "ITK Exception : " << e.GetDescription() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}*/
