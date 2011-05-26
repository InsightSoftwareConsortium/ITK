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
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk
{
namespace Statistics
{
MersenneTwisterRandomVariateGenerator::Pointer MersenneTwisterRandomVariateGenerator:: m_Instance = 0;

/**
     * This just calls GetInstance
     */
MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator::New()
{
  return GetInstance();
}

/**
 * Return the single instance of the MersenneTwisterRandomVariateGenerator
 */
MersenneTwisterRandomVariateGenerator::Pointer
MersenneTwisterRandomVariateGenerator
::GetInstance()
{
  if ( !MersenneTwisterRandomVariateGenerator::m_Instance )
    {
    // Try the factory first
    MersenneTwisterRandomVariateGenerator::m_Instance  = ObjectFactory< Self >::Create();
    // if the factory did not provide one, then create it here
    if ( !MersenneTwisterRandomVariateGenerator::m_Instance )
      {
      MersenneTwisterRandomVariateGenerator::m_Instance = new MersenneTwisterRandomVariateGenerator;
      // Remove extra reference from construction.
      MersenneTwisterRandomVariateGenerator::m_Instance->UnRegister();
      }
    }
  /**
   * return the instance
   */
  return MersenneTwisterRandomVariateGenerator::m_Instance;
}
}
}
