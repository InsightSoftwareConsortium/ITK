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
#include "itkRegistrationParameterScalesEstimator.h"

namespace itk
{
/** Define how to print enumerations */
std::ostream &
operator<<( std::ostream & out, const StrategyTypeForSampling value )
{
  const char * s = nullptr;
  switch ( value )
  {
    case StrategyTypeForSampling::FullDomainSampling:
      s = "StrategyTypeForSampling::FullDomainSampling";
      break;
    case StrategyTypeForSampling::CornerSampling:
      s = "StrategyTypeForSampling::CornerSampling";
      break;
    case StrategyTypeForSampling::RandomSampling:
      s = "StrategyTypeForSampling::RandomSampling";
      break;
    case StrategyTypeForSampling::CentralRegionSampling:
      s = "StrategyTypeForSampling::CentralRegionSampling";
      break;
    case StrategyTypeForSampling::VirtualDomainPointSetSampling:
      s = "StrategyTypeForSampling::VirtualDomainPointSetSampling";
      break;
    default:
      s = "INVALID VALUE FOR StrategyTypeForSampling";
  }
  return out << s;
}
} // namespace itk
