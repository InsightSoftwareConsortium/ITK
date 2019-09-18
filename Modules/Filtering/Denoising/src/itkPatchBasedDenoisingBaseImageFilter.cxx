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
#include "itkPatchBasedDenoisingBaseImageFilter.h"

namespace itk
{
/** Printing of enumeration values */
std::ostream &
operator<<(std::ostream & out, const NoiseType value)
{
  return out << [value] {
    switch (value)
    {
      case NoiseType::NOMODEL:
        return "NoiseType::NOMODEL";
      case NoiseType::GAUSSIAN:
        return "NoiseType::GAUSSIAN";
      case NoiseType::RICIAN:
        return "NoiseType::RICIAN";
      case NoiseType::POISSON:
        return "NoiseType::POISSON";
      default:
        return "INVALID VALUE FOR NoiseType";
    }
  }();
}

std::ostream &
operator<<(std::ostream & out, const SpaceType value)
{
  return out << [value] {
    switch (value)
    {
      case SpaceType::EUCLIDEAN:
        return "SpaceType::EUCLIDEAN";
      case SpaceType::RIEMANNIAN:
        return "SpaceType::RIEMANNIAN";
      default:
        return "INVALID VALUE FOR SpaceType";
    }
  }();
}
std::ostream &
operator<<(std::ostream & out, const StateTypeOfFilter value)
{
  return out << [value] {
    switch (value)
    {
      case StateTypeOfFilter::UNINITIALIZED:
        return "StateTypeOfFilter::UNINITIALIZED";
      case StateTypeOfFilter::INITIALIZED:
        return "StateTypeOfFilter::INITIALIZED";
      default:
        return "INVALID VALUE FOR StateTypeOfFilter";
    }
  }();
}
} // end namespace itk
