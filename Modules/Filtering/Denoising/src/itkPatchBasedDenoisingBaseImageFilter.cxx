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
  const char * s = nullptr;
  switch (value)
  {
    case NoiseType::NOMODEL:
      s = "NoiseType::NOMODEL";
      break;
    case NoiseType::GAUSSIAN:
      s = "NoiseType::GAUSSIAN";
      break;
    case NoiseType::RICIAN:
      s = "NoiseType::RICIAN";
      break;
    case NoiseType::POISSON:
      s = "NoiseType::POISSON";
      break;
    default:
      s = "INVALID VALUE FOR NoiseType";
  }
  return out << s;
}

std::ostream &
operator<<(std::ostream & out, const SpaceType value)
{
  const char * s = nullptr;
  switch (value)
  {
    case SpaceType::EUCLIDEAN:
      s = "SpaceType::EUCLIDEAN";
      break;
    case SpaceType::RIEMANNIAN:
      s = "SpaceType::RIEMANNIAN";
      break;
    default:
      s = "INVALID VALUE FOR SpaceType";
  }
  return out << s;
}
std::ostream &
operator<<(std::ostream & out, const StateTypeOfFilter value)
{
  const char * s = nullptr;
  switch (value)
  {
    case StateTypeOfFilter::UNINITIALIZED:
      s = "StateTypeOfFilter::UNINITIALIZED";
      break;
    case StateTypeOfFilter::INITIALIZED:
      s = "StateTypeOfFilter::INITIALIZED";
      break;
    default:
      s = "INVALID VALUE FOR StateTypeOfFilter";
  }
  return out << s;
}
} // end namespace itk
