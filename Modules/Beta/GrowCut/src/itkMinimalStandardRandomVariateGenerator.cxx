/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkMinimalStandardRandomVariateGenerator.h"

namespace itk
{
namespace Statistics
{

MinimalStandardRandomVariateGenerator ::MinimalStandardRandomVariateGenerator()
{
  this->m_NormalGenerator = NormalGeneratorType::New();
  this->Initialize(1);
}

void
MinimalStandardRandomVariateGenerator ::Initialize(int randomSeed)
{
  this->m_NormalGenerator->Initialize(randomSeed);
}


double
MinimalStandardRandomVariateGenerator ::GetVariate()
{
  return this->m_NormalGenerator->GetVariate();
}


void
MinimalStandardRandomVariateGenerator ::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace Statistics
} // end namespace itk
