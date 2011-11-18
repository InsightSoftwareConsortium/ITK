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

#include "itkFEMLightObject.h"

namespace itk
{
namespace fem
{
/**
 * Here we just read the global number from the stream.
 * This should be the first function called when reading object data.
 */

void FEMLightObject::SetGlobalNumber(int gn)
{
  this->m_GlobalNumber = gn;
}

int FEMLightObject::GetGlobalNumber() const
{
  return this->m_GlobalNumber;
}

void FEMLightObject::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Global Number: " << this->m_GlobalNumber << std::endl;
}

}
}  // end namespace itk::fem
