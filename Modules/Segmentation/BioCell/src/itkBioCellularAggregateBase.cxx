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
#include "itkBioCellularAggregateBase.h"

namespace itk
{
namespace bio
{
CellularAggregateBase
::CellularAggregateBase()
{}

CellularAggregateBase
::~CellularAggregateBase()
{}

/** The actual implementation is provided in the derived classes where the Cell
 * dimension is known. */
CellularAggregateBase::SubstrateValueType
CellularAggregateBase
::GetSubstrateValue( IdentifierType itkNotUsed(cellId), unsigned int itkNotUsed(substrateId) ) const
{
  return 0;
}

/** The actual implementation is provided in the derived classes where the Cell
 * dimension is known. */
void
CellularAggregateBase
::Add(CellBase *, CellBase *, double)
{}

/** The actual implementation is provided in the derived classes where the Cell
 * dimension is known. */
void
CellularAggregateBase
::Remove(CellBase *)
{}

void
CellularAggregateBase
::PrintSelf(std::ostream & os, itk::Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "Cellular Aggregate Base " << std::endl;
}
} // end namespace bio
} // end namespace itk
