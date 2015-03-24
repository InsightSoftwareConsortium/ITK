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
#ifndef itkChainCodePath_hxx
#define itkChainCodePath_hxx

#include "itkChainCodePath.h"
#include "itkNumericTraits.h"

namespace itk
{
template< unsigned int VDimension >
typename ChainCodePath< VDimension >::IndexType
ChainCodePath< VDimension >
::EvaluateToIndex(const InputType & input) const
{
  /* We could do something fancy here, such as "secretly" store the input and
   * total offset from the last time this function was called, and use such
   * information to speed up quasi-sequential access.  We would have to be
   * really, really careful about how we handle the "secret" member data,
   * though.

  **********************************
  * Begin broken, uncompiled Code. *
  **********************************

  int numberOfSteps = input - m_CurrentPosition;
  if( numberOfSteps > 0 )
    {
    const unsigned int steps = numberOfSteps;
    for(unsigned int i=0; i<steps; i++)
      {
      this->operator++();
      }
    }
  else
    {
    const unsigned int steps = -numberOfSteps;
    for(unsigned int i=0; i<steps; i++)
      {
      this->operator--();
      }
    }
  return m_CurrentIndex;

  ***********************************
  * End of broken, uncompiled Code. *
  **********************************/

  IndexType index = m_Start;

  // Iterate through the chaincode, summing the offsets as we go.
  for ( InputType i = 0; i < input; i++ )
    {
    index += m_Chain[i];
    }

  return index;
}

template< unsigned int VDimension >
typename ChainCodePath< VDimension >::OffsetType
ChainCodePath< VDimension >
::IncrementInput(InputType & input) const
{
  if ( input < NumberOfSteps() )
    {
    return m_Chain[input++];
    }
  else
    {
    return this->GetZeroOffset();
    }
}

/** Constructor */
template< unsigned int VDimension >
ChainCodePath< VDimension >
::ChainCodePath()
{
  m_Start = this->GetZeroIndex();
}

/** Standard "PrintSelf" method */
template< unsigned int VDimension >
void
ChainCodePath< VDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Start index:  " << m_Start << std::endl;
}
} // end namespace itk

#endif
