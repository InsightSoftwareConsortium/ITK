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
#include "itkChainCodePath2D.h"


namespace itk
{
ChainCodePath2D::OutputType
ChainCodePath2D
::Evaluate(const InputType & input) const
{
  return DecodeOffset(m_Chain2D[input]);
}

ChainCodePath2D::IndexType
ChainCodePath2D
::EvaluateToIndex(const InputType & input) const
{
  IndexType index = GetStart();

  // Iterate through the chaincode, summing the offsets as we go.
  for ( InputType i = 0; i < input; i++ )
    {
    index += DecodeOffset(m_Chain2D[i]);
    }

  return index;
}

ChainCodePath2D::OffsetType
ChainCodePath2D
::IncrementInput(InputType & input) const
{
  if ( input < NumberOfSteps() )
    {
    return DecodeOffset(m_Chain2D[input++]);
    }
  else
    {
    return this->GetZeroOffset();
    }
}

std::string
ChainCodePath2D
::GetChainCodeAsString(void) const
{
  std::string printableChain;

  for ( unsigned int i = 0; i < m_Chain2D.size(); i++ )
    {
    // Make a single char string out of the current step
    std::ostringstream printableStep;
    printableStep << m_Chain2D[i];

    // Append the new step (in string form) to the main string of steps
    printableChain.insert( i, printableStep.str() );
    }

  return printableChain;
}

/** Constructor */
ChainCodePath2D
::ChainCodePath2D()
{
  // Most of the work is done in the parent constructor.

  OffsetType offset;

  // Initialize the lookup tables m_FreemanCode and m_ReverseFreemanCode:
  offset[0] = 0;
  offset[1] = 0;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 0;
  m_ReverseFreemanCode[0] = offset;

  offset[0] = 0;
  offset[1] = 1;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 1;
  m_ReverseFreemanCode[1] = offset;

  offset[0] = 1;
  offset[1] = 1;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 2;
  m_ReverseFreemanCode[2] = offset;

  offset[0] = 1;
  offset[1] = 0;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 3;
  m_ReverseFreemanCode[3] = offset;

  offset[0] = 1;
  offset[1] = -1;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 4;
  m_ReverseFreemanCode[4] = offset;

  offset[0] = 0;
  offset[1] = -1;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 5;
  m_ReverseFreemanCode[5] = offset;

  offset[0] = -1;
  offset[1] = -1;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 6;
  m_ReverseFreemanCode[6] = offset;

  offset[0] = -1;
  offset[1] = 0;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 7;
  m_ReverseFreemanCode[7] = offset;

  offset[0] = -1;
  offset[1] = 1;
  m_FreemanCode[offset[0] + 1][offset[1] + 1] = 8;
  m_ReverseFreemanCode[8] = offset;
}

ChainCodePath2D::~ChainCodePath2D()
{}

/** Standard "PrintSelf" method */
void
ChainCodePath2D
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Chain code 2D:  " << GetChainCodeAsString() << std::endl;
}
} // end namespace itk
