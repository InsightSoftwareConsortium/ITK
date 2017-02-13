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
#ifndef itkPathToChainCodePathFilter_hxx
#define itkPathToChainCodePathFilter_hxx

#include "itkPathToChainCodePathFilter.h"

namespace itk
{

template< typename TInputPath, typename TOutputChainCodePath >
PathToChainCodePathFilter< TInputPath, TOutputChainCodePath >
::PathToChainCodePathFilter() :
  m_MaximallyConnected( false )
{
  this->SetNumberOfRequiredInputs(1);
}

template< typename TInputPath, typename TOutputChainCodePath >
void
PathToChainCodePathFilter< TInputPath, TOutputChainCodePath >
::GenerateData(void)
{
  OffsetType offset;
  OffsetType tempOffset;
  OffsetType zeroOffset;

  zeroOffset.Fill(0);

  InputPathInputType inputPathInput;

  int dimension = OffsetType::GetOffsetDimension();

  typename Superclass::InputPathConstPointer inputPtr  = this->GetInput();
  typename Superclass::OutputPathPointer outputPtr = this->GetOutput(0);

  //outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  //outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  //outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  //outputPtr->Allocate();  // Allocate() is an Image function

  outputPtr->Clear();
  inputPathInput = inputPtr->StartOfInput();
  outputPtr->SetStart( inputPtr->EvaluateToIndex(inputPathInput) );

  for ( OutputPathInputType outputPathInput = 0;; )
    {
    offset  = inputPtr->IncrementInput(inputPathInput);
    if ( zeroOffset == offset ) { break; }

    if ( !m_MaximallyConnected )
      {
      outputPtr->InsertStep(outputPathInput++, offset);
      }
    else
      {
      for ( int d = 0; d < dimension; d++ )
        {
        if ( offset[d] )
          {
          tempOffset.Fill(0);
          tempOffset[d] = offset[d];
          outputPtr->InsertStep(outputPathInput++, tempOffset);
          }
        }
      }
    }
}

template< typename TInputPath, typename TOutputChainCodePath >
void
PathToChainCodePathFilter< TInputPath, TOutputChainCodePath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "MaximallyConnected: " << m_MaximallyConnected << std::endl;
}
} // end namespace itk

#endif
