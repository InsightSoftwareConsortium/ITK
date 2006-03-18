/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodePath.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkChainCodePath_txx
#define __itkChainCodePath_txx

#include "itkChainCodePath.h"
#include "itkNumericTraits.h"

namespace itk
{

template <unsigned int VDimension>
typename ChainCodePath<VDimension>::IndexType
ChainCodePath<VDimension>
::EvaluateToIndex( const InputType & input ) const
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
  for(InputType i=0; i<input; i++)
    {
    index += m_Chain[i];
    }

  return index;
}

template <unsigned int VDimension>
typename ChainCodePath<VDimension>::OffsetType
ChainCodePath<VDimension>
::IncrementInput(InputType & input ) const
{
  if( input < NumberOfSteps() )
    {
    return m_Chain[input++];
    }
  else
    {
    return this->GetZeroOffset();
    }
}

/** Constructor */
template <unsigned int VDimension>
ChainCodePath<VDimension>
::ChainCodePath()
{
  m_Start = this->GetZeroIndex();
}

/** Standard "PrintSelf" method */
template <unsigned int VDimension>
void
ChainCodePath<VDimension>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Start index:  " << m_Start << std::endl;
}

} // end namespace itk

#endif
