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
#ifndef itkPointSetFunction_hxx
#define itkPointSetFunction_hxx

#include "itkPointSetFunction.h"

namespace itk
{

/**
 * Constructor
 */
template <typename TInputPointSet, typename TOutput, typename TCoordRep>
PointSetFunction<TInputPointSet, TOutput, TCoordRep>
::PointSetFunction()
{
  this->m_PointSet = ITK_NULLPTR;
}

/**
 * Destructor
 */
template <typename TInputPointSet, typename TOutput, typename TCoordRep>
PointSetFunction<TInputPointSet, TOutput, TCoordRep>
::~PointSetFunction()
{
}

/**
 * Standard "PrintSelf" method
 */
template <typename TInputPointSet, typename TOutput, typename TCoordRep>
void
PointSetFunction<TInputPointSet, TOutput, TCoordRep>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "InputPointSet: " << m_PointSet.GetPointer() << std::endl;
}

/**
 * Initialize by setting the input point set
 */
template <typename TInputPointSet, typename TOutput, typename TCoordRep>
void
PointSetFunction<TInputPointSet, TOutput, TCoordRep>
::SetInputPointSet( const InputPointSetType * ptr )
{
  this->m_PointSet = ptr;
}

} // end namespace itk

#endif
