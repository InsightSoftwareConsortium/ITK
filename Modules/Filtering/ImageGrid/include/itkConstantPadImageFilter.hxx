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
#ifndef itkConstantPadImageFilter_hxx
#define itkConstantPadImageFilter_hxx

#include "itkConstantPadImageFilter.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
ConstantPadImageFilter< TInputImage, TOutputImage >
::ConstantPadImageFilter()
{
  m_InternalBoundaryCondition.SetConstant( NumericTraits< OutputImagePixelType >::ZeroValue() );
  this->InternalSetBoundaryCondition( &m_InternalBoundaryCondition );
}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
void
ConstantPadImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Constant: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >
    ( m_InternalBoundaryCondition.GetConstant() ) << std::endl;
}

} // end namespace itk

#endif
