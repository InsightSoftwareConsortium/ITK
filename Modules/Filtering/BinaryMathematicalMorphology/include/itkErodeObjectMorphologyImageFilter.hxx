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
#ifndef itkErodeObjectMorphologyImageFilter_hxx
#define itkErodeObjectMorphologyImageFilter_hxx

#include "itkErodeObjectMorphologyImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
ErodeObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::ErodeObjectMorphologyImageFilter()
{
  m_BackgroundValue = NumericTraits< PixelType >::ZeroValue();

  m_ErodeBoundaryCondition.SetConstant( NumericTraits< PixelType >::max() );
  this->OverrideBoundaryCondition(&m_ErodeBoundaryCondition);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ErodeObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::Evaluate(OutputNeighborhoodIteratorType & nit,
           const KernelType & kernel)
{
  unsigned int             i;
  KernelIteratorType       kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  bool valid = true;

  for ( i = 0, kernel_it = kernel.Begin(); kernel_it < kernelEnd; ++kernel_it, ++i )
    {
    if ( *kernel_it )
      {
      nit.SetPixel(i, m_BackgroundValue, valid);
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
ErodeObjectMorphologyImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "BackgroundValue : " << m_BackgroundValue << std::endl;
}
} // end namespace itk
#endif
