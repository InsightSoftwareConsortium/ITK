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
#ifndef itkKernelImageFilter_hxx
#define itkKernelImageFilter_hxx

#include "itkKernelImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::KernelImageFilter()
{
  this->SetRadius(1UL);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::SetRadius(const RadiusType & radius)
{
  // SetKernel() must be called, because it can be overloaded in a subclass
  // - MovingHistogramImageFilterBase for example.
  KernelType kernel;
  // the right version of the MakeKernel method should be called there
  this->MakeKernel( radius, kernel );
  this->SetKernel( kernel );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::SetKernel(const KernelType & kernel)
{
  if ( m_Kernel != kernel )
    {
    m_Kernel = kernel;
    this->Modified();
    }
  // set the radius of the super class to be the same than the kernel one
  Superclass::SetRadius( kernel.GetRadius() );
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
}
}

#endif
