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
#ifndef __itkKernelImageFilter_txx
#define __itkKernelImageFilter_txx

#include "itkKernelImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage, class TKernel >
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::KernelImageFilter()
{
  this->SetRadius(1UL);
}

namespace KernelImageFilterLocal
{
struct true_type {};
struct false_type{};
template<typename, typename>
struct is_same
  : public false_type { };

template<typename _Tp>
struct is_same<_Tp, _Tp>
  : public true_type { };

template <typename RadiusType, typename KernelType>
KernelType
__SetRadius(const RadiusType &radius,const true_type &)
{
  KernelType kernel = KernelType::Box(radius);
  return kernel;
}

template <typename RadiusType, typename KernelType>
KernelType
__SetRadius(const RadiusType &radius,const false_type &)
{
  KernelType kernel;
  kernel.SetRadius(radius);
  return kernel;
  for ( typename KernelType::Iterator kit = kernel.Begin();
        kit != kernel.End(); kit++ )
    {
    *kit = 1;
    }
  return kernel;
}
} // namespace KernelImageFilterLocal

template< class TInputImage, class TOutputImage, class TKernel >
void
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::SetRadius(const RadiusType & radius)
{
  KernelType kernel =
    KernelImageFilterLocal::__SetRadius<RadiusType,KernelType>
    (radius,
     KernelImageFilterLocal::is_same<KernelType,FlatKernelType>());
  this->SetKernel(kernel);
}

template< class TInputImage, class TOutputImage, class TKernel >
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

template< class TInputImage, class TOutputImage, class TKernel >
void
KernelImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
}
}

#endif
