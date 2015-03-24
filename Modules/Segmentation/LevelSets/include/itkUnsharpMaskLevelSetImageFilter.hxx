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
#ifndef itkUnsharpMaskLevelSetImageFilter_hxx
#define itkUnsharpMaskLevelSetImageFilter_hxx

#include "itkUnsharpMaskLevelSetImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
UnsharpMaskLevelSetImageFilter< TInputImage, TOutputImage >
::UnsharpMaskLevelSetImageFilter()
{
  RadiusType radius;

  for ( unsigned int j = 0; j < Self::ImageDimension; j++ )
    {
    radius[j] = 1;
    }

  m_Function = FunctionType::New();
  this->SetLevelSetFunction(m_Function);
  this->SetNumberOfLayers( this->GetMinimumNumberOfLayers() );

  this->SetNormalProcessType (0);          // isotropic diffusion
  this->SetNormalProcessUnsharpFlag(true); // unsharp masking on
  this->SetNormalProcessUnsharpWeight (1);
  this->SetMaxNormalIteration(25);
  this->SetMaxRefitIteration(100);
  m_MaxFilterIteration = 99;
  m_Function->Initialize(radius);
}

template< typename TInputImage, typename TOutputImage >
void
UnsharpMaskLevelSetImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaxFilterIteration: " << m_MaxFilterIteration << std::endl;
}
} // end namespace itk

#endif
