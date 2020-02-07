/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkCastSpatialObjectFilter_hxx
#define itkCastSpatialObjectFilter_hxx

#include <iostream>

#include "itkCastSpatialObjectFilter.h"

namespace itk
{

/**
 *    Constructor
 */
template <unsigned int ObjectDimension>
CastSpatialObjectFilter<ObjectDimension>::CastSpatialObjectFilter()
{
  m_Input = nullptr;
}

/**
 *  Print Self
 */
template <unsigned int ObjectDimension>
void
CastSpatialObjectFilter<ObjectDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (m_Input.IsNotNull())
  {
    os << indent << "Input image: " << m_Input << std::endl;
  }
  else
  {
    os << indent << "Input image: NULL" << std::endl;
  }
}

} // end namespace itk

#endif
