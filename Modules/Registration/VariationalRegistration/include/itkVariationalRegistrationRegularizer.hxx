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
#ifndef itkVariationalRegistrationRegularizer_hxx
#define itkVariationalRegistrationRegularizer_hxx
#include "itkVariationalRegistrationRegularizer.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TDisplacementField>
VariationalRegistrationRegularizer<TDisplacementField>::VariationalRegistrationRegularizer()
{
  // Initialize default values.
  m_UseImageSpacing = true;
}

/*
 * Print status information
 */
template <typename TDisplacementField>
void
VariationalRegistrationRegularizer<TDisplacementField>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UseImageSpacing: ";
  os << m_UseImageSpacing << std::endl;
}

} // end namespace itk

#endif
