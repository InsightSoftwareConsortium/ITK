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
#include "itkSegmentationRegion.h"

namespace itk
{
SegmentationRegion
::SegmentationRegion(void):
  m_RegionLabel(0),
  m_RegionArea(0)
{}

SegmentationRegion
::~SegmentationRegion()
{}

/**
 * PrintSelf
 */
void
SegmentationRegion
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Segmentation region object" << std::endl;
  os << indent << "Region label            : " << m_RegionLabel << std::endl;
  os << indent << "Area of the region      : " << m_RegionArea << std::endl;
}
} // end namespace itk
