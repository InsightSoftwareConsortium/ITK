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
#include "itkSegmentationBorder.h"

namespace itk
{
SegmentationBorder
::SegmentationBorder(void):
  m_BorderLength(0)
{}

SegmentationBorder
::~SegmentationBorder()
{}

/**
 * PrintSelf
 */
void
SegmentationBorder
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Segmentation border object" << std::endl;
  os << indent << "Length of the border: " << m_BorderLength << std::endl;
}
} // end namespace itk
