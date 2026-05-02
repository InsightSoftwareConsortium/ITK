/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLabelOverlapLabelSetMeasures_h
#define itkLabelOverlapLabelSetMeasures_h

#include "itkIntTypes.h"

namespace itk
{
/** \class LabelOverlapLabelSetMeasures
 * \brief Metrics stored per label
 * \ingroup ITKImageStatistics
 */
struct LabelOverlapLabelSetMeasures
{
  SizeValueType m_Source{ 0 };
  SizeValueType m_Target{ 0 };
  SizeValueType m_Union{ 0 };
  SizeValueType m_Intersection{ 0 };
  SizeValueType m_SourceComplement{ 0 };
  SizeValueType m_TargetComplement{ 0 };

  // ITK's igenerator wrapping pipeline does not expose public data members of
  // a struct to Python.  Provide explicit getters so wrapped consumers (Python
  // tests, downstream bindings) can read these fields.  Also provide
  // descriptively-named aliases for the m_-prefixed forms; the m_Foo names
  // remain accessible from C++ for backward compatibility with consumers that
  // construct or mutate these values directly.
  SizeValueType
  GetSource() const
  {
    return m_Source;
  }
  SizeValueType
  GetTarget() const
  {
    return m_Target;
  }
  SizeValueType
  GetUnion() const
  {
    return m_Union;
  }
  SizeValueType
  GetIntersection() const
  {
    return m_Intersection;
  }
  SizeValueType
  GetSourceComplement() const
  {
    return m_SourceComplement;
  }
  SizeValueType
  GetTargetComplement() const
  {
    return m_TargetComplement;
  }
};
} // namespace itk
#endif // itkLabelOverlapLabelSetMeasures_h
