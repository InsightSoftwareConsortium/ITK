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

#define ITK_TEMPLATE_EXPLICIT_TransformBase
#include "itkTransformBase.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const typename TransformBaseTemplate<double>::TransformCategoryType value)
{
  return out << [value] {
    switch (value)
    {
      case TransformBaseTemplate<double>::TransformCategoryType::UnknownTransformCategory:
        return "TransformBaseTemplate<double>::TransformCategoryType::UnknownTransformCategory";
      case TransformBaseTemplate<double>::TransformCategoryType::Linear:
        return "TransformBaseTemplate<double>::TransformCategoryType::Linear";
      case TransformBaseTemplate<double>::TransformCategoryType::BSpline:
        return "TransformBaseTemplate<double>::TransformCategoryType::BSpline";
      case TransformBaseTemplate<double>::TransformCategoryType::Spline:
        return "TransformBaseTemplate<double>::TransformCategoryType::Spline";
      case TransformBaseTemplate<double>::TransformCategoryType::DisplacementField:
        return "TransformBaseTemplate<double>::TransformCategoryType::DisplacementField";
      case TransformBaseTemplate<double>::TransformCategoryType::VelocityField:
        return "TransformBaseTemplate<double>::TransformCategoryType::VelocityField";
      default:
        return "INVALID VALUE FOR TransformBaseTemplate<double>::TransformCategoryType";
    }
  }();
}
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const typename TransformBaseTemplate<float>::TransformCategoryType value)
{
  return out << [value] {
    switch (value)
    {
      case TransformBaseTemplate<float>::TransformCategoryType::UnknownTransformCategory:
        return "TransformBaseTemplate<float>::TransformCategoryType::UnknownTransformCategory";
      case TransformBaseTemplate<float>::TransformCategoryType::Linear:
        return "TransformBaseTemplate<float>::TransformCategoryType::Linear";
      case TransformBaseTemplate<float>::TransformCategoryType::BSpline:
        return "TransformBaseTemplate<float>::TransformCategoryType::BSpline";
      case TransformBaseTemplate<float>::TransformCategoryType::Spline:
        return "TransformBaseTemplate<float>::TransformCategoryType::Spline";
      case TransformBaseTemplate<float>::TransformCategoryType::DisplacementField:
        return "TransformBaseTemplate<float>::TransformCategoryType::DisplacementField";
      case TransformBaseTemplate<float>::TransformCategoryType::VelocityField:
        return "TransformBaseTemplate<float>::TransformCategoryType::VelocityField";
      default:
        return "INVALID VALUE FOR TransformBaseTemplate<float>::TransformCategoryType";
    }
  }();
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKTransform_EXPORT TransformBaseTemplate<double>;
template class ITKTransform_EXPORT TransformBaseTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
