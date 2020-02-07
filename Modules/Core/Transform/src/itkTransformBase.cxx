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

#define ITK_TEMPLATE_EXPLICIT_TransformBase
#include "itkTransformBase.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const typename TransformBaseTemplate<double>::TransformCategoryEnum value)
{
  return out << [value] {
    switch (value)
    {
      case TransformBaseTemplate<double>::TransformCategoryEnum::UnknownTransformCategory:
        return "TransformBaseTemplate<double>::TransformCategoryEnum::UnknownTransformCategory";
      case TransformBaseTemplate<double>::TransformCategoryEnum::Linear:
        return "TransformBaseTemplate<double>::TransformCategoryEnum::Linear";
      case TransformBaseTemplate<double>::TransformCategoryEnum::BSpline:
        return "TransformBaseTemplate<double>::TransformCategoryEnum::BSpline";
      case TransformBaseTemplate<double>::TransformCategoryEnum::Spline:
        return "TransformBaseTemplate<double>::TransformCategoryEnum::Spline";
      case TransformBaseTemplate<double>::TransformCategoryEnum::DisplacementField:
        return "TransformBaseTemplate<double>::TransformCategoryEnum::DisplacementField";
      case TransformBaseTemplate<double>::TransformCategoryEnum::VelocityField:
        return "TransformBaseTemplate<double>::TransformCategoryEnum::VelocityField";
      default:
        return "INVALID VALUE FOR TransformBaseTemplate<double>::TransformCategoryEnum";
    }
  }();
}
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const typename TransformBaseTemplate<float>::TransformCategoryEnum value)
{
  return out << [value] {
    switch (value)
    {
      case TransformBaseTemplate<float>::TransformCategoryEnum::UnknownTransformCategory:
        return "TransformBaseTemplate<float>::TransformCategoryEnum::UnknownTransformCategory";
      case TransformBaseTemplate<float>::TransformCategoryEnum::Linear:
        return "TransformBaseTemplate<float>::TransformCategoryEnum::Linear";
      case TransformBaseTemplate<float>::TransformCategoryEnum::BSpline:
        return "TransformBaseTemplate<float>::TransformCategoryEnum::BSpline";
      case TransformBaseTemplate<float>::TransformCategoryEnum::Spline:
        return "TransformBaseTemplate<float>::TransformCategoryEnum::Spline";
      case TransformBaseTemplate<float>::TransformCategoryEnum::DisplacementField:
        return "TransformBaseTemplate<float>::TransformCategoryEnum::DisplacementField";
      case TransformBaseTemplate<float>::TransformCategoryEnum::VelocityField:
        return "TransformBaseTemplate<float>::TransformCategoryEnum::VelocityField";
      default:
        return "INVALID VALUE FOR TransformBaseTemplate<float>::TransformCategoryEnum";
    }
  }();
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKTransform_EXPORT TransformBaseTemplate<double>;
template class ITKTransform_EXPORT TransformBaseTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
