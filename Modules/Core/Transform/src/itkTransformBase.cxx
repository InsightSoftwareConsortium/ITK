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
operator<<(std::ostream & out, const TransformBaseTemplateEnums::TransformCategory value)
{
  return out << [value] {
    switch (value)
    {
      case TransformBaseTemplateEnums::TransformCategory::UnknownTransformCategory:
        return "itk::TransformBaseTemplateEnums::TransformCategory::UnknownTransformCategory";
      case TransformBaseTemplateEnums::TransformCategory::Linear:
        return "itk::TransformBaseTemplateEnums::TransformCategory::Linear";
      case TransformBaseTemplateEnums::TransformCategory::BSpline:
        return "itk::TransformBaseTemplateEnums::TransformCategory::BSpline";
      case TransformBaseTemplateEnums::TransformCategory::Spline:
        return "itk::TransformBaseTemplateEnums::TransformCategory::Spline";
      case TransformBaseTemplateEnums::TransformCategory::DisplacementField:
        return "itk::TransformBaseTemplateEnums::TransformCategory::DisplacementField";
      case TransformBaseTemplateEnums::TransformCategory::VelocityField:
        return "itk::TransformBaseTemplateEnums::TransformCategory::VelocityField";
      default:
        return "INVALID VALUE FOR itk::TransformBaseTemplateEnums::TransformCategory";
    }
  }();
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKTransform_EXPORT TransformBaseTemplate<double>;
template class ITKTransform_EXPORT TransformBaseTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
