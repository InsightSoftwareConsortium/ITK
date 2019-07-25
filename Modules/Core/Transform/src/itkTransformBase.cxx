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
    std::ostream& operator<<(std::ostream& out,const typename TransformBaseTemplate<double>::TransformCategoryType value)
    {
        const char* s = 0;
        switch(value)
        {
            case TransformBaseTemplate<double>::TransformCategoryType::UnknownTransformCategory: s = "TransformBaseTemplate<double>::TransformCategoryType::UnknownTransformCategory"; break;
            case TransformBaseTemplate<double>::TransformCategoryType::Linear: s = "TransformBaseTemplate<double>::TransformCategoryType::Linear"; break;
            case TransformBaseTemplate<double>::TransformCategoryType::BSpline: s = "TransformBaseTemplate<double>::TransformCategoryType::BSpline"; break;
            case TransformBaseTemplate<double>::TransformCategoryType::Spline: s = "TransformBaseTemplate<double>::TransformCategoryType::Spline"; break;
            case TransformBaseTemplate<double>::TransformCategoryType::DisplacementField: s = "TransformBaseTemplate<double>::TransformCategoryType::DisplacementField"; break;
            case TransformBaseTemplate<double>::TransformCategoryType::VelocityField: s = "TransformBaseTemplate<double>::TransformCategoryType::VelocityField"; break;
            default: s = "INVALID VALUE FOR TransformBaseTemplate<double>::TransformCategoryType";
        }
        return out << s;
    }
    /** Print enum values */
    std::ostream& operator<<(std::ostream& out,const typename TransformBaseTemplate<float>::TransformCategoryType value)
    {
        const char* s = 0;
        switch(value)
        {
            case TransformBaseTemplate<float>::TransformCategoryType::UnknownTransformCategory: s = "TransformBaseTemplate<float>::TransformCategoryType::UnknownTransformCategory"; break;
            case TransformBaseTemplate<float>::TransformCategoryType::Linear: s = "TransformBaseTemplate<float>::TransformCategoryType::Linear"; break;
            case TransformBaseTemplate<float>::TransformCategoryType::BSpline: s = "TransformBaseTemplate<float>::TransformCategoryType::BSpline"; break;
            case TransformBaseTemplate<float>::TransformCategoryType::Spline: s = "TransformBaseTemplate<float>::TransformCategoryType::Spline"; break;
            case TransformBaseTemplate<float>::TransformCategoryType::DisplacementField: s = "TransformBaseTemplate<float>::TransformCategoryType::DisplacementField"; break;
            case TransformBaseTemplate<float>::TransformCategoryType::VelocityField: s = "TransformBaseTemplate<float>::TransformCategoryType::VelocityField"; break;
            default: s = "INVALID VALUE FOR TransformBaseTemplate<float>::TransformCategoryType";
        }
        return out << s;
    }

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKTransform_EXPORT TransformBaseTemplate< double >;
template class ITKTransform_EXPORT TransformBaseTemplate< float >;

ITK_GCC_PRAGMA_DIAG_POP()

}  // end namespace itk
