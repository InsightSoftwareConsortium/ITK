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
#ifndef itkPixelTransformation_hxx
#define itkPixelTransformation_hxx

#include "itkPixelTransformation.h"
#include "itkMatrixOffsetTransformBase.h"
namespace itk
{
template <typename TPixelType, typename TTransformType, typename TOutputPointType>
PixelTransformation<TPixelType, TTransformType, TOutputPointType>::PixelTransformation()
{
  // initialize variables
  this->InitializeDefaultImageTransform();
}

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
void
PixelTransformation<TPixelType, TTransformType, TOutputPointType>::InitializeDefaultImageTransform()
{
  // The m_ImageTransform is meant to be set by the user.
  // However, to avoid run-time error in case it has not been set, or having
  // to check for every point, a default m_ImageTransform is initialized.
  // The default m_ImageTransform is an affine transform set as identity.
  // Note that itkIdentityTransform cannot be use because it requires input
  // and output dimensions to coincide.
  // In case they do not coincide AffineTransform assign 1 to the main
  // diagonal and 0 elsewhere. The effect when the input dimension is larger
  // than the output one is a projection ignoring the last coordinates.
  // The effect when the input dimension is smaller that the output one is
  // a linear embedding, where the additional coordinates are set to zero.

  auto defaultImageTransform = MatrixOffsetTransformBase<typename TransformType::ParametersValueType,
                                                         TransformType::InputSpaceDimension,
                                                         TransformType::OutputSpaceDimension>::New();
  defaultImageTransform->SetIdentity();
  m_ImageTransform = dynamic_cast<TransformType *>(defaultImageTransform.GetPointer());
}

template <typename TPixelType, typename TTransformType, typename TOutputPointType>
void
PixelTransformation<TPixelType, TTransformType, TOutputPointType>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Transform: " << this->GetImageTransform() << std::endl;
}

} // end namespace itk

#endif
