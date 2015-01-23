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
#ifndef itkCenteredVersorTransformInitializer_hxx
#define itkCenteredVersorTransformInitializer_hxx

#include "itkCenteredVersorTransformInitializer.h"

namespace itk
{
template< typename TFixedImage, typename TMovingImage >
CenteredVersorTransformInitializer< TFixedImage, TMovingImage >
::CenteredVersorTransformInitializer()
{
  // Force to use Moments computation since we need here the second
  // order moments in order to estimate a rotation
  this->Superclass::MomentsOn();

  this->m_ComputeRotation = false;
}

template< typename TFixedImage, typename TMovingImage >
void
CenteredVersorTransformInitializer< TFixedImage, TMovingImage >
::InitializeTransform()
{
  // Compute moments and initialize center of rotaion and translation
  this->Superclass::InitializeTransform();

  if ( this->m_ComputeRotation )
    {
    typedef typename Superclass::FixedImageCalculatorType::MatrixType  FixedMatrixType;
    typedef typename Superclass::MovingImageCalculatorType::MatrixType MovingMatrixType;

    FixedMatrixType  fixedPrincipalAxis  = this->GetFixedCalculator()->GetPrincipalAxes();
    MovingMatrixType movingPrincipalAxis = this->GetMovingCalculator()->GetPrincipalAxes();

    MovingMatrixType rotationMatrix = movingPrincipalAxis * fixedPrincipalAxis.GetInverse();

    this->GetModifiableTransform()->SetMatrix(rotationMatrix);
    }
}

template< typename TFixedImage, typename TMovingImage >
void
CenteredVersorTransformInitializer< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Compute Rotation " << this->m_ComputeRotation << std::endl;
}
}  // namespace itk

#endif /* itkCenteredVersorTransformInitializer_hxx */
