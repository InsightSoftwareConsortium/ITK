/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredVersorTransformInitializer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredVersorTransformInitializer_txx
#define __itkCenteredVersorTransformInitializer_txx

#include "itkCenteredVersorTransformInitializer.h"

namespace itk
{
template< class TFixedImage, class TMovingImage >
CenteredVersorTransformInitializer< TFixedImage, TMovingImage >
::CenteredVersorTransformInitializer()
{
  // Force to use Moments computation since we need here the second
  // order moments in order to estimate a rotation
  this->Superclass::MomentsOn();

  this->m_ComputeRotation = false;
}

template< class TFixedImage, class TMovingImage >
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

    this->GetTransform()->SetMatrix(rotationMatrix);
    }
}

template< class TFixedImage, class TMovingImage >
void
CenteredVersorTransformInitializer< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Compute Rotation " << this->m_ComputeRotation << std::endl;
}
}  // namespace itk

#endif /* __itkCenteredVersorTransformInitializer_txx */
