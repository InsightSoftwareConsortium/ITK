/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredTransformInitializer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredTransformInitializer_txx
#define __itkCenteredTransformInitializer_txx

#include "itkCenteredTransformInitializer.h"
#include "itkImageMomentsCalculator.h"

namespace itk
{

template < class TTransform, class TFixedImage, class TMovingImage >
void 
CenteredTransformInitializer<TTransform, TFixedImage, TMovingImage >
::InitializeTransform() const
{
  // Sanity check
  if( !m_FixedImage )
    {
    return;
    }
  if( !m_MovingImage )
    {
    return;
    }
  if( !m_Transform )
    {
    return;
    }

  // If images come from filters, then update those filters.
  if( m_FixedImage->GetSource() )
    { 
    m_FixedImage->GetSource()->Update();
    }
  if( m_MovingImage->GetSource() )
    { 
    m_MovingImage->GetSource()->Update();
    }

  typedef ImageMomentsCalculator< FixedImageType >   FixedImageCalculatorType;
  typedef ImageMomentsCalculator< MovingImageType >  MovingImageCalculatorType;

  FixedImageCalculatorType    fixedCalculator;
  MovingImageCalculatorType   movingCalculator;

  fixedCalculator.ComputeMoments(  m_FixedImage );
  movingCalculator.ComputeMoments( m_MovingImage );
  
  typename FixedImageCalculatorType::VectorType fixedCenter =
                        fixedCalculator.GetCenterOfGravity();

  typename MovingImageCalculatorType::VectorType movingCenter =
                        movingCalculator.GetCenterOfGravity();

  InputPointType rotationCenter;
  OutputVectorType translationVector;

  for( unsigned int i=0; i<InputSpaceDimension; i++)
    {
    rotationCenter[i]    = movingCenter[i];
    translationVector[i] = movingCenter[i] - fixedCenter[i];
    }

  m_Transform->SetCenterOfRotation( rotationCenter );

  m_Transform->SetTranslation( translationVector );


}
  

      

template < class TTransform, class TFixedImage, class TMovingImage >
void 
CenteredTransformInitializer<TTransform, TFixedImage, TMovingImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Transform   = " << m_Transform   << std::endl;
  os << indent << "FixedImage  = " << m_FixedImage  << std::endl;
  os << indent << "MovingImage = " << m_MovingImage << std::endl;

}



 
}  // namespace itk

#endif /* __itkCenteredTransformInitializer_txx */
