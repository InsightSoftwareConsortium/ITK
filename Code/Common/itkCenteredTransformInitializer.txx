/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredTransformInitializer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredTransformInitializer_txx
#define __itkCenteredTransformInitializer_txx

#include "itkCenteredTransformInitializer.h"

namespace itk
{


template < class TTransform, class TFixedImage, class TMovingImage >
CenteredTransformInitializer<TTransform, TFixedImage, TMovingImage >
::CenteredTransformInitializer() 
{
  m_FixedCalculator  = FixedImageCalculatorType::New();
  m_MovingCalculator = MovingImageCalculatorType::New();
}


template < class TTransform, class TFixedImage, class TMovingImage >
void 
CenteredTransformInitializer<TTransform, TFixedImage, TMovingImage >
::InitializeTransform() const
{
  // Sanity check
  if( !m_FixedImage )
    {
    itkExceptionMacro( "Fixed Image has not been set" );
    return;
    }
  if( !m_MovingImage )
    {
    itkExceptionMacro( "Moving Image has not been set" );
    return;
    }
  if( !m_Transform )
    {
    itkExceptionMacro( "Transform has not been set" );
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


  InputPointType    rotationCenter;
  OutputVectorType  translationVector;


  if( m_UseMoments )
    {
    m_FixedCalculator->SetImage(  m_FixedImage );
    m_FixedCalculator->Compute();

    m_MovingCalculator->SetImage( m_MovingImage );
    m_MovingCalculator->Compute();
    
    typename FixedImageCalculatorType::VectorType fixedCenter =
      m_FixedCalculator->GetCenterOfGravity();

    typename MovingImageCalculatorType::VectorType movingCenter =
      m_MovingCalculator->GetCenterOfGravity();

    for( unsigned int i=0; i<InputSpaceDimension; i++)
      {
      rotationCenter[i]    = fixedCenter[i];
      translationVector[i] = movingCenter[i] - fixedCenter[i];
      }
    }
  else 
    {
    // Here use the geometrical center of each image.

    const typename FixedImageType::SpacingType& 
      fixedSpacing = m_FixedImage->GetSpacing();
    const typename FixedImageType::PointType&
      fixedOrigin  = m_FixedImage->GetOrigin();
    
    typename FixedImageType::SizeType fixedSize = 
      m_FixedImage->GetLargestPossibleRegion().GetSize();
    
    typename TransformType::InputPointType centerFixed;
    
    for( unsigned int k=0; k<InputSpaceDimension; k++ )
      {
      centerFixed[k] = fixedOrigin[k] + fixedSpacing[k] * fixedSize[k] / 2.0;
      }


    const typename MovingImageType::SpacingType&
      movingSpacing = m_MovingImage->GetSpacing();
    const typename MovingImageType::PointType&
      movingOrigin  = m_MovingImage->GetOrigin();
    
    typename MovingImageType::SizeType movingSize = 
      m_MovingImage->GetLargestPossibleRegion().GetSize();
    
    typename TransformType::InputPointType centerMoving;
    
    for( unsigned int m=0; m<InputSpaceDimension; m++ )
      {
      centerMoving[m] = movingOrigin[m] + movingSpacing[m] 
                                                   * movingSize[m] / 2.0;
      }

    for( unsigned int i=0; i<InputSpaceDimension; i++)
      {
      rotationCenter[i]    = centerFixed[i];
      translationVector[i] = centerMoving[i] - centerFixed[i];
      }

    }

  m_Transform->SetCenter( rotationCenter );

  m_Transform->SetTranslation( translationVector );

}
  

template < class TTransform, class TFixedImage, class TMovingImage >
void 
CenteredTransformInitializer<TTransform, TFixedImage, TMovingImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
     
  os << indent << "Transform   = " << std::endl;
  if (m_Transform)
    { 
    os << indent << m_Transform  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "FixedImage   = " << std::endl;
  if (m_FixedImage)
    { 
    os << indent << m_FixedImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "MovingImage   = " << std::endl;
  if (m_MovingImage)
    { 
    os << indent << m_MovingImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "MovingMomentCalculator   = " << std::endl;
  if (m_MovingCalculator)
    { 
    os << indent << m_MovingCalculator  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "FixedMomentCalculator   = " << std::endl;
  if (m_FixedCalculator)
    { 
    os << indent << m_FixedCalculator  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

}
 
}  // namespace itk

#endif /* __itkCenteredTransformInitializer_txx */
