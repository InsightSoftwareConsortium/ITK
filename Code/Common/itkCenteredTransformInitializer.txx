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


  InputPointType    rotationCenter;
  OutputVectorType  translationVector;


  if( m_UseMoments )
    {
    // Here use the center of mass for each image.
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

    for( unsigned int i=0; i<InputSpaceDimension; i++)
      {
      rotationCenter[i]    = movingCenter[i];
      translationVector[i] = movingCenter[i] - fixedCenter[i];
      }
    }
  else 
    {
    // Here use the geometrical center of each image.

    const double * fixedSpacing = m_FixedImage->GetSpacing();
    const double * fixedOrigin  = m_FixedImage->GetOrigin();
    
    typename FixedImageType::SizeType fixedSize = 
      m_FixedImage->GetLargestPossibleRegion().GetSize();
    
    typename TransformType::InputPointType centerFixed;
    
    centerFixed[0] = fixedOrigin[0] + fixedSpacing[0] * fixedSize[0] / 2.0;
    centerFixed[1] = fixedOrigin[1] + fixedSpacing[1] * fixedSize[1] / 2.0;


    const double * movingSpacing = m_MovingImage->GetSpacing();
    const double * movingOrigin  = m_MovingImage->GetOrigin();
    
    typename MovingImageType::SizeType movingSize = 
      m_MovingImage->GetLargestPossibleRegion().GetSize();
    
    typename TransformType::InputPointType centerMoving;
    
    centerMoving[0] = movingOrigin[0] + movingSpacing[0] * movingSize[0] / 2.0;
    centerMoving[1] = movingOrigin[1] + movingSpacing[1] * movingSize[1] / 2.0;

    for( unsigned int i=0; i<InputSpaceDimension; i++)
      {
      rotationCenter[i]    = centerMoving[i];
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
}
 
}  // namespace itk

#endif /* __itkCenteredTransformInitializer_txx */
