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
#ifndef itkCenteredTransformInitializer_hxx
#define itkCenteredTransformInitializer_hxx

#include "itkCenteredTransformInitializer.h"

namespace itk
{
template< typename TTransform, typename TFixedImage, typename TMovingImage >
CenteredTransformInitializer< TTransform, TFixedImage, TMovingImage >
::CenteredTransformInitializer()
{
  m_FixedCalculator  = FixedImageCalculatorType::New();
  m_MovingCalculator = MovingImageCalculatorType::New();
  m_UseMoments = false;
}

template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
CenteredTransformInitializer< TTransform, TFixedImage, TMovingImage >
::InitializeTransform()
{
  // Sanity check
  if ( !m_FixedImage )
    {
    itkExceptionMacro("Fixed Image has not been set");
    return;
    }
  if ( !m_MovingImage )
    {
    itkExceptionMacro("Moving Image has not been set");
    return;
    }
  if ( !m_Transform )
    {
    itkExceptionMacro("Transform has not been set");
    return;
    }

  // If images come from filters, then update those filters.
  if ( m_FixedImage->GetSource() )
    {
    m_FixedImage->GetSource()->Update();
    }
  if ( m_MovingImage->GetSource() )
    {
    m_MovingImage->GetSource()->Update();
    }

  InputPointType   rotationCenter;
  OutputVectorType translationVector;

  if ( m_UseMoments )
    {
    m_FixedCalculator->SetImage(m_FixedImage);
    m_FixedCalculator->Compute();

    m_MovingCalculator->SetImage(m_MovingImage);
    m_MovingCalculator->Compute();

    typename FixedImageCalculatorType::VectorType fixedCenter =
      m_FixedCalculator->GetCenterOfGravity();

    typename MovingImageCalculatorType::VectorType movingCenter =
      m_MovingCalculator->GetCenterOfGravity();

    for ( unsigned int i = 0; i < InputSpaceDimension; i++ )
      {
      rotationCenter[i]    = fixedCenter[i];
      translationVector[i] = movingCenter[i] - fixedCenter[i];
      }
    }
  else
    {
    // Here use the geometrical center of each image.

    const typename FixedImageType::RegionType & fixedRegion =
      m_FixedImage->GetLargestPossibleRegion();
    const typename FixedImageType::IndexType & fixedIndex =
      fixedRegion.GetIndex();
    const typename FixedImageType::SizeType & fixedSize =
      fixedRegion.GetSize();

    InputPointType centerFixedPoint;

    typedef typename InputPointType::ValueType CoordRepType;

    typedef ContinuousIndex< CoordRepType,
                             InputSpaceDimension >  ContinuousIndexType;

    typedef typename ContinuousIndexType::ValueType ContinuousIndexValueType;

    ContinuousIndexType centerFixedIndex;

    for ( unsigned int k = 0; k < InputSpaceDimension; k++ )
      {
      centerFixedIndex[k] =
        static_cast< ContinuousIndexValueType >( fixedIndex[k] )
        + static_cast< ContinuousIndexValueType >( fixedSize[k] - 1 ) / 2.0;
      }

    m_FixedImage->TransformContinuousIndexToPhysicalPoint(
      centerFixedIndex, centerFixedPoint);

    const typename MovingImageType::RegionType & movingRegion =
      m_MovingImage->GetLargestPossibleRegion();
    const typename MovingImageType::IndexType & movingIndex =
      movingRegion.GetIndex();
    const typename MovingImageType::SizeType & movingSize =
      movingRegion.GetSize();

    InputPointType centerMovingPoint;

    ContinuousIndexType centerMovingIndex;

    for ( unsigned int m = 0; m < InputSpaceDimension; m++ )
      {
      centerMovingIndex[m] =
        static_cast< ContinuousIndexValueType >( movingIndex[m] )
        + static_cast< ContinuousIndexValueType >( movingSize[m] - 1 ) / 2.0;
      }

    m_MovingImage->TransformContinuousIndexToPhysicalPoint(
      centerMovingIndex, centerMovingPoint);

    for ( unsigned int i = 0; i < InputSpaceDimension; i++ )
      {
      rotationCenter[i]    = centerFixedPoint[i];
      translationVector[i] = centerMovingPoint[i] - centerFixedPoint[i];
      }
    }

  m_Transform->SetCenter(rotationCenter);

  m_Transform->SetTranslation(translationVector);
}

template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
CenteredTransformInitializer< TTransform, TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Transform   = " << std::endl;
  if ( m_Transform )
    {
    os << indent << m_Transform  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "FixedImage   = " << std::endl;
  if ( m_FixedImage )
    {
    os << indent << m_FixedImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "MovingImage   = " << std::endl;
  if ( m_MovingImage )
    {
    os << indent << m_MovingImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "MovingMomentCalculator   = " << std::endl;
  if ( m_UseMoments && m_MovingCalculator )
    {
    os << indent << m_MovingCalculator  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "FixedMomentCalculator   = " << std::endl;
  if ( m_UseMoments && m_FixedCalculator )
    {
    os << indent << m_FixedCalculator  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }
}
}  // namespace itk

#endif /* itkCenteredTransformInitializer_hxx */
