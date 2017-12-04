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
#ifndef itkPointSetToImageMetric_hxx
#define itkPointSetToImageMetric_hxx

#include "itkPointSetToImageMetric.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFixedPointSet, typename TMovingImage >
PointSetToImageMetric< TFixedPointSet, TMovingImage >
::PointSetToImageMetric()
{
  m_FixedPointSet = ITK_NULLPTR;         // has to be provided by the user.
  m_MovingImage   = ITK_NULLPTR;         // has to be provided by the user.
  m_Transform     = ITK_NULLPTR;         // has to be provided by the user.
  m_Interpolator  = ITK_NULLPTR;         // has to be provided by the user.
  m_ComputeGradient = true;    // metric computes gradient by default
  m_NumberOfPixelsCounted = 0; // initialize to zero
  m_GradientImage = ITK_NULLPTR;      // computed at initialization
}

/**
 * Set the parameters that define a unique transform
 */
template< typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageMetric< TFixedPointSet, TMovingImage >
::SetTransformParameters(const ParametersType & parameters) const
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform has not been assigned");
    }
  m_Transform->SetParameters(parameters);
}

/**
 * PrintSelf
 */
template< typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageMetric< TFixedPointSet, TMovingImage >
::Initialize(void)
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform is not present");
    }

  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator is not present");
    }

  if ( !m_MovingImage )
    {
    itkExceptionMacro(<< "MovingImage is not present");
    }

  if ( !m_FixedPointSet )
    {
    itkExceptionMacro(<< "FixedPointSet is not present");
    }

  // If the image is provided by a source, update the source.
  if ( m_MovingImage->GetSource() )
    {
    m_MovingImage->GetSource()->Update();
    }

  // If the point set is provided by a source, update the source.
  if ( m_FixedPointSet->GetSource() )
    {
    m_FixedPointSet->GetSource()->Update();
    }

  m_Interpolator->SetInputImage(m_MovingImage);

  if ( m_ComputeGradient )
    {
    GradientImageFilterPointer gradientFilter =
      GradientImageFilterType::New();

    gradientFilter->SetInput(m_MovingImage);

    const typename MovingImageType::SpacingType &
    spacing = m_MovingImage->GetSpacing();
    double maximumSpacing = 0.0;
    for ( unsigned int i = 0; i < MovingImageDimension; i++ )
      {
      if ( spacing[i] > maximumSpacing )
        {
        maximumSpacing = spacing[i];
        }
      }
    gradientFilter->SetSigma(maximumSpacing);
    gradientFilter->SetNormalizeAcrossScale(true);

    gradientFilter->Update();

    m_GradientImage = gradientFilter->GetOutput();
    }
}

/**
 * PrintSelf
 */
template< typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageMetric< TFixedPointSet, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Moving Image: " << m_MovingImage.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedPointSet.GetPointer()   << std::endl;
  os << indent << "Gradient Image: " << m_GradientImage.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Number of Pixels Counted: " << m_NumberOfPixelsCounted << std::endl;
  os << indent << "Compute Gradient: " << m_ComputeGradient << std::endl;
}
} // end namespace itk

#endif
