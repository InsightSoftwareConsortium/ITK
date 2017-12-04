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
#ifndef itkImageToSpatialObjectMetric_hxx
#define itkImageToSpatialObjectMetric_hxx

#include "itkImageToSpatialObjectMetric.h"

namespace itk
{
/** Constructor */
template< typename TFixedImage, typename TMovingSpatialObject >
ImageToSpatialObjectMetric< TFixedImage, TMovingSpatialObject >
::ImageToSpatialObjectMetric():
  m_MatchMeasure(0)
{
  m_FixedImage          = ITK_NULLPTR; // has to be provided by the user.
  m_MovingSpatialObject = ITK_NULLPTR; // has to be provided by the user.
  m_Transform           = ITK_NULLPTR; // has to be provided by the user.
  m_Interpolator        = ITK_NULLPTR; // has to be provided by the user.
}

/** Return the number of parameters required by the Transform */
template< typename TFixedImage, typename TMovingSpatialObject >
unsigned int
ImageToSpatialObjectMetric< TFixedImage, TMovingSpatialObject >
::GetNumberOfParameters( void ) const
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform is not present");
    }
  return m_Transform->GetNumberOfParameters();
}

/**
 * Initialize
 */

template< typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectMetric< TFixedImage, TMovingSpatialObject >
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

  if ( !m_MovingSpatialObject )
    {
    itkExceptionMacro(<< "MovingSpatialObject is not present");
    }

  if ( !m_FixedImage )
    {
    itkExceptionMacro(<< "FixedImage is not present");
    }

  // If the image is provided by a source, update the source.
  if ( m_FixedImage->GetSource() )
    {
    m_FixedImage->GetSource()->Update();
    }

  m_Interpolator->SetInputImage(m_FixedImage);

  // If there are any observers on the metric, call them to give the
  // user code a chance to set parameters on the metric
  this->InvokeEvent( InitializeEvent() );
}

/** PrintSelf */
template< typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectMetric< TFixedImage, TMovingSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Moving Spatial Object: " << m_MovingSpatialObject.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedImage.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Last Transform parameters = " << m_LastTransformParameters << std::endl;
}
} // end namespace itk

#endif
