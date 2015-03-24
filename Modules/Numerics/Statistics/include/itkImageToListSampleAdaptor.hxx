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
#ifndef itkImageToListSampleAdaptor_hxx
#define itkImageToListSampleAdaptor_hxx

#include "itkImageToListSampleAdaptor.h"

namespace itk
{
namespace Statistics
{
template< typename TImage >
ImageToListSampleAdaptor< TImage >
::ImageToListSampleAdaptor()
{
  m_Image = ITK_NULLPTR;
}

template< typename TImage >
const typename ImageToListSampleAdaptor< TImage >::MeasurementVectorType &
ImageToListSampleAdaptor< TImage >
::GetMeasurementVector(InstanceIdentifier id) const
{
  if ( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }
  MeasurementVectorTraits::Assign( m_MeasurementVectorInternal,
                                   m_Image->GetPixel( m_Image->ComputeIndex(id) ) );

  return m_MeasurementVectorInternal;
}

/** returns the number of measurement vectors in this container*/
template< typename TImage >
typename ImageToListSampleAdaptor< TImage >::InstanceIdentifier
ImageToListSampleAdaptor< TImage >
::Size() const
{
  if ( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return m_Image->GetLargestPossibleRegion().GetNumberOfPixels();
}

template< typename TImage >
inline typename ImageToListSampleAdaptor< TImage >::AbsoluteFrequencyType
ImageToListSampleAdaptor< TImage >
::GetFrequency(InstanceIdentifier) const
{
  if ( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return NumericTraits< AbsoluteFrequencyType >::OneValue();
}

template< typename TImage >
void
ImageToListSampleAdaptor< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Image: ";
  if ( m_Image.IsNotNull() )
    {
    os << m_Image << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
  os << indent << "MeasurementVectorSize: ";
  if ( m_Image.IsNotNull() )
    {
    os << this->GetMeasurementVectorSize() << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
}

template< typename TImage >
void
ImageToListSampleAdaptor< TImage >
::SetImage(const TImage *image)
{
  m_Image = image;
  this->Modified();
}

template< typename TImage >
const TImage *
ImageToListSampleAdaptor< TImage >
::GetImage() const
{
  if ( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return m_Image.GetPointer();
}

template< typename TImage >
typename ImageToListSampleAdaptor< TImage >::TotalAbsoluteFrequencyType
ImageToListSampleAdaptor< TImage >
::GetTotalFrequency() const
{
  if ( m_Image.IsNull() )
    {
    itkExceptionMacro("Image has not been set yet");
    }

  return this->Size();
}
} // end of namespace Statistics
} // end of namespace itk

#endif
