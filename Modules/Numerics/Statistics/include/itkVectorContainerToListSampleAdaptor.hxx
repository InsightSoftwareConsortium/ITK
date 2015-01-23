/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License" );
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
#ifndef itkVectorContainerToListSampleAdaptor_hxx
#define itkVectorContainerToListSampleAdaptor_hxx

#include "itkVectorContainerToListSampleAdaptor.h"

namespace itk
{
namespace Statistics
{
template<typename TVectorContainer>
VectorContainerToListSampleAdaptor<TVectorContainer>
::VectorContainerToListSampleAdaptor()
{
  this->m_VectorContainer = ITK_NULLPTR;
}

template<typename TVectorContainer>
void
VectorContainerToListSampleAdaptor<TVectorContainer>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "VectorContainer: ";
  if( this->m_VectorContainer.IsNotNull() )
    {
    os << this->m_VectorContainer << std::endl;
    }
  else
    {
    os << "not set." << std::endl;
    }
}

/** returns the number of measurement vectors in this container*/
template<typename TVectorContainer>
typename VectorContainerToListSampleAdaptor<TVectorContainer>
::InstanceIdentifier
VectorContainerToListSampleAdaptor<TVectorContainer>
::Size() const
{
  if( this->m_VectorContainer.IsNull() )
    {
    itkExceptionMacro( "Vector container has not been set yet" );
    }

  return this->m_VectorContainer->Size();
}

template<typename TVectorContainer>
inline const typename VectorContainerToListSampleAdaptor<TVectorContainer>
::MeasurementVectorType &
VectorContainerToListSampleAdaptor<TVectorContainer>
::GetMeasurementVector( InstanceIdentifier identifier ) const
{
  if( this->m_VectorContainer.IsNull() )
    {
    itkExceptionMacro( "Vector container has not been set yet" );
    }

  this->m_TempPoint = this->m_VectorContainer->GetElement( identifier );
  return this->m_TempPoint;
}

template<typename TVectorContainer>
inline typename VectorContainerToListSampleAdaptor<TVectorContainer>
::AbsoluteFrequencyType
VectorContainerToListSampleAdaptor<TVectorContainer>
::GetFrequency(InstanceIdentifier) const
{
  if( this->m_VectorContainer.IsNull() )
    {
    itkExceptionMacro( "Vector container has not been set yet" );
    }

  return 1;
}

template<typename TVectorContainer>
typename VectorContainerToListSampleAdaptor<TVectorContainer>
::TotalAbsoluteFrequencyType
VectorContainerToListSampleAdaptor<TVectorContainer>
::GetTotalFrequency() const
{
  if( this->m_VectorContainer.IsNull() )
    {
    itkExceptionMacro( "Vector container has not been set yet" );
    }
  return this->Size();
}
} // end of namespace Statistics
} // end of namespace itk

#endif
