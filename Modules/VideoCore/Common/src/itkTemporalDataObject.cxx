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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkProcessObject.h"
#include "itkSmartPointerForwardReference.txx"
#include "itkRealTimeClock.h"
#include "itkTemporalDataObject.h"

// Manual instantiation is necessary to prevent link errors
template class itk::SmartPointerForwardReference< itk::ProcessObject >;

namespace itk
{

//----------------------------------------------------------------------------
TemporalDataObject::TemporalDataObject()
  : m_LargestPossibleTemporalRegion(),
    m_RequestedTemporalRegion(),
    m_BufferedTemporalRegion(),
    m_TemporalUnit(Frame)
{
  m_DataObjectBuffer = BufferType::New();
}

//----------------------------------------------------------------------------
TemporalDataObject
::~TemporalDataObject()
{}

//----------------------------------------------------------------------------
void
TemporalDataObject
::SetRequestedRegionToLargestPossibleRegion()
{
  this->SetRequestedTemporalRegion( this->GetLargestPossibleTemporalRegion() );
}

//----------------------------------------------------------------------------
bool
TemporalDataObject
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  bool frameFlag = m_RequestedTemporalRegion.GetFrameStart() <
    m_BufferedTemporalRegion.GetFrameStart();
  frameFlag |= m_RequestedTemporalRegion.GetFrameDuration() >
    m_BufferedTemporalRegion.GetFrameDuration();
  bool realTimeFlag = m_RequestedTemporalRegion.GetRealStart() <
    m_BufferedTemporalRegion.GetRealStart();
  realTimeFlag |= m_RequestedTemporalRegion.GetRealDuration() >
    m_BufferedTemporalRegion.GetRealDuration();
  switch( m_TemporalUnit )
    {
    case Frame:
      {
      return frameFlag;
      }
    case RealTime:
      {
      return realTimeFlag;
      }
    case FrameAndRealTime:
      {
      return frameFlag || realTimeFlag;
      }
    default:
      itkExceptionMacro( << "itk::TemporalDataObject::"
                         << "RequestedRegionIsOutsideOfTheBufferedRegion() "
                         << "Invalid Temporal Unit" );
      return true;
    }
}

//----------------------------------------------------------------------------
bool
TemporalDataObject
::VerifyRequestedRegion()
{
  bool frameFlag = m_RequestedTemporalRegion.GetFrameStart() >=
    m_LargestPossibleTemporalRegion.GetFrameStart();
  frameFlag &= m_RequestedTemporalRegion.GetFrameDuration() <=
    m_LargestPossibleTemporalRegion.GetFrameDuration();
  bool realTimeFlag = m_RequestedTemporalRegion.GetRealStart() >=
    m_LargestPossibleTemporalRegion.GetRealStart();
  realTimeFlag &= m_RequestedTemporalRegion.GetRealDuration() <=
    m_LargestPossibleTemporalRegion.GetRealDuration();
  switch( m_TemporalUnit )
    {
    case Frame:
      {
      return frameFlag;
      }
    case RealTime:
      {
      return realTimeFlag;
      }
    case FrameAndRealTime:
      {
      return frameFlag && realTimeFlag;
      }
    default:
      itkExceptionMacro( << "itk::TemporalDataObject::VerifyRequestedRegion() "
                         << "Invalid Temporal Unit" );
      return false;
    }
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  const TemporalDataObject* temporalData;
  temporalData = dynamic_cast< const TemporalDataObject* >( data );

  if ( temporalData )
    {
    // Copy the meta data for this data type
    this->SetLargestPossibleTemporalRegion(
      temporalData->GetLargestPossibleTemporalRegion() );
    for( unsigned int i = 0;
         i < this->m_DataObjectBuffer->GetNumberOfBuffers();
         ++i )
      {
      if( this->m_DataObjectBuffer->BufferIsFull(i) )
        {
        m_DataObjectBuffer->GetBufferContents(i)->CopyInformation(
          temporalData->m_DataObjectBuffer->GetBufferContents(i) );
        }
      }
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::TemporalDataObject::CopyInformation() "
                       << "cannot cast " << typeid( data ).name() << " to "
                       << typeid( const TemporalDataObject* ).name() );
    }
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::Graft(const DataObject *data)
{
  const TemporalDataObject* temporalData;

  temporalData = dynamic_cast< const TemporalDataObject* >( data );

  if( temporalData )
    {
    // Copy the meta-information
    this->CopyInformation( temporalData );

    this->SetBufferedTemporalRegion(
      temporalData->GetBufferedTemporalRegion() );
    this->SetRequestedTemporalRegion(
      temporalData->GetRequestedTemporalRegion() );

    for( unsigned int i = 0;
         i < this->m_DataObjectBuffer->GetNumberOfBuffers();
         ++i )
      {
      if( this->m_DataObjectBuffer->BufferIsFull(i) )
        {
        m_DataObjectBuffer->GetBufferContents(i)->Graft(
          temporalData->m_DataObjectBuffer->GetBufferContents(i) );
        }
      }
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::TemporalDataObject::Graft() "
                       << "cannot cast " << typeid( data ).name() << " to "
                       << typeid( const TemporalDataObject* ).name() );
    }
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::SetRequestedRegion(DataObject *data)
{
  TemporalDataObject *temporalData;

  temporalData = dynamic_cast< TemporalDataObject * >( data );

  if ( temporalData )
    {
    // only copy the RequestedTemporalRegion if the parameter object is
    // a temporal data object
    this->SetRequestedTemporalRegion(
      temporalData->GetRequestedTemporalRegion() );
    for( unsigned int i = 0;
         i < this->m_DataObjectBuffer->GetNumberOfBuffers();
         ++i )
      {
      if( this->m_DataObjectBuffer->BufferIsFull(i) )
        {
        m_DataObjectBuffer->GetBufferContents(i)->SetRequestedRegion(
          temporalData->m_DataObjectBuffer->GetBufferContents(i) );
        }
      }
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::TemporalDataObject:SetRequestedRegion() "
                       << "cannot cast " << typeid( data ).name() << " to "
                       << typeid( const TemporalDataObject* ).name() );
    }
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Data Object Buffer: " << m_DataObjectBuffer.GetPointer()
     << std::endl;
  os << indent << "LargestPossibleTemporalRegion: " << std::endl;
  this->GetLargestPossibleTemporalRegion().Print( os,
                                                  indent.GetNextIndent() );

  os << indent << "BufferedTemporalRegion: " << std::endl;
  this->GetBufferedTemporalRegion().Print( os, indent.GetNextIndent() );

  os << indent << "RequestedTemporalRegion: " << std::endl;
  this->GetRequestedTemporalRegion().Print( os, indent.GetNextIndent() );
}

} // end namespace itk
