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
  : m_DataObjectBuffer(),
    m_LargestPossibleTemporalRegion(),
    m_RequestedTemporalRegion(),
    m_BufferedTemporalRegion()
{}

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
  // TODO Implement this
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::VerifyRequestedRegion()
{
  /** TODO: Actually implement this

  bool         retval = true;
  unsigned int i;

  // Is the requested region within the LargestPossibleRegion?
  // Note that the test is indeed against the largest possible region
  // rather than the buffered region; see DataObject::VerifyRequestedRegion.
  const IndexType & requestedRegionIndex = this->GetRequestedRegion().GetIndex();
  const IndexType & largestPossibleRegionIndex =
    this->GetLargestPossibleRegion().GetIndex();

  const SizeType & requestedRegionSize = this->GetRequestedRegion().GetSize();
  const SizeType & largestPossibleRegionSize =
    this->GetLargestPossibleRegion().GetSize();

  for ( i = 0; i < VImageDimension; i++ )
    {
    if ( ( requestedRegionIndex[i] < largestPossibleRegionIndex[i] )
         || ( ( requestedRegionIndex[i] + static_cast< OffsetValueType >( requestedRegionSize[i] ) )
              > ( largestPossibleRegionIndex[i] + static_cast< OffsetValueType >( largestPossibleRegionSize[i] ) ) ) )
      {
      retval = false;
      }
    }

  return retval;**/
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  if ( data )
    {
    // Attempt to cast data to a TemporalDataObject
    const TemporalDataObject* temporalData;

    try
      {
      temporalData = dynamic_cast< const TemporalDataObject* >( data );
    catch ( ... )
      {
      return;
      }

    if ( temporalData )
      {
      // Copy the meta data for this data type
      this->SetLargestPossibleTemporalRegion(
        temporalData->GetLargestPossibleTemporalRegion() );
      }
    else
      {
      // pointer could not be cast back down
      itkExceptionMacro( << "itk::TemporalDataObject::CopyInformation() "
                         << "cannot cast " << typeid( data ).name() << " to "
                         << typeid( const TemporalDataObject * ).name() );
      }
    }

  // TODO Iterate through the ring buffer
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::Graft(const DataObject *data)
{

  const TemporalDataObject* temporalData;

  try
    {
    temporalData = dynamic_cast< const TemporalDataObject* >( data );
    }
  catch ( ... )
    {
    return;
    }

  if ( !temporalData )
    {
    return;
    }

  // Copy the meta-information
  this->CopyInformation( temporalData );

  this->SetBufferedTemporalRegion( temporalData->GetBufferedTemporalRegion() );
  this->SetRequestedTemporalRegion(
    temporalData->GetRequestedTemporalRegion() );

  // TODO Add iteration through the ring buffer grafting
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::SetRequestedRegion(DataObject *data)
{
  TemporalDataObject *imgData;

  temporalData = dynamic_cast< TemporalDataObject * >( data );

  if ( temporalData )
    {
    // only copy the RequestedTemporalRegion if the parameter object is
    // a temporal data object
    this->SetRequestedTemporalRegion(
      temporalData->GetRequestedTemporalRegion() );
    }
}

//----------------------------------------------------------------------------
void
TemporalDataObject
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Ring Buffer: " << m_RingBuffer.GetPointer() << std::endl;
  os << indent << "LargestPossibleTemporalRegion: " << std::endl;
  this->GetLargestPossibleTemporalRegion().PrintSelf( os,
                                                      indent.GetNextIndent() );

  os << indent << "BufferedTemporalRegion: " << std::endl;
  this->GetBufferedTemporalRegion().PrintSelf( os, indent.GetNextIndent() );

  os << indent << "RequestedTemporalRegion: " << std::endl;
  this->GetRequestedTemporalRegion().PrintSelf( os, indent.GetNextIndent() );
}

} // end namespace itk
