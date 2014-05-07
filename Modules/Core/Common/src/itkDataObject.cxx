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

namespace itk
{
// after use by filter
bool DataObject:: m_GlobalReleaseDataFlag = false;

DataObjectError
::DataObjectError():
  ExceptionObject(), m_DataObject(ITK_NULLPTR)
{}

DataObjectError
::DataObjectError(const char *file, unsigned int lineNumber):
  ExceptionObject(file, lineNumber), m_DataObject(ITK_NULLPTR)
{}

DataObjectError
::DataObjectError(const std::string & file, unsigned int lineNumber):
  ExceptionObject(file, lineNumber), m_DataObject(ITK_NULLPTR)
{}

DataObjectError
::DataObjectError(const DataObjectError & orig):
  ExceptionObject(orig)
{
  m_DataObject = orig.m_DataObject;
}

DataObjectError &
DataObjectError
::operator=(const DataObjectError & orig)
{
  ExceptionObject::operator=(orig);
  m_DataObject = orig.m_DataObject;
  return *this;
}

void
DataObjectError
::SetDataObject(DataObject *dobj)
{
  m_DataObject = dobj;
}

DataObject *
DataObjectError
::GetDataObject()
{
  return m_DataObject;
}

void
DataObjectError
::PrintSelf(std::ostream & os, Indent indent) const
{
  ExceptionObject::Print(os);

  os << indent << "Data object: ";
  if ( m_DataObject )
    {
    os << std::endl;
    m_DataObject->PrintSelf( os, indent.GetNextIndent() );
    }
  else
    {
    os << "(None)" << std::endl;
    }
}

InvalidRequestedRegionError
::InvalidRequestedRegionError():
  DataObjectError()
{}

InvalidRequestedRegionError
::InvalidRequestedRegionError(const char *file, unsigned int lineNumber):
  DataObjectError(file, lineNumber)
{}

InvalidRequestedRegionError
::InvalidRequestedRegionError(const std::string & file, unsigned int lineNumber):
  DataObjectError(file, lineNumber)
{}

InvalidRequestedRegionError
::InvalidRequestedRegionError(const InvalidRequestedRegionError & orig):
  DataObjectError(orig)
{}

InvalidRequestedRegionError &
InvalidRequestedRegionError
::operator=(const InvalidRequestedRegionError & orig)
{
  DataObjectError::operator=(orig);
  return *this;
}

void
InvalidRequestedRegionError
::PrintSelf(std::ostream & os, Indent indent) const
{
  DataObjectError::PrintSelf(os, indent);
}

//----------------------------------------------------------------------------
DataObject::DataObject():m_UpdateMTime()
{
  m_Source = ITK_NULLPTR;
  m_SourceOutputName = "";
  m_ReleaseDataFlag = false;

  // We have to assume that if a user is creating the data on their own,
  // then they will fill it with valid data.
  m_DataReleased = false;

  m_PipelineMTime = 0;
}

//----------------------------------------------------------------------------
DataObject
::~DataObject()
{}

//----------------------------------------------------------------------------
void
DataObject
::Initialize()
{
// We don't modify ourselves because the "ReleaseData" methods depend upon
// no modification when initialized.
//
}

//----------------------------------------------------------------------------
void
DataObject
::SetGlobalReleaseDataFlag(bool val)
{
  if ( val == m_GlobalReleaseDataFlag )
    {
    return;
    }
  m_GlobalReleaseDataFlag = val;
}

//----------------------------------------------------------------------------
bool
DataObject
::GetGlobalReleaseDataFlag()
{
  return m_GlobalReleaseDataFlag;
}

//----------------------------------------------------------------------------
void
DataObject
::ReleaseData()
{
  this->Initialize();
  m_DataReleased = true;
}

//----------------------------------------------------------------------------
bool
DataObject
::ShouldIReleaseData() const
{
  return ( m_GlobalReleaseDataFlag || m_ReleaseDataFlag );
}

//----------------------------------------------------------------------------
// Set the process object that generates this data object.
//
void
DataObject
::DisconnectPipeline()
{
  itkDebugMacro("disconnecting from the pipeline.");

  // disconnect ourselves from the current process object
  if ( m_Source )
    {
    m_Source->SetOutput(m_SourceOutputName, ITK_NULLPTR);
    }

  // set our release data flag to off by default (purposely done after
  // we have disconnected from the pipeline so the new output of the
  // source can copy our original ReleaseDataFlag)
  this->ReleaseDataFlagOff();

  // reset our PipelineMTime (there is now nothing upstream from us)
  m_PipelineMTime = 0;

  this->Modified();
}

bool
DataObject
::DisconnectSource(ProcessObject *arg, const DataObjectIdentifierType & name)
{
  if ( m_Source == arg && m_SourceOutputName == name )
    {
    itkDebugMacro("disconnecting source  " << arg
                                           << ", source output name " << name);

    m_Source = ITK_NULLPTR;
    m_SourceOutputName = "";
    this->Modified();
    return true;
    }
  else
    {
    itkDebugMacro("could not disconnect source  " << arg
                                                  << ", source output name " << name);
    return false;
    }
}

bool
DataObject
::ConnectSource(ProcessObject *arg, const DataObjectIdentifierType & name)
{
  if ( m_Source != arg || m_SourceOutputName != name )
    {
    itkDebugMacro("connecting source  " << arg
                                        << ", source output name " << name);

    m_Source = arg;
    m_SourceOutputName = name;
    this->Modified();
    return true;
    }
  else
    {
    itkDebugMacro("could not connect source  " << arg
                                               << ", source output name " << name);

    return false;
    }
}

//----------------------------------------------------------------------------

SmartPointerForwardReference< ProcessObject >
DataObject
::GetSource() const
{
  itkDebugMacro( "returning Source address " << m_Source.GetPointer() );
  return m_Source.GetPointer();
}

const DataObject::DataObjectIdentifierType &
DataObject
::GetSourceOutputName() const
{
  itkDebugMacro("returning Source name " << m_SourceOutputName);
  return m_SourceOutputName;
}

DataObject::DataObjectPointerArraySizeType
DataObject
::GetSourceOutputIndex() const
{
  if( ! m_Source )
    {
    return 0;
    }
  return m_Source->MakeIndexFromOutputName( m_SourceOutputName );
}

//----------------------------------------------------------------------------
void
DataObject
::PrintSelf(std::ostream & os, Indent indent) const
{
  Object::PrintSelf(os, indent);

  if ( m_Source )
    {
    os << indent << "Source: (" << m_Source.GetPointer() << ") \n";
    os << indent << "Source output name: " << m_SourceOutputName << "\n";
    }
  else
    {
    os << indent << "Source: (none)\n";
    os << indent << "Source output name: (none)\n";
    }

  os << indent << "Release Data: "
     << ( m_ReleaseDataFlag ? "On\n" : "Off\n" );

  os << indent << "Data Released: "
     << ( m_DataReleased ? "True\n" : "False\n" );

  os << indent << "Global Release Data: "
     << ( m_GlobalReleaseDataFlag ? "On\n" : "Off\n" );

  os << indent << "PipelineMTime: " << m_PipelineMTime << std::endl;
  os << indent << "UpdateMTime: " << m_UpdateMTime << std::endl;
  os << indent << "RealTimeStamp: " << m_RealTimeStamp << std::endl;
}

// The following methods are used for updating the data processing pipeline.
//

//----------------------------------------------------------------------------
void
DataObject
::Update()
{
  this->UpdateOutputInformation();
  this->PropagateRequestedRegion();
  this->UpdateOutputData();
}

void
DataObject
::UpdateOutputInformation()
{

  if ( this->GetSource() )
    {
    this->GetSource()->UpdateOutputInformation();
    }
}

void
DataObject
::ResetPipeline()
{
  this->PropagateResetPipeline();
}

void
DataObject
::PropagateResetPipeline()
{
  if ( m_Source )
    {
    m_Source->PropagateResetPipeline();
    }
}

//----------------------------------------------------------------------------
void
DataObject
::PropagateRequestedRegion()
{
  // If we need to update due to PipelineMTime, or the fact that our
  // data was released, then propagate the update region to the source
  // if there is one.
  if ( m_UpdateMTime < m_PipelineMTime || m_DataReleased
       || this->RequestedRegionIsOutsideOfTheBufferedRegion() )
    {
    if ( m_Source )
      {
      m_Source->PropagateRequestedRegion(this);
      }
    }

  // Check that the requested region lies within the largest possible region
  if ( !this->VerifyRequestedRegion() )
    {
    // invalid requested region, throw an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(this);

    throw e;
    // return;
    }
}

//----------------------------------------------------------------------------
void
DataObject
::UpdateOutputData()
{
  // If we need to update due to PipelineMTime, or the fact that our
  // data was released, then propagate the UpdateOutputData to the source
  // if there is one.
  if ( m_UpdateMTime < m_PipelineMTime || m_DataReleased
       || this->RequestedRegionIsOutsideOfTheBufferedRegion() )
    {
    if ( m_Source )
      {
      m_Source->UpdateOutputData(this);
      }
    }
}

//----------------------------------------------------------------------------
void
DataObject
::DataHasBeenGenerated()
{
  this->m_DataReleased = 0;
  this->Modified();
  this->m_UpdateMTime.Modified();
}

//----------------------------------------------------------------------------
ModifiedTimeType
DataObject
::GetUpdateMTime() const
{
  return m_UpdateMTime.GetMTime();
}

} // end namespace itk
