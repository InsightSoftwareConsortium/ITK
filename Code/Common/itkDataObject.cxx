/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDataObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointerForwardReference.txx"

// Manual instantiation is necessary to prevent link errors
template class itk::SmartPointerForwardReference<itk::ProcessObject>;

namespace itk
{
  
// after use by filter
bool DataObject::m_GlobalReleaseDataFlag = false;

//----------------------------------------------------------------------------
DataObject::
DataObject()
{
  m_Source = 0;
  m_SourceOutputIndex = 0;

  // We have to assume that if a user is creating the data on their own,
  // then they will fill it with valid data.
  m_DataReleased = false;

  m_ReleaseDataFlag = false;

  m_PipelineMTime = 0;
  
  m_RequestedRegionInitialized = false;
}

//----------------------------------------------------------------------------
DataObject
::~DataObject()
{
}


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
  if (val == m_GlobalReleaseDataFlag)
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
  if ( m_GlobalReleaseDataFlag || m_ReleaseDataFlag )
    {
    return true;
    }
  else
    {
    return false;
    }
}

//----------------------------------------------------------------------------
// Set the process object that generates this data object.
//
void 
DataObject
::DisconnectPipeline() const
{
  itkDebugMacro( << this->GetClassName() << " (" 
                 << this << "): disconnecting from the pipeline." );

  // disconnect ourselves from the current process object
  if (m_Source)
    {
    m_Source->SetNthOutput(m_SourceOutputIndex, 0);
    }

  this->Modified(); 
}


void
DataObject
::DisconnectSource(ProcessObject *arg, unsigned int idx) const
{
  itkDebugMacro( << this->GetClassName() << " (" 
                 << this << "): disconnecting source  " << arg
                 << ", source output index " << idx);

  if ( m_Source == arg && m_SourceOutputIndex == idx)
    {
    m_Source = 0;
    m_SourceOutputIndex = 0;
    this->Modified();
    }
}

void
DataObject
::ConnectSource(ProcessObject *arg, unsigned int idx) const
{
  itkDebugMacro( << this->GetClassName() << " (" 
                 << this << "): connecting source  " << arg
                 << ", source output index " << idx);

  if ( m_Source != arg || m_SourceOutputIndex != idx)
    {
    m_Source = arg;
    m_SourceOutputIndex = idx;
    this->Modified();
    }
}


//----------------------------------------------------------------------------

SmartPointerForwardReference<ProcessObject>
DataObject
::GetSource() const
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Source address " << m_Source );
  return m_Source.GetPointer();
}

unsigned int
DataObject
::GetSourceOutputIndex() const
{
  itkDebugMacro(<< this->GetClassName() << " (" << this
                << "): returning Source index " << m_SourceOutputIndex );
  return m_SourceOutputIndex;
}


//----------------------------------------------------------------------------
void 
DataObject
::PrintSelf(std::ostream& os, Indent indent)
{
  Object::PrintSelf(os,indent);

  if ( m_Source )
    {
    os << indent << "Source: " << m_Source << "\n";
    os << indent << "Source output index: " << m_SourceOutputIndex << "\n";
    }
  else
    {
    os << indent << "Source: (none)\n";
    os << indent << "Source output index:  0\n";
    }

  os << indent << "Release Data: " 
     << (m_ReleaseDataFlag ? "On\n" : "Off\n");

  os << indent << "Data Released: " 
     << (m_DataReleased ? "True\n" : "False\n");
  
  os << indent << "Global Release Data: " 
     << (m_GlobalReleaseDataFlag ? "On\n" : "Off\n");

  os << indent << "PipelineMTime: " << m_PipelineMTime << std::endl;
  os << indent << "UpdateTime: " << m_UpdateTime << std::endl;
  
  os << indent << "LastRequestedRegionWasOutsideOfTheBufferedRegion: " << 
    m_LastRequestedRegionWasOutsideOfTheBufferedRegion << std::endl;
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

//----------------------------------------------------------------------------
void 
DataObject
::PropagateRequestedRegion()
{
  // If we need to update due to PipelineMTime, or the fact that our
  // data was released, then propagate the update region to the source 
  // if there is one.
  if ( m_UpdateTime < m_PipelineMTime || m_DataReleased ||
       this->RequestedRegionIsOutsideOfTheBufferedRegion() || 
       m_LastRequestedRegionWasOutsideOfTheBufferedRegion)
    {

    if ( m_Source )
      {
      m_Source->PropagateRequestedRegion(this);
      }
    }
  
  // update the value of this ivar
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 
    this->RequestedRegionIsOutsideOfTheBufferedRegion();
  
  // Check that the requested region lies within the largest possible region
  if ( ! this->VerifyRequestedRegion() )
    {
    // invalid requested region - this should not occur!
    return;
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
  if ( m_UpdateTime < m_PipelineMTime || m_DataReleased ||
       this->RequestedRegionIsOutsideOfTheBufferedRegion())
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
  m_DataReleased = 0;
  m_UpdateTime.Modified();
}

//----------------------------------------------------------------------------
void 
DataObject
::ComputeEstimatedPipelineMemorySize(unsigned long sizes[3])
{
  if ( m_Source )
    {
    m_Source->ComputeEstimatedPipelineMemorySize( this, sizes );
    } 
  else
    {
    unsigned long size = this->GetActualMemorySize();
    sizes[0] = size;
    sizes[1] = size;
    sizes[2] = size;
    }
}

//----------------------------------------------------------------------------
unsigned long 
DataObject
::GetEstimatedPipelineMemorySize()
{
  unsigned long sizes[3];
  unsigned long memorySize = 0;

  if ( m_Source )
    {
    m_Source->ComputeEstimatedPipelineMemorySize( this, sizes );
    memorySize = sizes[2];
    } 

  return memorySize;
}


//----------------------------------------------------------------------------
unsigned long 
DataObject
::GetEstimatedMemorySize()
{
  // This should be implemented in a subclass. If not, default to
  // estimating that no memory is used.
  return 0;
}

//----------------------------------------------------------------------------
unsigned long 
DataObject
::GetActualMemorySize()
{
  return 0;
}


} // end namespace itk
