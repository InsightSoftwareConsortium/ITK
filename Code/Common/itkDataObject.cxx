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

itkDataObject::Pointer itkDataObject::New()
{
  return itkDataObject::Pointer(new itkDataObject);
}

// Initialize static member that controls global data release 
// after use by filter
static bool itkDataObjectGlobalReleaseDataFlag = false;

//----------------------------------------------------------------------------
itkDataObject::itkDataObject()
{
  m_Dimension = 0; //unspecified

  m_Source = 0;

  // We have to assume that if a user is creating the data on their own,
  // then they will fill it with valid data.
  m_DataReleased = false;

  m_ReleaseDataFlag = false;

  // The extent is uninitialized
  m_WholeExtent = 0;
  m_Extent = 0;
  m_UpdateExtent = 0;

  // If we used pieces instead of 3D extent, then assume this object was
  // created by the user and this is piece 0 of 1 pieces.
  m_Piece          =  0;
  m_NumberOfPieces =  1;
  m_UpdatePiece          =   0;
  m_UpdateNumberOfPieces =   1;
  m_MaximumNumberOfPieces = 1;

  m_PipelineMTime = 0;
}

//----------------------------------------------------------------------------
itkDataObject::~itkDataObject()
{
  if ( m_WholeExtent )
    {
    delete [] m_WholeExtent;
    m_WholeExtent = NULL;
    delete [] m_Extent;
    m_Extent = NULL;
    delete [] m_UpdateExtent;
    m_UpdateExtent = NULL;
    }
}


//----------------------------------------------------------------------------
void itkDataObject::Initialize()
{
// We don't modify ourselves because the "ReleaseData" methods depend upon
// no modification when initialized.
//
}

//----------------------------------------------------------------------------
void itkDataObject::SetDimension(unsigned int dim)
{
  if ( m_WholeExtent )
    {
    delete [] m_WholeExtent;
    delete [] m_Extent;
    delete [] m_UpdateExtent;
    }
  m_WholeExtent = new int [dim*2];
  m_Extent = new int [dim*2];
  m_UpdateExtent = new int [dim*2];
  
  m_Dimension = dim;
}

//----------------------------------------------------------------------------
void itkDataObject::SetGlobalReleaseDataFlag(bool val)
{
  if (val == itkDataObjectGlobalReleaseDataFlag)
    {
    return;
    }
  itkDataObjectGlobalReleaseDataFlag = val;
}

//----------------------------------------------------------------------------
bool itkDataObject::GetGlobalReleaseDataFlag()
{
  return itkDataObjectGlobalReleaseDataFlag;
}

//----------------------------------------------------------------------------
void itkDataObject::ReleaseData()
{
  this->Initialize();
  m_DataReleased = true;
}

//----------------------------------------------------------------------------
bool itkDataObject::ShouldIReleaseData() const
{
  if ( itkDataObjectGlobalReleaseDataFlag || m_ReleaseDataFlag )
    {
    return true;
    }
  else
    {
    return false;
    }
}

//----------------------------------------------------------------------------

void itkDataObject::SetSource(itkProcessObject *arg)
{
  itkDebugMacro( << this->GetClassName() << " (" 
                 << this << "): setting Source to " << arg ); 

  if (m_Source != arg) 
    {
    itkProcessObject *tmp = m_Source;
    m_Source = arg; 
    if (m_Source != 0) 
      { 
      m_Source->Register(); 
      } 
    if (tmp != 0) 
      { 
      tmp->UnRegister(); 
      }
    this->Modified(); 
    } 
}


//----------------------------------------------------------------------------
void itkDataObject::UnRegister()
{
  // detect the circular loop source <-> data
  // If we have two references and one of them is my data
  // and I am not being unregistered by my data, break the loop.
  int refCount = this->GetReferenceCount();
  if (refCount == 2 && m_Source != 0 &&
      m_Source->InRegisterLoop(this))
    {
    this->SetSource(0);
    }
  
  this->itkObject::UnRegister();
}

//----------------------------------------------------------------------------
void itkDataObject::PrintSelf(std::ostream& os, itkIndent indent)
{
  int i;
        
  itkObject::PrintSelf(os,indent);

  os << indent << "Dimension: " << m_Dimension << std::endl;

  if ( m_Source )
    {
    os << indent << "Source: " << m_Source << "\n";
    }
  else
    {
    os << indent << "Source: (none)\n";
    }

  os << indent << "Release Data: " 
     << (m_ReleaseDataFlag ? "On\n" : "Off\n");

  os << indent << "Data Released: " 
     << (m_DataReleased ? "True\n" : "False\n");
  
  os << indent << "Global Release Data: " 
     << (itkDataObjectGlobalReleaseDataFlag ? "On\n" : "Off\n");

  os << indent << "PipelineMTime: " << m_PipelineMTime << std::endl;
  os << indent << "UpdateTime: " << m_UpdateTime << std::endl;
  
  os << indent << "Update Number Of Pieces: " << m_UpdateNumberOfPieces << std::endl;
  os << indent << "Update Piece: " << m_UpdatePiece << std::endl;
  os << indent << "Maximum Number Of Pieces: " << m_MaximumNumberOfPieces << std::endl;

  os << indent << "UpdateExtent: ( ";
  for (i=0; i<m_Dimension; i++)
    {
    os << m_UpdateExtent[2*i] << "," << m_UpdateExtent[2*i+1] << " " << std::endl;
    }
  os << " )";

  os << indent << "WholeExtent: ( ";
  for (i=0; i<m_Dimension; i++)
    {
    os << m_WholeExtent[2*i] << "," << m_WholeExtent[2*i+1] << " " << std::endl;
    }
  os << " )";

  os << indent << "LastUpdateExtentWasOutsideOfTheExtent: " << 
    m_LastUpdateExtentWasOutsideOfTheExtent << std::endl;
}

// The follwing methods are used for updating the data processing pipeline.
//

//----------------------------------------------------------------------------
void itkDataObject::Update()
{
  this->UpdateInformation();
  this->PropagateUpdateExtent();
  this->TriggerAsynchronousUpdate();
  this->UpdateData();
}

//----------------------------------------------------------------------------
void itkDataObject::UpdateInformation()
{
  if (m_Source)
    {
    m_Source->UpdateInformation();
    }
  // If we don't have a source, then let's make our whole
  // extent equal to our extent. 
  else
    {
    memcpy( m_WholeExtent, m_Extent, m_Dimension*2*sizeof(int) );
    }
  
  // Now we should know what our whole extent is. If our update extent
  // was not set yet, (or has been set to something invalid - with no 
  // data in it ) then set it to the whole extent.
  switch ( this->GetExtentType() )
    {
    case ITK_UNSTRUCTURED_EXTENT:
      if ( m_UpdatePiece == -1 && m_UpdateNumberOfPieces == 0 )
        {
        this->SetUpdateExtentToWholeExtent();
        }
      break;
      
    case ITK_STRUCTURED_EXTENT:
      for (int i=0; i<m_Dimension; i++)
        {
        if ( m_UpdateExtent[2*i+1] < m_UpdateExtent[2*i] )
          {
          this->SetUpdateExtentToWholeExtent();
          }
        break; //out of for loop
        }
      break;
    }
  
  m_LastUpdateExtentWasOutsideOfTheExtent = 0;
}

//----------------------------------------------------------------------------
void itkDataObject::PropagateUpdateExtent()
{
  // If we need to update due to PipelineMTime, or the fact that our
  // data was released, then propagate the update extent to the source 
  // if there is one.
  if ( m_UpdateTime < m_PipelineMTime || m_DataReleased ||
       this->UpdateExtentIsOutsideOfTheExtent() || 
       m_LastUpdateExtentWasOutsideOfTheExtent)
    {
    if (m_Source)
      {
      m_Source->PropagateUpdateExtent(this);
      }
    }
  
  // update the value of this ivar
  m_LastUpdateExtentWasOutsideOfTheExtent = 
    this->UpdateExtentIsOutsideOfTheExtent();
  
  // Check that the update extent lies within the whole extent
  if ( ! this->VerifyUpdateExtent() )
    {
    // invalid update piece - this should not occur!
    return;
    }

}

//----------------------------------------------------------------------------
void itkDataObject::TriggerAsynchronousUpdate()
{
  // If we need to update due to PipelineMTime, or the fact that our
  // data was released, then propagate the trigger to the source
  // if there is one.
  if ( m_UpdateTime < m_PipelineMTime || m_DataReleased ||
       this->UpdateExtentIsOutsideOfTheExtent())
    {
    if (m_Source)
      {
      m_Source->TriggerAsynchronousUpdate();
      }
    }
}

//----------------------------------------------------------------------------
void itkDataObject::UpdateData()
{
  // If we need to update due to PipelineMTime, or the fact that our
  // data was released, then propagate the UpdateData to the source
  // if there is one.
  if ( m_UpdateTime < m_PipelineMTime || m_DataReleased ||
       this->UpdateExtentIsOutsideOfTheExtent())
    {
    if (m_Source)
      {
      m_Source->UpdateData(this);
      } 
    } 
}

//----------------------------------------------------------------------------
void itkDataObject::SetUpdateExtentToWholeExtent()
{
  switch ( this->GetExtentType() )
    {
    // Our update extent will be the first piece of one piece (the whole thing)
    case ITK_UNSTRUCTURED_EXTENT:
      m_UpdateNumberOfPieces  = 1;
      m_UpdatePiece           = 0;
      break;

    // Our update extent will be the whole extent
    case ITK_STRUCTURED_EXTENT:
      memcpy( m_UpdateExtent, m_WholeExtent, 6*sizeof(int) );
      break;

    // We should never have this case occur
    default:
      itkErrorMacro( << "Internal error - invalid extent type!" );
      break;
    }
}

//----------------------------------------------------------------------------
void itkDataObject::DataHasBeenGenerated()
{
  m_DataReleased = 0;
  m_UpdateTime.Modified();
}

//----------------------------------------------------------------------------
void itkDataObject::ComputeEstimatedPipelineMemorySize(unsigned long sizes[3])
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
unsigned long itkDataObject::GetEstimatedPipelineMemorySize()
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
unsigned long itkDataObject::GetEstimatedMemorySize()
{
  // This should be implemented in a subclass. If not, default to
  // estimating that no memory is used.
  return 0;
}

//----------------------------------------------------------------------------
unsigned long itkDataObject::GetActualMemorySize()
{
  return 0;
}

//----------------------------------------------------------------------------
void itkDataObject::CopyInformation(itkDataObject *data)
{
  if ( this->GetExtentType() == ITK_STRUCTURED_EXTENT &&
       data->GetExtentType() == ITK_STRUCTURED_EXTENT )
    {
    memcpy( m_WholeExtent, data->GetWholeExtent(), 6*sizeof(int) );
    }
  else if ( this->GetExtentType() == ITK_UNSTRUCTURED_EXTENT &&
	    data->GetExtentType() == ITK_UNSTRUCTURED_EXTENT )
    {
    m_MaximumNumberOfPieces = data->GetMaximumNumberOfPieces();
    }  
}


bool itkDataObject::UpdateExtentIsOutsideOfTheExtent()
{
  int i;

  switch ( this->GetExtentType() )
    {
    case ITK_UNSTRUCTURED_EXTENT:
      if ( m_UpdatePiece != m_Piece ||
           m_UpdateNumberOfPieces != m_NumberOfPieces )
        {
        return true;
        }
      break;

    case ITK_STRUCTURED_EXTENT:
      for (i=0; i<m_Dimension; i++)
        {
        if ( m_UpdateExtent[2*i] < m_Extent[2*i] ||
             m_UpdateExtent[2*i+1] > m_Extent[2*i+1] )
          {
          return true;
          }
        }
      break;

    // We should never have this case occur
    default:
      itkErrorMacro( << "Internal error - invalid extent type!" );
      break;
    }
  return false;
}

bool itkDataObject::VerifyUpdateExtent()
{
  bool retval = true;
  int i;

  switch ( this->GetExtentType() )
    {
    // Are we asking for more pieces than we can get?
    case ITK_UNSTRUCTURED_EXTENT:
      if ( m_UpdateNumberOfPieces > m_MaximumNumberOfPieces )
        {
        itkErrorMacro( << "Cannot break object into " <<
                       m_UpdateNumberOfPieces << ". The limit is " <<
                       m_MaximumNumberOfPieces );
        retval = false;
        }

      if ( m_UpdatePiece >= m_UpdateNumberOfPieces ||
           m_UpdatePiece < 0 )
        {
          itkErrorMacro( << "Invalid update piece " << m_UpdatePiece
                         << ". Must be between 0 and " 
                         << m_UpdateNumberOfPieces - 1);
        retval = false;
        }
      break;

    // Is our update extent within the whole extent?
    case ITK_STRUCTURED_EXTENT:
      for (i=0; i<m_Dimension; i++)
        {
        if ( m_UpdateExtent[2*i] < m_WholeExtent[2*i] ||
             m_UpdateExtent[2*i+1] > m_WholeExtent[2*i+1] )
          {
          itkErrorMacro( << "Update extent does not lie within whole extent" );
          retval = false;
          }
        }
      break;

    // We should never have this case occur
    default:
      itkErrorMacro( << "Internal error - invalid extent type!" );
      break;
    }

  return retval;
}
