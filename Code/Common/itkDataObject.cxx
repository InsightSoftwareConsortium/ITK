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
  m_Dimension = -1; //unspecified

  m_Source = 0;

  // We have to assume that if a user is creating the data on their own,
  // then they will fill it with valid data.
  m_DataReleased = false;

  m_ReleaseDataFlag = true;

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
}


//----------------------------------------------------------------------------
void itkDataObject::Initialize()
{
// We don't modify ourselves because the "ReleaseData" methods depend upon
// no modification when initialized.
//
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
bool itkDataObject::ShouldIReleaseData()
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

  os << indent << "Dimension: " << m_Dimension << endl;

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

  os << indent << "PipelineMTime: " << m_PipelineMTime << endl;
  os << indent << "UpdateTime: " << m_UpdateTime << endl;
  
  os << indent << "Update Number Of Pieces: " << m_UpdateNumberOfPieces << endl;
  os << indent << "Update Piece: " << m_UpdatePiece << endl;
  os << indent << "Maximum Number Of Pieces: " << m_MaximumNumberOfPieces << endl;

  os << indent << "UpdateExtent: ( ";
  for (i=0; i<m_Dimension; i++)
    {
    os << m_UpdateExtent[2*i] << "," << m_UpdateExtent[2*i+1] << " " << endl;
    }
  os << " )";

  os << indent << "WholeExtent: ( ";
  for (i=0; i<m_Dimension; i++)
    {
    os << m_WholeExtent[2*i] << "," << m_WholeExtent[2*i+1] << " " << endl;
    }
  os << " )";

}
