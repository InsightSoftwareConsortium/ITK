/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkMeshSource_txx
#define _itkMeshSource_txx

#include "itkMeshSource.h"

namespace itk
{

/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>
::MeshSource()
{
  /**
   * Create the output
   */
  typename TOutputMesh::Pointer output = TOutputMesh::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

  m_GenerateDataRegion = 0;
  m_GenerateDataNumberOfRegions = 0;
}


/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>::OutputMeshPointer
MeshSource<TOutputMesh>
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TOutputMesh*>
                     (this->ProcessObject::GetOutput(0).GetPointer());
}

  
/**
 *
 */
template<class TOutputMesh>
MeshSource<TOutputMesh>::OutputMeshPointer
MeshSource<TOutputMesh>
::GetOutput(unsigned int idx)
{
  return static_cast<TOutputMesh*>
                     (this->ProcessObject::GetOutput(idx).GetPointer());
}


/**
 *
 */
template<class TOutputMesh>
void 
MeshSource<TOutputMesh>
::SetOutput(TOutputMesh *output)
{
  this->ProcessObject::SetNthOutput(0, output);
}


/**
 *
 */
template<class TOutputMesh>
void 
MeshSource<TOutputMesh>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
}


/**
 *
 */
template<class TOutputMesh>
void 
MeshSource<TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
