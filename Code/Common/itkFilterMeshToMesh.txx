/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterMeshToMesh.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFilterMeshToMesh.h"


ITK_NAMESPACE_BEGIN
  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
FilterMeshToMesh<TInputMesh,TOutputMesh>
::FilterMeshToMesh()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
FilterMeshToMesh<TInputMesh,TOutputMesh>
::SetInput(TInputMesh *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
TInputMesh *
FilterMeshToMesh<TInputMesh,TOutputMesh>
::GetInput()
{
  if (this->NumberOfInputs < 1)
    {
    return 0;
    }
  
  return static_cast<TInputMesh *>(this->GetInput(0));
}

  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
TInputMesh *
FilterMeshToMesh<TInputMesh,TOutputMesh>
::GetInput(unsigned int idx)
{
  return static_cast<TInputMesh *>(this->ProcessObject::GetInput(idx));
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
FilterMeshToMesh<TInputMesh,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent)
{
  MeshSource<TOutputMesh>::PrintSelf(os,indent);
}

ITK_NAMESPACE_END
