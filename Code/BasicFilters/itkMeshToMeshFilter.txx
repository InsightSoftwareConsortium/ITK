/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshToMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkMeshToMeshFilter.h"


namespace itk
{
  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
MeshToMeshFilter<TInputMesh,TOutputMesh>
::MeshToMeshFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
MeshToMeshFilter<TInputMesh,TOutputMesh>
::SetInput(TInputMesh *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
MeshToMeshFilter<TInputMesh,TOutputMesh>::InputMeshPointer
MeshToMeshFilter<TInputMesh,TOutputMesh>
::GetInput()
{
  if (this->NumberOfInputs < 1)
    {
    return 0;
    }
  
  return static_cast<TInputMesh*>
                     (this->ProcessObject::GetInput(0).GetPointer());
}

  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
MeshToMeshFilter<TInputMesh,TOutputMesh>::InputMeshPointer
MeshToMeshFilter<TInputMesh,TOutputMesh>
::GetInput(unsigned int idx)
{
  return static_cast<TInputMesh*>
                     (this->ProcessObject::GetInput(idx).GetPointer());
}


/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void 
MeshToMeshFilter<TInputMesh,TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk
