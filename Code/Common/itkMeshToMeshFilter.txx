/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshToMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMeshToMeshFilter_txx
#define _itkMeshToMeshFilter_txx

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
typename MeshToMeshFilter<TInputMesh,TOutputMesh>::InputMeshType *
MeshToMeshFilter<TInputMesh,TOutputMesh>
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputMesh*>
    (this->ProcessObject::GetInput(0));
}

  
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
typename MeshToMeshFilter<TInputMesh,TOutputMesh>::InputMeshType *
MeshToMeshFilter<TInputMesh,TOutputMesh>
::GetInput(unsigned int idx)
{
  return static_cast<TInputMesh*>
    (this->ProcessObject::GetInput(idx));
}


} // end namespace itk

#endif
