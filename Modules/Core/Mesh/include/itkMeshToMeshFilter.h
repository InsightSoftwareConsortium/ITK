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
#ifndef itkMeshToMeshFilter_h
#define itkMeshToMeshFilter_h

#include "itkMeshSource.h"

namespace itk
{
/** \class MeshToMeshFilter
 * \brief
 *
 * MeshToMeshFilter is the base class for all process objects that output
 * mesh data, and require mesh data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * \ingroup MeshFilters
 *
 * \ingroup ITKMesh
 */
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT MeshToMeshFilter:public MeshSource< TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef MeshToMeshFilter           Self;
  typedef MeshSource< TOutputMesh >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshToMeshFilter, MeshSource);

  /** Some convenient typedefs. */
  typedef TInputMesh                       InputMeshType;
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef TOutputMesh                      OutputMeshType;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  /** Set the mesh input of this process object.  */
  using Superclass::SetInput;
  void SetInput(const InputMeshType *input);

  /** Get the mesh input of this process object.  */
  const InputMeshType * GetInput() const;

  const InputMeshType * GetInput(unsigned int idx) const;

protected:
  MeshToMeshFilter();
  ~MeshToMeshFilter() ITK_OVERRIDE {}

  void CopyInputMeshToOutputMeshPoints();

  void CopyInputMeshToOutputMeshPointData();

  void CopyInputMeshToOutputMeshCellLinks();

  void CopyInputMeshToOutputMeshCells();

  void CopyInputMeshToOutputMeshCellData();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeshToMeshFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshToMeshFilter.hxx"
#endif

#endif
