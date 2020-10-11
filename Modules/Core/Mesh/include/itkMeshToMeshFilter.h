/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT MeshToMeshFilter : public MeshSource<TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeshToMeshFilter);

  /** Standard class type aliases. */
  using Self = MeshToMeshFilter;
  using Superclass = MeshSource<TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshToMeshFilter, MeshSource);

  /** Some convenient type alias. */
  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  /** Set the mesh input of this process object.  */
  using Superclass::SetInput;
  void
  SetInput(const InputMeshType * input);

  /** Get the mesh input of this process object.  */
  const InputMeshType *
  GetInput() const;

  const InputMeshType *
  GetInput(unsigned int idx) const;

protected:
  MeshToMeshFilter();
  ~MeshToMeshFilter() override = default;

  void
  CopyInputMeshToOutputMeshPoints();

  void
  CopyInputMeshToOutputMeshPointData();

  void
  CopyInputMeshToOutputMeshCellLinks();

  void
  CopyInputMeshToOutputMeshCells();

  void
  CopyInputMeshToOutputMeshCellData();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeshToMeshFilter.hxx"
#endif

#endif
