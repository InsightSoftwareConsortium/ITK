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
#ifndef itkMeshToPolyDataFilter_h
#define itkMeshToPolyDataFilter_h

#include "itkProcessObject.h"

namespace itk
{

/** \class MeshToPolyDataFilter
 *
 * \brief Filters a image by iterating over its pixels.
 *
 * Filters a image by iterating over its pixels in a multi-threaded way
 * and {to be completed by the developer}.
 *
 * \ingroup MeshToPolyData
 *
 */
template< typename TInputMesh >
class MeshToPolyDataFilter: public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeshToPolyDataFilter);

  static constexpr unsigned int PointDimension = TInputMesh::PointDimension;

  using InputMeshType = TInputMesh;

  /** Standard class typedefs. */
  using Self = MeshToPolyDataFilter< InputMeshType >;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Run-time type information. */
  itkTypeMacro( MeshToPolyDataFilter, ProcessObject );

  /** Standard New macro. */
  itkNewMacro( Self );

  /** Set the mesh input of this process object.  */
  using Superclass::SetInput;
  void SetInput(const InputMeshType *input);

  /** Get the mesh input of this process object.  */
  const InputMeshType * GetInput() const;

  const InputMeshType * GetInput(unsigned int idx) const;

protected:
  MeshToPolyDataFilter();
  ~MeshToPolyDataFilter() override {}

  void PrintSelf( std::ostream& os, Indent indent ) const override;

  void GenerateData() override;

private:

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshToPolyDataFilter.hxx"
#endif

#endif // itkMeshToPolyDataFilter
