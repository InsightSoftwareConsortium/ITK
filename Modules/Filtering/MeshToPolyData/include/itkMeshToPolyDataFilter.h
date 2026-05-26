/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkPolyData.h"

#include <type_traits>

namespace itk
{

template <typename TInputType>
class HasCellTraits
{
  typedef char Yes[1];
  typedef char No[2];
  template <typename CellType>
  static Yes &
  test(typename CellType::CellTraits *); // selected if CellType is a class type
  template <typename CellType>
  static No &
  test(...); // selected otherwise
public:
  static const bool value = sizeof(test<TInputType>(0)) == sizeof(Yes);
};

/** \class MeshToPolyDataFilter
 *
 * \brief Convert a PointSet or Mesh to PolyData
 *
 * Convert an itk::PointSet or itk::Mesh to an itk::PolyData for visualization
 * with vtk.js.
 *
 * Currently support ITK cell types:
 *
 * - itk::VertexCell
 * - itk::LineCell
 * - itk::TriangleCell
 * - itk::QuadrilateralCell
 * - itk::PolygonCell
 *
 * \ingroup MeshToPolyData
 *
 */
template <typename TInputMesh>
class MeshToPolyDataFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeshToPolyDataFilter);

  static constexpr unsigned int PointDimension = TInputMesh::PointDimension;

  using InputMeshType = TInputMesh;

  /** Standard class typedefs. */
  using Self = MeshToPolyDataFilter<InputMeshType>;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using PolyDataType = PolyData<typename InputMeshType::PixelType>;

  /** Run-time type information. */
  itkOverrideGetNameOfClassMacro(MeshToPolyDataFilter);

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Set the mesh input of this process object.  */
  using Superclass::SetInput;
  void
  SetInput(const InputMeshType * input);

  /** Get the mesh input of this process object.  */
  const InputMeshType *
  GetInput() const;

  const InputMeshType *
  GetInput(unsigned int idx) const;

  PolyDataType *
  GetOutput();
  const PolyDataType *
  GetOutput() const;

  PolyDataType *
  GetOutput(unsigned int idx);

protected:
  MeshToPolyDataFilter();
  ~MeshToPolyDataFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  template <typename TInputMeshDispatch,
            typename std::enable_if<!HasCellTraits<TInputMeshDispatch>::value, int>::type = 0>
  void
  GenerateDataDispatch();

  template <typename TInputMeshDispatch,
            typename std::enable_if<HasCellTraits<TInputMeshDispatch>::value, int>::type = 0>
  void
  GenerateDataDispatch();

  ProcessObject::DataObjectPointer
  MakeOutput(ProcessObject::DataObjectPointerArraySizeType idx) override;
  ProcessObject::DataObjectPointer
  MakeOutput(const ProcessObject::DataObjectIdentifierType &) override;

private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeshToPolyDataFilter.hxx"
#endif

#endif // itkMeshToPolyDataFilter
