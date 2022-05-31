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
#ifndef itkPolyDataToMeshFilter_h
#define itkPolyDataToMeshFilter_h

#include "itkProcessObject.h"
#include "itkPolyData.h"
#include "itkMesh.h"

#include <type_traits>

namespace itk
{
/** \class PolyDataToMeshFilter
 *
 * \brief Convert PolyData to a Mesh
 *
 * Convert an itk::PolyData to an itk::PointSet or itk::Mesh
 *
 * \ingroup MeshToPolyData
 *
 */
template <typename TInputPolyData>
class PolyDataToMeshFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PolyDataToMeshFilter);

  /** Standard class typedefs. */
  using Self = PolyDataToMeshFilter<TInputPolyData>;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New macro. */
  itkNewMacro(Self);

  /** Run-time type information. */
  itkTypeMacro(PolyDataToMeshFilter, ProcessObject);

  static constexpr unsigned int PointDimension = TInputPolyData::PointDimension;

  using InputPolyDataType = TInputPolyData;
  using PolyDataType = InputPolyDataType;
  using OutputMeshType = Mesh<typename InputPolyDataType::PixelType, PointDimension>;
  using MeshType = OutputMeshType;

  using OutputCoordRepType = typename OutputMeshType::CoordRepType;
  using OutputPointPixelType = typename OutputMeshType::PixelType;
  using OutputCellPixelType = typename OutputMeshType::CellPixelType;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputCellIdentifier = typename OutputMeshType::CellIdentifier;
  using OutputCellAutoPointer = typename OutputMeshType::CellAutoPointer;
  using OutputCellType = typename OutputMeshType::CellType;

  /** Set the polydata input of this process object.  */
  using Superclass::SetInput;
  void
  SetInput(const InputPolyDataType * input);

  /** Get the polydata input of this process object.  */
  const InputPolyDataType *
  GetInput() const;

  const InputPolyDataType *
  GetInput(unsigned int idx) const;

  OutputMeshType *
  GetOutput();
  const OutputMeshType *
  GetOutput() const;

  OutputMeshType *
  GetOutput(unsigned int idx);

  void
  GenerateOutputInformation() override;

protected:
  PolyDataToMeshFilter();
  ~PolyDataToMeshFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  ProcessObject::DataObjectPointer
  MakeOutput(ProcessObject::DataObjectPointerArraySizeType idx) override;
  ProcessObject::DataObjectPointer
  MakeOutput(const ProcessObject::DataObjectIdentifierType &) override;

private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolyDataToMeshFilter.hxx"
#endif

#endif // itkPolyDataToMeshFilter
