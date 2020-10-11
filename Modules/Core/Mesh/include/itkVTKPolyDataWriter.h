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
#ifndef itkVTKPolyDataWriter_h
#define itkVTKPolyDataWriter_h

#include "itkIntTypes.h"
#include "itkMesh.h"
#include "itkTriangleCell.h"

namespace itk
{
/** \class VTKPolyDataWriter
 * \brief
 * Writes an itkMesh to a file in VTK file format.
 *
 * Caveat: The input to itkVTKPolyDataWriter must be a triangle mesh.
 *         Use vtkTriangleFilter to convert your mesh to a triangle mesh.
 *
 * This class may be deprecated in the future. The MeshFileWriter is
 * preferred.
 *
 * \ingroup ITKMesh
 *
 * \sa MeshFileWriter
 *
 * \sphinx
 * \sphinxexample{Core/Mesh/WorkingWithPointAndCellData,Write Mesh To VTP}
 * \endsphinx
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT VTKPolyDataWriter : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTKPolyDataWriter);

  /** Standard "Self" type alias. */
  using Self = VTKPolyDataWriter;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataWriter, Object);

  /** Write the Input mesh to the Output file.
   * Use either Update() or Write(). */
  void
  Update();

  void
  Write();

  /** Hold on to the type information specified by the template parameters.
   */
  using InputMeshType = TInputMesh;
  using PixelType = typename InputMeshType::PixelType;
  using PointType = typename InputMeshType::PointType;
  using CellType = typename InputMeshType::CellType;
  using PointIdentifier = typename InputMeshType::PointIdentifier;

  /** Some convenient type alias. */
  using InputMeshPointer = typename InputMeshType::ConstPointer;
  using CellTraits = typename InputMeshType::CellTraits;

  /** Define the triangular cell types which form the surface  */
  using CellInterfaceType = CellInterface<PixelType, CellTraits>;
  using TriangleCellType = TriangleCell<CellInterfaceType>;

  using PointsContainer = typename InputMeshType::PointsContainer;
  using CellsContainer = typename InputMeshType::CellsContainer;

  using PointIterator = typename PointsContainer::ConstIterator;
  using CellIterator = typename CellsContainer::ConstIterator;

  using PointIdIterator = typename CellType::PointIdIterator;

  /** Set the Input */
  void
  SetInput(const InputMeshType * input);

  /** Set/Get the name of the file where data are written. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

protected:
  VTKPolyDataWriter();
  ~VTKPolyDataWriter() override = default;

  virtual void
  GenerateData();

  std::string m_FileName;

  InputMeshPointer m_Input;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVTKPolyDataWriter.hxx"
#endif

#endif
