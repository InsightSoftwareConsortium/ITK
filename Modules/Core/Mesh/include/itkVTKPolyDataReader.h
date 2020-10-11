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
#ifndef itkVTKPolyDataReader_h
#define itkVTKPolyDataReader_h

#include "itkIntTypes.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkTriangleCell.h"

namespace itk
{
/** \class VTKPolyDataReader
 * \brief
 * Reads a vtkPolyData file and create an itkMesh.
 *
 * Caveat1: itkVTKPolyDataReader can only read triangle meshes.
 *          Use vtkTriangleFilter to convert your mesh to a triangle mesh.
 * Caviet2: itkVTKPolyDataReader can only read vtk legacy files.
 * Caveat3: itkVTKPolyDataReader cannot read binary vtk files.
 *
 * This class may be deprecated in the future. The MeshFileReader is
 * preferred.
 *
 * \ingroup ITKMesh
 *
 * \sa MeshFileReader
 */
template <typename TOutputMesh>
class ITK_TEMPLATE_EXPORT VTKPolyDataReader : public MeshSource<TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VTKPolyDataReader);

  /** Standard "Self" type alias. */
  using Self = VTKPolyDataReader;
  using Superclass = MeshSource<TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataReader, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  using OutputMeshType = TOutputMesh;
  using MeshTraits = typename OutputMeshType::MeshTraits;
  using PointType = typename OutputMeshType::PointType;
  using PixelType = typename MeshTraits::PixelType;

  /** Some convenient type alias. */
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using CellTraits = typename OutputMeshType::CellTraits;
  using CellIdentifier = typename OutputMeshType::CellIdentifier;
  using CellType = typename OutputMeshType::CellType;
  using CellAutoPointer = typename OutputMeshType::CellAutoPointer;
  using PointIdentifier = typename OutputMeshType::PointIdentifier;
  using PointIdIterator = typename CellTraits::PointIdIterator;

  using PointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using PointsContainer = typename OutputMeshType::PointsContainer;

  /** Define the triangular cell types which form the surface  */
  using TriangleCellType = TriangleCell<CellType>;

  using TriangleCellAutoPointer = typename TriangleCellType::SelfAutoPointer;

  using IndexPairType = std::pair<IdentifierType, IdentifierType>;
  using PointMapType = MapContainer<IndexPairType, IdentifierType>;
  using VectorType = typename PointType::VectorType;

  /** Set the resolution level to be used for generating cells in the
   * Sphere. High values of this parameter will produce sphere with more
   * triangles. */
  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Get the file version line */
  itkGetStringMacro(Version);

  /** Get the file header line */
  itkGetStringMacro(Header);

protected:
  VTKPolyDataReader();
  ~VTKPolyDataReader() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Reads the file */
  void
  GenerateData() override;

  /** Filename to read */

private:
  std::string m_FileName;
  std::string m_Header;
  std::string m_Version;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVTKPolyDataReader.hxx"
#endif

#endif //_itkVTKPolyDataReader_h
