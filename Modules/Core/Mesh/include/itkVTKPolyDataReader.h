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
template< typename TOutputMesh >
class ITK_TEMPLATE_EXPORT VTKPolyDataReader:public MeshSource< TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef VTKPolyDataReader          Self;
  typedef MeshSource< TOutputMesh >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VTKPolyDataReader, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  typedef TOutputMesh                         OutputMeshType;
  typedef typename OutputMeshType::MeshTraits MeshTraits;
  typedef typename OutputMeshType::PointType  PointType;
  typedef typename MeshTraits::PixelType      PixelType;

  /** Some convenient typedefs. */
  typedef typename OutputMeshType::Pointer         OutputMeshPointer;
  typedef typename OutputMeshType::CellTraits      CellTraits;
  typedef typename OutputMeshType::CellIdentifier  CellIdentifier;
  typedef typename OutputMeshType::CellType        CellType;
  typedef typename OutputMeshType::CellAutoPointer CellAutoPointer;
  typedef typename OutputMeshType::PointIdentifier PointIdentifier;
  typedef typename CellTraits::PointIdIterator     PointIdIterator;

  typedef typename OutputMeshType::PointsContainerPointer PointsContainerPointer;
  typedef typename OutputMeshType::PointsContainer        PointsContainer;

  /** Define the triangular cell types which form the surface  */
  typedef TriangleCell< CellType > TriangleCellType;

  typedef typename TriangleCellType::SelfAutoPointer TriangleCellAutoPointer;

  typedef std::pair< IdentifierType, IdentifierType >    IndexPairType;
  typedef MapContainer< IndexPairType, IdentifierType >  PointMapType;
  typedef typename PointType::VectorType                 VectorType;

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
  ~VTKPolyDataReader() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Reads the file */
  void GenerateData() ITK_OVERRIDE;

  /** Filename to read */

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VTKPolyDataReader);

  std::string m_FileName;
  std::string m_Header;
  std::string m_Version;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVTKPolyDataReader.hxx"
#endif

#endif //_itkVTKPolyDataReader_h
