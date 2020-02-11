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
#ifndef itkSimplexMeshToTriangleMeshFilter_h
#define itkSimplexMeshToTriangleMeshFilter_h

#include "itkPolygonCell.h"

#include "itkSimplexMesh.h"
#include "itkMeshToMeshFilter.h"
#include "itkVectorContainer.h"
#include "itkAutomaticTopologyMeshSource.h"

namespace itk
{
/**  \class SimplexMeshToTriangleMeshFilter
 * \brief This filter converts a 2-simplex mesh into a triangle mesh
 *
 * Convert a simplex mesh into a triangle mesh. Therefore the center of each
 * simplex cell is computed. These centers are taken as the points for the
 * triangle mesh then the points are connected.
 *
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT SimplexMeshToTriangleMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SimplexMeshToTriangleMeshFilter);

  /** Standard "Self" type alias. */
  using Self = SimplexMeshToTriangleMeshFilter;

  /** Standard "Superclass" type alias. */
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimplexMeshToTriangleMeshFilter, MeshToMeshFilter);

  using InputMeshType = TInputMesh;
  using InputMeshConstPointer = typename InputMeshType::ConstPointer;
  using InputPointType = typename InputMeshType::PointType;
  using InputPixelType = typename InputMeshType::PixelType;
  using InputCellTraitsType = typename InputMeshType::MeshTraits::CellTraits;
  using PointIdentifier = typename InputMeshType::PointIdentifier;
  using CellIdentifier = typename InputMeshType::CellIdentifier;

  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerPointer = typename InputPointsContainer::Pointer;
  using InputPointsContainerIterator = typename InputPointsContainer::Iterator;

  using InputNeighbors = typename InputMeshType::NeighborListType;
  using InputNeighborsIterator = typename InputMeshType::NeighborListType::iterator;

  using AutoMeshSourceType = itk::AutomaticTopologyMeshSource<TOutputMesh>;

  using SimplexCellType = typename InputMeshType::CellType;
  using SimplexPolygonType = itk::PolygonCell<SimplexCellType>;

  // stores the center for each simplex mesh cell, key is the point id
  using PointMapType = itk::MapContainer<PointIdentifier, InputPointType>;
  using PointMapPointer = typename PointMapType::Pointer;

  /** \class SimplexCellVisitor
   * This class provides methods for visiting
   * each simplex cell of a simplex mesh
   * It computes the center of each visited cell.
   * \ingroup ITKMesh
   */
  class SimplexCellVisitor
  {
  public:
    /**
     * default constructor
     */
    SimplexCellVisitor() { m_CenterMap = PointMapType::New(); }

    /**
     * default destructor
     */
    virtual ~SimplexCellVisitor() = default;

    /**
     * \brief visits all polygon cells and compute the cell centers
     */
    void
    Visit(CellIdentifier cellId, SimplexPolygonType * poly)
    {
      using PointIdIterator = typename SimplexPolygonType::PointIdIterator;
      PointIdIterator it = poly->PointIdsBegin();
      InputPointType  center;
      center.Fill(0);

      InputPointType p;
      p.Fill(0);

      while (it != poly->PointIdsEnd())
      {
        this->m_Mesh->GetPoint(*it, &p);
        center += p.GetVectorFromOrigin();
        it++;
      }

      center[0] /= poly->GetNumberOfPoints();
      center[1] /= poly->GetNumberOfPoints();
      center[2] /= poly->GetNumberOfPoints();

      m_CenterMap->InsertElement(cellId, center);
    }

    PointMapPointer
    GetCenterMap()
    {
      return m_CenterMap;
    }

    void
    SetMesh(const InputMeshType * mesh)
    {
      this->m_Mesh = mesh;
    }

  protected:
    InputMeshConstPointer m_Mesh;
    PointMapPointer       m_CenterMap;
  };

  using SimplexVisitorInterfaceType = itk::
    CellInterfaceVisitorImplementation<InputPixelType, InputCellTraitsType, SimplexPolygonType, SimplexCellVisitor>;

  using SimplexVisitorInterfacePointer = typename SimplexVisitorInterfaceType::Pointer;
  using CellMultiVisitorType = typename SimplexCellType::MultiVisitor;
  using CellMultiVisitorPointer = typename CellMultiVisitorType::Pointer;

protected:
  SimplexMeshToTriangleMeshFilter() = default;
  ~SimplexMeshToTriangleMeshFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  void
  Initialize();

  /** creates dual triangles for all simplex cells */
  void
  CreateTriangles();

  /** part of algorithm */
  CellIdentifier
  FindCellId(CellIdentifier id1, CellIdentifier id2, CellIdentifier id3);

  /** attribute stores the result of the simplex cell visitor */
  PointMapPointer m_Centers;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSimplexMeshToTriangleMeshFilter.hxx"
#endif

#endif //__SimplexMeshToTriangleMeshFilter_h
