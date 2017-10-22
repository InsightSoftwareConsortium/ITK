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
template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT SimplexMeshToTriangleMeshFilter:public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard "Self" typedef. */
  typedef SimplexMeshToTriangleMeshFilter Self;

  /** Standard "Superclass" typedef. */
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimplexMeshToTriangleMeshFilter, MeshToMeshFilter);

  typedef TInputMesh                                     InputMeshType;
  typedef typename InputMeshType::ConstPointer           InputMeshConstPointer;
  typedef typename InputMeshType::PointType              InputPointType;
  typedef typename InputMeshType::PixelType              InputPixelType;
  typedef typename InputMeshType::MeshTraits::CellTraits InputCellTraitsType;
  typedef typename InputMeshType::PointIdentifier        PointIdentifier;
  typedef typename InputMeshType::CellIdentifier         CellIdentifier;

  typedef typename InputMeshType::PointsContainer InputPointsContainer;
  typedef typename InputPointsContainer::Pointer  InputPointsContainerPointer;
  typedef typename InputPointsContainer::Iterator InputPointsContainerIterator;

  typedef typename InputMeshType::NeighborListType           InputNeighbors;
  typedef typename InputMeshType::NeighborListType::iterator InputNeighborsIterator;

  typedef          itk::AutomaticTopologyMeshSource< TOutputMesh > AutoMeshSourceType;

  typedef typename InputMeshType::CellType             SimplexCellType;
  typedef          itk::PolygonCell< SimplexCellType > SimplexPolygonType;

  // stores the center for each simplex mesh cell, key is the point id
  typedef          itk::MapContainer< PointIdentifier, InputPointType > PointMapType;
  typedef typename PointMapType::Pointer                                PointMapPointer;

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
    SimplexCellVisitor()
    {
      m_CenterMap = PointMapType::New();
    }

    /**
     * default destructor
     */
    virtual ~SimplexCellVisitor() {}

    /**
     * \brief visits all polygon cells and compute the cell centers
     */
    void Visit(CellIdentifier cellId, SimplexPolygonType *poly)
    {
      typedef typename SimplexPolygonType::PointIdIterator PointIdIterator;
      PointIdIterator it =  poly->PointIdsBegin();
      InputPointType  center;
      center.Fill(0);

      InputPointType p;
      p.Fill(0);

      while ( it != poly->PointIdsEnd() )
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

    PointMapPointer GetCenterMap()
    {
      return m_CenterMap;
    }

    void SetMesh(const InputMeshType *mesh)
    {
      this->m_Mesh = mesh;
    }

protected:
    InputMeshConstPointer m_Mesh;
    PointMapPointer       m_CenterMap;
  };

  typedef itk::CellInterfaceVisitorImplementation< InputPixelType,
                                                   InputCellTraitsType,
                                                   SimplexPolygonType,
                                                   SimplexCellVisitor >
  SimplexVisitorInterfaceType;

  typedef typename SimplexVisitorInterfaceType::Pointer SimplexVisitorInterfacePointer;
  typedef typename SimplexCellType::MultiVisitor        CellMultiVisitorType;
  typedef typename CellMultiVisitorType::Pointer        CellMultiVisitorPointer;

protected:

  SimplexMeshToTriangleMeshFilter();
  virtual ~SimplexMeshToTriangleMeshFilter() ITK_OVERRIDE;
  SimplexMeshToTriangleMeshFilter(const Self &) {}
  void operator=(const Self &) {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  void Initialize();

  /** creates dual triangles for all simplex cells */
  void CreateTriangles();

  /** part of algorithm */
  CellIdentifier FindCellId(CellIdentifier id1, CellIdentifier id2, CellIdentifier id3);

  /** attribute stores the result of the simplex cell visitor */
  PointMapPointer m_Centers;
};
} //end of namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimplexMeshToTriangleMeshFilter.hxx"
#endif

#endif //__SimplexMeshToTriangleMeshFilter_h
