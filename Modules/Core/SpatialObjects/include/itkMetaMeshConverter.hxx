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
#ifndef itkMetaMeshConverter_hxx
#define itkMetaMeshConverter_hxx

#include "itkMetaMeshConverter.h"
#include "itkTetrahedronCell.h"
#include "itkPolygonCell.h"
#include "itkHexahedronCell.h"
#include "itkQuadraticTriangleCell.h"
#include <list>

namespace itk
{

template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
typename MetaMeshConverter<NDimensions, PixelType, TMeshTraits>::MetaObjectType *
MetaMeshConverter<NDimensions, PixelType, TMeshTraits>::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new MeshMetaObjectType);
}

/** Convert a metaMesh into an Mesh SpatialObject  */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
typename MetaMeshConverter<NDimensions, PixelType, TMeshTraits>::SpatialObjectPointer
MetaMeshConverter<NDimensions, PixelType, TMeshTraits>::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const auto * _mesh = dynamic_cast<const MeshMetaObjectType *>(mo);
  if (_mesh == nullptr)
  {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaMesh");
  }

  typename MeshSpatialObjectType::Pointer meshSO = MeshSpatialObjectType::New();

  meshSO->GetProperty().SetName(_mesh->Name());
  meshSO->SetId(_mesh->ID());
  meshSO->SetParentId(_mesh->ParentID());
  meshSO->GetProperty().SetRed(_mesh->Color()[0]);
  meshSO->GetProperty().SetGreen(_mesh->Color()[1]);
  meshSO->GetProperty().SetBlue(_mesh->Color()[2]);
  meshSO->GetProperty().SetAlpha(_mesh->Color()[3]);

  // Create a new Mesh
  typename MeshType::Pointer mesh = MeshType::New();

  // Add Points
  using PointListType = typename MeshMetaObjectType::PointListType;
  const PointListType points = _mesh->GetPoints();
  auto                it_points = points.begin();

  while (it_points != points.end())
  {
    typename MeshType::PointType pt;
    for (unsigned int i = 0; i < NDimensions; i++)
    {
      pt[i] = ((*it_points)->m_X)[i] * _mesh->ElementSpacing(i);
    }
    mesh->SetPoint((*it_points)->m_Id, pt);
    it_points++;
  }

  // Add Cells
  using CellType = typename MeshType::CellType;
  using CellAutoPointer = typename CellType::CellAutoPointer;
  mesh->SetCellsAllocationMethod(MeshEnums::MeshClassCellsAllocationMethod::CellsAllocatedDynamicallyCellByCell);

  for (unsigned int celltype = 0; celltype < MET_NUM_CELL_TYPES; celltype++)
  {
    using CellListType = typename MetaMesh::CellListType;
    const CellListType cells = _mesh->GetCells((MET_CellGeometry)celltype);
    auto               it_cells = cells.begin();

    using CellInterfaceType = typename MeshType::CellType;
    using VertexCellType = itk::VertexCell<CellInterfaceType>;
    using LineCellType = itk::LineCell<CellInterfaceType>;
    using TriangleCellType = itk::TriangleCell<CellInterfaceType>;
    using QuadrilateralCellType = itk::QuadrilateralCell<CellInterfaceType>;
    using PolygonCellType = itk::PolygonCell<CellInterfaceType>;
    using TetraCellType = itk::TetrahedronCell<CellInterfaceType>;
    using HexahedronCellType = itk::HexahedronCell<CellInterfaceType>;
    using QuadraticEdgeCellType = itk::QuadraticEdgeCell<CellInterfaceType>;
    using QuadraticTriangleCellType = itk::QuadraticTriangleCell<CellInterfaceType>;

    while (it_cells != cells.end())
    {
      CellAutoPointer cell;

      switch ((MET_CellGeometry)celltype)
      {
        case MET_VERTEX_CELL:
          cell.TakeOwnership(new VertexCellType);
          break;
        case MET_LINE_CELL:
          cell.TakeOwnership(new LineCellType);
          break;
        case MET_TRIANGLE_CELL:
          cell.TakeOwnership(new TriangleCellType);
          break;
        case MET_QUADRILATERAL_CELL:
          cell.TakeOwnership(new QuadrilateralCellType);
          break;
        case MET_POLYGON_CELL:
          cell.TakeOwnership(new PolygonCellType);
          break;
        case MET_TETRAHEDRON_CELL:
          cell.TakeOwnership(new TetraCellType);
          break;
        case MET_HEXAHEDRON_CELL:
          cell.TakeOwnership(new HexahedronCellType);
          break;
        case MET_QUADRATIC_EDGE_CELL:
          cell.TakeOwnership(new QuadraticEdgeCellType);
          break;
        case MET_QUADRATIC_TRIANGLE_CELL:
          cell.TakeOwnership(new QuadraticTriangleCellType);
          break;
        default:
          cell.TakeOwnership(new VertexCellType);
      }

      for (unsigned int i = 0; i < MET_CellSize[celltype]; i++)
      {
        cell->SetPointId(i, (*it_cells)->m_PointsId[i]);
      }

      mesh->SetCell((*it_cells)->m_Id, cell);
      it_cells++;
    }
  }

  // Add cell links
  using CellLinkListType = typename MetaMesh::CellLinkListType;
  const CellLinkListType links = _mesh->GetCellLinks();
  auto                   it_links = links.begin();

  using CellLinksContainerType = typename MeshType::CellLinksContainer;
  typename CellLinksContainerType::Pointer linkContainer = CellLinksContainerType::New();

  while (it_links != links.end())
  {
    typename MeshType::PointCellLinksContainer pcl;

    std::list<int>::const_iterator it_link = (*it_links)->m_Links.begin();
    while (it_link != (*it_links)->m_Links.end())
    {
      pcl.insert(*it_link);
      it_link++;
    }
    linkContainer->InsertElement((*it_links)->m_Id, pcl);
    it_links++;
  }

  mesh->SetCellLinks(linkContainer);

  // Add point data
  using PointDataContainer = typename MeshType::PointDataContainer;
  typename PointDataContainer::Pointer pointData = PointDataContainer::New();

  auto it_pd = _mesh->GetPointData().begin();

  while (it_pd != _mesh->GetPointData().end())
  {
    pointData->InsertElement((*it_pd)->m_Id, static_cast<MeshData<PixelType> *>(*it_pd)->m_Data);
    it_pd++;
  }
  mesh->SetPointData(pointData);

  // Add cell data
  using CellDataContainer = typename MeshType::CellDataContainer;
  typename CellDataContainer::Pointer cellData = CellDataContainer::New();

  auto it_cd = _mesh->GetCellData().begin();
  while (it_cd != _mesh->GetCellData().end())
  {
    using CellPixelType = typename MeshType::CellPixelType;
    cellData->InsertElement((*it_cd)->m_Id, static_cast<MeshData<CellPixelType> *>(*it_cd)->m_Data);
    it_cd++;
  }

  mesh->SetCellData(cellData);

  // Add the mesh
  meshSO->SetMesh(mesh);
  return meshSO.GetPointer();
}

/** Convert a Mesh SpatialObject into a metaMesh */
template <unsigned int NDimensions, typename PixelType, typename TMeshTraits>
typename MetaMeshConverter<NDimensions, PixelType, TMeshTraits>::MetaObjectType *
MetaMeshConverter<NDimensions, PixelType, TMeshTraits>::SpatialObjectToMetaObject(const SpatialObjectType * so)
{
  const MeshSpatialObjectConstPointer meshSO = dynamic_cast<const MeshSpatialObjectType *>(so);

  if (meshSO.IsNull())
  {
    itkExceptionMacro(<< "Can't downcast SpatialObject to MeshSpatialObject");
  }
  auto * metamesh = new MeshMetaObjectType(NDimensions);

  typename MeshType::ConstPointer mesh = meshSO->GetMesh();

  if (!mesh)
  {
    std::cout << "MetaMeshConverter : GetMesh() returned a nullptr Pointer" << std::endl;
    return nullptr;
  }

  // fill in the Mesh information
  metamesh->ID(meshSO->GetId());

  // Add Points
  using PointsContainer = typename MeshType::PointsContainer;
  const PointsContainer *                           points = mesh->GetPoints();
  typename MeshType::PointsContainer::ConstIterator it_points = points->Begin();

  while (it_points != points->End())
  {
    auto * pnt = new MeshPoint(NDimensions);
    for (unsigned int i = 0; i < NDimensions; i++)
    {
      pnt->m_X[i] = (*it_points)->Value()[i];
    }
    pnt->m_Id = (*it_points)->Index();
    metamesh->GetPoints().push_back(pnt);
    ++it_points;
  }

  // Add Cells
  using CellsContainer = typename MeshType::CellsContainer;
  const CellsContainer *                           cells = mesh->GetCells();
  typename MeshType::CellsContainer::ConstIterator it_cells = cells->Begin();

  while (it_cells != cells->End())
  {
    unsigned int celldim = (*it_cells)->Value()->GetNumberOfPoints();
    auto *       cell = new MeshCell(celldim);

    typename MeshType::CellTraits::PointIdConstIterator itptids = (*it_cells)->Value()->GetPointIds();
    unsigned int                                        i = 0;
    while (itptids != (*it_cells)->Value()->PointIdsEnd())
    {
      cell->m_PointsId[i++] = *itptids;
      itptids++;
    }
    cell->m_Id = (*it_cells)->Index();

    CellGeometryEnum geom = (*it_cells)->Value()->GetType();

    switch (geom)
    {
      case CellGeometryEnum::VERTEX_CELL:
        metamesh->GetCells(MET_VERTEX_CELL).push_back(cell);
        break;
      case CellGeometryEnum::LINE_CELL:
        metamesh->GetCells(MET_LINE_CELL).push_back(cell);
        break;
      case CellGeometryEnum::TRIANGLE_CELL:
        metamesh->GetCells(MET_TRIANGLE_CELL).push_back(cell);
        break;
      case CellGeometryEnum::QUADRILATERAL_CELL:
        metamesh->GetCells(MET_QUADRILATERAL_CELL).push_back(cell);
        break;
      case CellGeometryEnum::POLYGON_CELL:
        metamesh->GetCells(MET_POLYGON_CELL).push_back(cell);
        break;
      case CellGeometryEnum::TETRAHEDRON_CELL:
        metamesh->GetCells(MET_TETRAHEDRON_CELL).push_back(cell);
        break;
      case CellGeometryEnum::HEXAHEDRON_CELL:
        metamesh->GetCells(MET_HEXAHEDRON_CELL).push_back(cell);
        break;
      case CellGeometryEnum::QUADRATIC_EDGE_CELL:
        metamesh->GetCells(MET_QUADRATIC_EDGE_CELL).push_back(cell);
        break;
      case CellGeometryEnum::QUADRATIC_TRIANGLE_CELL:
        metamesh->GetCells(MET_QUADRATIC_TRIANGLE_CELL).push_back(cell);
        break;
      default:
        metamesh->GetCells(MET_VERTEX_CELL).push_back(cell);
    }
    ++it_cells;
  }

  // Add cell links
  using CellLinksContainer = typename MeshType::CellLinksContainer;
  const CellLinksContainer * links = mesh->GetCellLinks();

  if (links)
  {
    typename MeshType::CellLinksContainer::ConstIterator it_celllinks = links->Begin();

    while (it_celllinks != links->End())
    {
      auto * link = new MeshCellLink();
      link->m_Id = (*it_celllinks)->Index();

      auto it = (*it_celllinks)->Value().begin();
      while (it != (*it_celllinks)->Value().end())
      {
        link->m_Links.push_back(*it);
        it++;
      }
      metamesh->GetCellLinks().push_back(link);
      ++it_celllinks;
    }
  }

  // Add point data
  metamesh->PointDataType(MET_GetPixelType(typeid(PixelType)));

  using PointDataContainer = typename MeshType::PointDataContainer;
  const PointDataContainer * pd = mesh->GetPointData();
  if (pd)
  {
    typename MeshType::PointDataContainer::ConstIterator it_pd = pd->Begin();
    while (it_pd != pd->End())
    {
      auto * data = new MeshData<PixelType>();
      data->m_Id = (*it_pd)->Index();
      data->m_Data = (*it_pd)->Value();
      metamesh->GetPointData().push_back(data);
      ++it_pd;
    }
  }

  // Add cell data
  using CellPixelType = typename TMeshTraits::CellPixelType;
  metamesh->CellDataType(MET_GetPixelType(typeid(CellPixelType)));

  using CellDataContainer = typename MeshType::CellDataContainer;
  const CellDataContainer * cd = mesh->GetCellData();
  if (cd)
  {
    typename MeshType::CellDataContainer::ConstIterator it_cd = cd->Begin();

    while (it_cd != cd->End())
    {
      auto * data = new MeshData<CellPixelType>();
      data->m_Id = (*it_cd)->Index();
      data->m_Data = (*it_cd)->Value();
      metamesh->GetCellData().push_back(data);
      ++it_cd;
    }
  }
  return metamesh;
}

} // end namespace itk

#endif
