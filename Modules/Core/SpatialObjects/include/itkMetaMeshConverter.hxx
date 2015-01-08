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
/** Constructor */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
MetaMeshConverter< NDimensions, PixelType, TMeshTraits >
::MetaMeshConverter()
{}

template < unsigned int NDimensions, typename PixelType, typename TMeshTraits >
typename MetaMeshConverter< NDimensions, PixelType, TMeshTraits >::MetaObjectType *
MetaMeshConverter< NDimensions, PixelType, TMeshTraits >
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new MeshMetaObjectType);
}

/** Convert a metaMesh into an Mesh SpatialObject  */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
typename MetaMeshConverter< NDimensions, PixelType, TMeshTraits >::SpatialObjectPointer
MetaMeshConverter< NDimensions, PixelType, TMeshTraits >
::MetaObjectToSpatialObject(const MetaObjectType *mo)
{
  const MeshMetaObjectType *_mesh =
    dynamic_cast<const MeshMetaObjectType *>(mo);
  if(_mesh == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaMesh");
    }

  typename MeshSpatialObjectType::Pointer meshSO = MeshSpatialObjectType::New();

  double spacing[NDimensions];

  unsigned int ndims = _mesh->NDims();
  for ( unsigned int i = 0; i < ndims; i++ )
    {
    spacing[i] = _mesh->ElementSpacing()[i];
    }
  meshSO->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  meshSO->GetProperty()->SetName( _mesh->Name() );
  meshSO->SetId( _mesh->ID() );
  meshSO->SetParentId( _mesh->ParentID() );
  meshSO->GetProperty()->SetRed(_mesh->Color()[0]);
  meshSO->GetProperty()->SetGreen(_mesh->Color()[1]);
  meshSO->GetProperty()->SetBlue(_mesh->Color()[2]);
  meshSO->GetProperty()->SetAlpha(_mesh->Color()[3]);

  // Create a new Mesh
  typename MeshType::Pointer mesh = MeshType::New();

  // Add Points
  typedef typename MeshMetaObjectType::PointListType PointListType;
  const PointListType points = _mesh->GetPoints();
  typename PointListType::const_iterator it_points = points.begin();

  while ( it_points != points.end() )
    {
    typename MeshType::PointType pt;
    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      pt[i] = ( ( *it_points )->m_X )[i];
      }
    mesh->SetPoint( ( *it_points )->m_Id, pt );
    it_points++;
    }

  // Add Cells
  typedef typename MeshType::CellType        CellType;
  typedef typename CellType::CellAutoPointer CellAutoPointer;
  mesh->SetCellsAllocationMethod(
    MeshType::CellsAllocatedDynamicallyCellByCell);

  for ( unsigned int celltype = 0; celltype < MET_NUM_CELL_TYPES; celltype++ )
    {
    typedef typename MetaMesh::CellListType CellListType;
    const CellListType cells = _mesh->GetCells( (MET_CellGeometry)celltype );
    typename CellListType::const_iterator it_cells = cells.begin();

    typedef typename MeshType::CellType                 CellInterfaceType;
    typedef itk::VertexCell< CellInterfaceType >        VertexCellType;
    typedef itk::LineCell< CellInterfaceType >          LineCellType;
    typedef itk::TriangleCell< CellInterfaceType >      TriangleCellType;
    typedef itk::QuadrilateralCell< CellInterfaceType > QuadrilateralCellType;
    typedef itk::PolygonCell< CellInterfaceType >       PolygonCellType;
    typedef itk::TetrahedronCell< CellInterfaceType >   TetraCellType;
    typedef itk::HexahedronCell< CellInterfaceType >    HexahedronCellType;
    typedef itk::QuadraticEdgeCell< CellInterfaceType > QuadraticEdgeCellType;
    typedef itk::QuadraticTriangleCell< CellInterfaceType >
    QuadraticTriangleCellType;

    while ( it_cells != cells.end() )
      {
      CellAutoPointer cell;

      switch ( (MET_CellGeometry)celltype )
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

      for ( unsigned int i = 0; i < MET_CellSize[celltype]; i++ )
        {
        cell->SetPointId(i, ( *it_cells )->m_PointsId[i]);
        }

      mesh->SetCell( ( *it_cells )->m_Id, cell );
      it_cells++;
      }
    }

  // Add cell links
  typedef typename MetaMesh::CellLinkListType CellLinkListType;
  const CellLinkListType links = _mesh->GetCellLinks();
  typename CellLinkListType::const_iterator it_links = links.begin();

  typedef typename MeshType::CellLinksContainer CellLinksContainerType;
  typename CellLinksContainerType::Pointer linkContainer =
    CellLinksContainerType::New();

  while ( it_links != links.end() )
    {
    typename MeshType::PointCellLinksContainer pcl;

    std::list< int >::const_iterator it_link = ( *it_links )->m_Links.begin();
    while ( it_link != ( *it_links )->m_Links.end() )
      {
      pcl.insert(*it_link);
      it_link++;
      }
    linkContainer->InsertElement( ( *it_links )->m_Id, pcl );
    it_links++;
    }

  mesh->SetCellLinks(linkContainer);

  // Add point data
  typedef typename MeshType::PointDataContainer PointDataContainer;
  typename PointDataContainer::Pointer pointData = PointDataContainer::New();

  typedef MetaMesh::PointDataListType PointDataListType;
  PointDataListType::const_iterator it_pd = _mesh->GetPointData().begin();

  while ( it_pd != _mesh->GetPointData().end() )
    {
    pointData->InsertElement( ( *it_pd )->m_Id,
                              static_cast< MeshData< PixelType > * >( *it_pd )->m_Data );
    it_pd++;
    }
  mesh->SetPointData(pointData);

  // Add cell data
  typedef typename MeshType::CellDataContainer CellDataContainer;
  typename CellDataContainer::Pointer cellData = CellDataContainer::New();

  typedef MetaMesh::CellDataListType CellDataListType;
  CellDataListType::const_iterator it_cd = _mesh->GetCellData().begin();
  while ( it_cd != _mesh->GetCellData().end() )
    {
    typedef typename MeshType::CellPixelType CellPixelType;
    cellData->InsertElement( ( *it_cd )->m_Id,
                             static_cast< MeshData< CellPixelType > * >( *it_cd )->m_Data );
    it_cd++;
    }

  mesh->SetCellData(cellData);

  // Add the mesh
  meshSO->SetMesh(mesh);
  return meshSO.GetPointer();
}

/** Convert a Mesh SpatialObject into a metaMesh */
template< unsigned int NDimensions, typename PixelType, typename TMeshTraits >
typename MetaMeshConverter< NDimensions, PixelType, TMeshTraits >::MetaObjectType *
MetaMeshConverter< NDimensions, PixelType, TMeshTraits >
::SpatialObjectToMetaObject(const SpatialObjectType *so)
{
  const MeshSpatialObjectConstPointer meshSO =
    dynamic_cast<const MeshSpatialObjectType *>(so);

  if(meshSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to MeshSpatialObject");
    }
  MeshMetaObjectType *metamesh = new MeshMetaObjectType(NDimensions);

  typename MeshType::ConstPointer mesh = meshSO->GetMesh();

  if ( !mesh )
    {
    std::cout << "MetaMeshConverter : GetMesh() returned a ITK_NULLPTR Pointer"
              << std::endl;
    return ITK_NULLPTR;
    }

  // fill in the Mesh information
  metamesh->ID( meshSO->GetId() );

  // Add Points
  typedef typename MeshType::PointsContainer PointsContainer;
  const PointsContainer *points = mesh->GetPoints();
  typename MeshType::PointsContainer::ConstIterator it_points = points->Begin();

  while ( it_points != points->End() )
    {
    MeshPoint *pnt = new MeshPoint(NDimensions);
    for ( unsigned int i = 0; i < NDimensions; i++ )
      {
      pnt->m_X[i] = ( *it_points )->Value()[i];
      }
    pnt->m_Id = ( *it_points )->Index();
    metamesh->GetPoints().push_back(pnt);
    it_points++;
    }

  // Add Cells
  typedef typename MeshType::CellsContainer CellsContainer;
  const CellsContainer *cells = mesh->GetCells();
  typename MeshType::CellsContainer::ConstIterator it_cells = cells->Begin();

  while ( it_cells != cells->End() )
    {
    unsigned int celldim = ( *it_cells )->Value()->GetNumberOfPoints();
    MeshCell *   cell = new MeshCell(celldim);

    typename MeshType::CellTraits::PointIdConstIterator
    itptids = ( *it_cells )->Value()->GetPointIds();
    unsigned int i = 0;
    while ( itptids != ( *it_cells )->Value()->PointIdsEnd() )
      {
      cell->m_PointsId[i++] = *itptids;
      itptids++;
      }
    cell->m_Id = ( *it_cells )->Index();

    typename MeshType::MeshTraits::CellType::CellGeometry
    geom = ( *it_cells )->Value()->GetType();
    typedef typename MeshType::MeshTraits::CellType CellType;

    switch ( geom )
      {
      case CellType::VERTEX_CELL:
        metamesh->GetCells(MET_VERTEX_CELL).push_back(cell);
        break;
      case CellType::LINE_CELL:
        metamesh->GetCells(MET_LINE_CELL).push_back(cell);
        break;
      case CellType::TRIANGLE_CELL:
        metamesh->GetCells(MET_TRIANGLE_CELL).push_back(cell);
        break;
      case CellType::QUADRILATERAL_CELL:
        metamesh->GetCells(MET_QUADRILATERAL_CELL).push_back(cell);
        break;
      case CellType::POLYGON_CELL:
        metamesh->GetCells(MET_POLYGON_CELL).push_back(cell);
        break;
      case CellType::TETRAHEDRON_CELL:
        metamesh->GetCells(MET_TETRAHEDRON_CELL).push_back(cell);
        break;
      case CellType::HEXAHEDRON_CELL:
        metamesh->GetCells(MET_HEXAHEDRON_CELL).push_back(cell);
        break;
      case CellType::QUADRATIC_EDGE_CELL:
        metamesh->GetCells(MET_QUADRATIC_EDGE_CELL).push_back(cell);
        break;
      case CellType::QUADRATIC_TRIANGLE_CELL:
        metamesh->GetCells(MET_QUADRATIC_TRIANGLE_CELL).push_back(cell);
        break;
      default:
        metamesh->GetCells(MET_VERTEX_CELL).push_back(cell);
      }
    it_cells++;
    }

  // Add cell links
  typedef typename MeshType::CellLinksContainer CellLinksContainer;
  const CellLinksContainer *links = mesh->GetCellLinks();

  if ( links )
    {
    typename MeshType::CellLinksContainer::ConstIterator
    it_celllinks = links->Begin();

    while ( it_celllinks != links->End() )
      {
      MeshCellLink *link = new MeshCellLink();
      link->m_Id = ( *it_celllinks )->Index();

      typename MeshType::PointCellLinksContainer::const_iterator
      it = ( *it_celllinks )->Value().begin();
      while ( it != ( *it_celllinks )->Value().end() )
        {
        link->m_Links.push_back(*it);
        it++;
        }
      metamesh->GetCellLinks().push_back(link);
      it_celllinks++;
      }
    }

  // Add point data
  metamesh->PointDataType( MET_GetPixelType( typeid( PixelType ) ) );

  typedef typename MeshType::PointDataContainer PointDataContainer;
  const PointDataContainer *pd = mesh->GetPointData();
  if ( pd )
    {
    typename MeshType::PointDataContainer::ConstIterator it_pd = pd->Begin();
    while ( it_pd != pd->End() )
      {
      MeshData< PixelType > *data = new MeshData< PixelType >();
      data->m_Id = ( *it_pd )->Index();
      data->m_Data = ( *it_pd )->Value();
      metamesh->GetPointData().push_back(data);
      it_pd++;
      }
    }

  // Add cell data
  typedef typename TMeshTraits::CellPixelType CellPixelType;
  metamesh->CellDataType( MET_GetPixelType( typeid( CellPixelType ) ) );

  typedef typename MeshType::CellDataContainer CellDataContainer;
  const CellDataContainer *cd = mesh->GetCellData();
  if ( cd )
    {
    typename MeshType::CellDataContainer::ConstIterator it_cd = cd->Begin();

    while ( it_cd != cd->End() )
      {
      MeshData< CellPixelType > *data = new MeshData< CellPixelType >();
      data->m_Id = ( *it_cd )->Index();
      data->m_Data = ( *it_cd )->Value();
      metamesh->GetCellData().push_back(data);
      it_cd++;
      }
    }
  return metamesh;
}

} // end namespace itk

#endif
