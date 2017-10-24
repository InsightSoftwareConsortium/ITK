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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkConnectedRegionsMeshFilter_h
#define itkConnectedRegionsMeshFilter_h

#include "itkIntTypes.h"
#include "itkMeshToMeshFilter.h"

namespace itk
{
/** \class ConnectedRegionsMeshFilter
 * \brief Extract portions of a mesh that are connected at vertices.
 *
 * ConnectedRegionsMeshFilter will extract portions of a mesh that
 * are connected at vertices. (Such connected portions of the mesh
 * are referred to as a region.) Options exist to extract the largest
 * region, a particular region, a region containing a specified
 * point, or a region containing a specified cell.
 *
 * \ingroup MeshFilters
 * \ingroup ITKMesh
 */

template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT ConnectedRegionsMeshFilter:
  public MeshToMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /**
   * Standard class typedefs.
   */
  typedef ConnectedRegionsMeshFilter Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshToMeshFilter< TInputMesh, TOutputMesh > Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Convenient typedefs for this filter.
   */
  typedef TInputMesh                        InputMeshType;
  typedef TOutputMesh                       OutputMeshType;
  typedef typename TInputMesh::ConstPointer InputMeshConstPointer;
  typedef typename TOutputMesh::Pointer     OutputMeshPointer;

  itkStaticConstMacro(PointDimension, unsigned int,
                      TInputMesh::PointDimension);

  typedef typename TInputMesh::PointType                           InputMeshPointType;
  typedef typename TInputMesh::PointIdentifier                     InputMeshPointIdentifier;
  typedef typename TInputMesh::PointsContainerConstPointer         InputMeshPointsContainerConstPointer;
  typedef typename TInputMesh::CellsContainer                      InputMeshCellsContainer;
  typedef typename TInputMesh::CellsContainerPointer               InputMeshCellsContainerPointer;
  typedef typename TInputMesh::CellsContainerConstPointer          InputMeshCellsContainerConstPointer;
  typedef typename TInputMesh::CellDataContainer                   InputMeshCellDataContainer;
  typedef typename TInputMesh::CellDataContainerPointer            InputMeshCellDataContainerPointer;
  typedef typename TInputMesh::CellDataContainerConstPointer       InputMeshCellDataContainerConstPointer;
  typedef typename InputMeshType::PointsContainer::ConstIterator   PointsContainerConstIterator;
  typedef typename InputMeshType::CellsContainer::ConstIterator    CellsContainerConstIterator;
  typedef typename InputMeshType::CellDataContainer::ConstIterator CellDataContainerConstIterator;
  typedef typename TInputMesh::CellAutoPointer                     InputMeshCellPointer;
  typedef typename TInputMesh::CellTraits::PointIdConstIterator    InputMeshPointIdConstIterator;
  typedef typename TInputMesh::CellLinksContainerConstPointer      InputMeshCellLinksContainerConstPointer;
  typedef typename TInputMesh::PointCellLinksContainer             InputMeshCellLinksContainer;
  typedef typename TInputMesh::CellIdentifier                      InputMeshCellIdentifier;

  /**
   * Different modes of operation. Use these to specify
   * how to extract the regions.
   */
  enum { PointSeededRegions = 0,
         CellSeededRegions = 1,
         SpecifiedRegions = 2,
         LargestRegion = 3,
         AllRegions = 4,
         ClosestPointRegion = 5 };

  /**
   * Methods specify mode of operation for the filter. Note that
   * some modes require additional information. For example,
   * SetExtractionModeToClosestPointRegion() also requires that
   * a point be defined.
   */
  itkSetMacro(ExtractionMode, int);
  itkGetConstMacro(ExtractionMode, int);

  void SetExtractionModeToPointSeededRegions(void)
  {
    this->SetExtractionMode(Self::PointSeededRegions);
  }

  void SetExtractionModeToCellSeededRegions(void)
  {
    this->SetExtractionMode(Self::CellSeededRegions);
  }

  void SetExtractionModeToSpecifiedRegions(void)
  {
    this->SetExtractionMode(Self::SpecifiedRegions);
  }

  void SetExtractionModeToLargestRegion(void)
  {
    this->SetExtractionMode(Self::LargestRegion);
  }

  void SetExtractionModeToAllRegions(void)
  {
    this->SetExtractionMode(Self::AllRegions);
  }

  void SetExtractionModeToClosestPointRegion(void)
  {
    this->SetExtractionMode(Self::ClosestPointRegion);
  }

  /**
   * Initialize list of point ids/cell ids used to seed regions.
   */
  void InitializeSeedList(void)
  {
    this->Modified();
    m_SeedList.clear();
  }

  /**
   * Add a seed id (point or cell id). Note: ids are 0-offset.
   */
  void AddSeed(IdentifierType id)
  {
    this->Modified();
    m_SeedList.push_back(id);
  }

  /**
   * Delete a seed id (point or cell id). Note: ids are 0-offset.
   */
  void DeleteSeed(IdentifierType id);

  /**
   * Initialize list of region ids to extract.
   */
  void InitializeSpecifiedRegionList(void)
  {
    this->Modified();
    m_RegionList.clear();
  }

  /**
   * Add a region id to extract. Note: ids are 0-offset.
   */
  void AddSpecifiedRegion(IdentifierType id)
  {
    this->Modified();
    m_RegionList.push_back(id);
  }

  /**
   * Delete a region id to extract. Note: ids are 0-offset.
   */
  void DeleteSpecifiedRegion(IdentifierType id);

  /**
   * Use to specify x-y-z point coordinates when extracting the region
   * closest to a specified point.
   */
  void SetClosestPoint(InputMeshPointType & p)
  {
    if ( m_ClosestPoint != p )
      {
      m_ClosestPoint = p;
      this->Modified();
      }
  }

  InputMeshPointType & GetClosestPoint(InputMeshPointType &)
  {
    return m_ClosestPoint;
  }

  /**
   * Obtain the number of connected regions.
   */
  SizeValueType GetNumberOfExtractedRegions()
  {
    return m_RegionList.size();
  }

protected:

  ConnectedRegionsMeshFilter();
  virtual ~ConnectedRegionsMeshFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  void PropagateConnectedWave();

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(ConnectedRegionsMeshFilter);

  int                            m_ExtractionMode;
  InputMeshPointType             m_ClosestPoint;
  std::vector< IdentifierType >  m_SeedList;
  std::vector< IdentifierType >  m_RegionList;
  std::vector< SizeValueType >   m_RegionSizes;

  std::vector< OffsetValueType > m_Visited;
  SizeValueType                  m_NumberOfCellsInRegion;
  IdentifierType                 m_RegionNumber;
  std::vector< IdentifierType > *m_Wave;
  std::vector< IdentifierType > *m_Wave2;
}; // class declaration
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedRegionsMeshFilter.hxx"
#endif

#endif
