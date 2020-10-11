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

template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT ConnectedRegionsMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConnectedRegionsMeshFilter);

  /**
   * Standard class type aliases.
   */
  using Self = ConnectedRegionsMeshFilter;

  /**
   * Standard "Superclass" type alias.
   */
  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;

  /**
   * Smart pointer type alias support
   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Method for creation through the object factory.
   */

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConnectedRegionsMeshFilter, MeshToMeshFilter);

  /**
   * Convenient type alias for this filter.
   */
  using InputMeshType = TInputMesh;
  using OutputMeshType = TOutputMesh;
  using InputMeshConstPointer = typename TInputMesh::ConstPointer;
  using OutputMeshPointer = typename TOutputMesh::Pointer;

  static constexpr unsigned int PointDimension = TInputMesh::PointDimension;

  using InputMeshPointType = typename TInputMesh::PointType;
  using InputMeshPointIdentifier = typename TInputMesh::PointIdentifier;
  using InputMeshPointsContainerConstPointer = typename TInputMesh::PointsContainerConstPointer;
  using InputMeshCellsContainer = typename TInputMesh::CellsContainer;
  using InputMeshCellsContainerPointer = typename TInputMesh::CellsContainerPointer;
  using InputMeshCellsContainerConstPointer = typename TInputMesh::CellsContainerConstPointer;
  using InputMeshCellDataContainer = typename TInputMesh::CellDataContainer;
  using InputMeshCellDataContainerPointer = typename TInputMesh::CellDataContainerPointer;
  using InputMeshCellDataContainerConstPointer = typename TInputMesh::CellDataContainerConstPointer;
  using PointsContainerConstIterator = typename InputMeshType::PointsContainer::ConstIterator;
  using CellsContainerConstIterator = typename InputMeshType::CellsContainer::ConstIterator;
  using CellDataContainerConstIterator = typename InputMeshType::CellDataContainer::ConstIterator;
  using InputMeshCellPointer = typename TInputMesh::CellAutoPointer;
  using InputMeshPointIdConstIterator = typename TInputMesh::CellTraits::PointIdConstIterator;
  using InputMeshCellLinksContainerConstPointer = typename TInputMesh::CellLinksContainerConstPointer;
  using InputMeshCellLinksContainer = typename TInputMesh::PointCellLinksContainer;
  using InputMeshCellIdentifier = typename TInputMesh::CellIdentifier;

  /**
   * Different modes of operation. Use these to specify
   * how to extract the regions.
   */
  enum
  {
    PointSeededRegions = 0,
    CellSeededRegions = 1,
    SpecifiedRegions = 2,
    LargestRegion = 3,
    AllRegions = 4,
    ClosestPointRegion = 5
  };

  /**
   * Methods specify mode of operation for the filter. Note that
   * some modes require additional information. For example,
   * SetExtractionModeToClosestPointRegion() also requires that
   * a point be defined.
   */
  itkSetMacro(ExtractionMode, int);
  itkGetConstMacro(ExtractionMode, int);

  void
  SetExtractionModeToPointSeededRegions()
  {
    this->SetExtractionMode(Self::PointSeededRegions);
  }

  void
  SetExtractionModeToCellSeededRegions()
  {
    this->SetExtractionMode(Self::CellSeededRegions);
  }

  void
  SetExtractionModeToSpecifiedRegions()
  {
    this->SetExtractionMode(Self::SpecifiedRegions);
  }

  void
  SetExtractionModeToLargestRegion()
  {
    this->SetExtractionMode(Self::LargestRegion);
  }

  void
  SetExtractionModeToAllRegions()
  {
    this->SetExtractionMode(Self::AllRegions);
  }

  void
  SetExtractionModeToClosestPointRegion()
  {
    this->SetExtractionMode(Self::ClosestPointRegion);
  }

  /**
   * Initialize list of point ids/cell ids used to seed regions.
   */
  void
  InitializeSeedList()
  {
    this->Modified();
    m_SeedList.clear();
  }

  /**
   * Add a seed id (point or cell id). Note: ids are 0-offset.
   */
  void
  AddSeed(IdentifierType id)
  {
    this->Modified();
    m_SeedList.push_back(id);
  }

  /**
   * Delete a seed id (point or cell id). Note: ids are 0-offset.
   */
  void
  DeleteSeed(IdentifierType id);

  /**
   * Initialize list of region ids to extract.
   */
  void
  InitializeSpecifiedRegionList()
  {
    this->Modified();
    m_RegionList.clear();
  }

  /**
   * Add a region id to extract. Note: ids are 0-offset.
   */
  void
  AddSpecifiedRegion(IdentifierType id)
  {
    this->Modified();
    m_RegionList.push_back(id);
  }

  /**
   * Delete a region id to extract. Note: ids are 0-offset.
   */
  void
  DeleteSpecifiedRegion(IdentifierType id);

  /**
   * Use to specify x-y-z point coordinates when extracting the region
   * closest to a specified point.
   */
  void
  SetClosestPoint(InputMeshPointType & p)
  {
    if (m_ClosestPoint != p)
    {
      m_ClosestPoint = p;
      this->Modified();
    }
  }

  InputMeshPointType &
  GetClosestPoint(InputMeshPointType &)
  {
    return m_ClosestPoint;
  }

  /**
   * Obtain the number of connected regions.
   */
  SizeValueType
  GetNumberOfExtractedRegions()
  {
    return m_RegionList.size();
  }

protected:
  ConnectedRegionsMeshFilter();
  ~ConnectedRegionsMeshFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  void
  PropagateConnectedWave();

private:
  int                         m_ExtractionMode;
  InputMeshPointType          m_ClosestPoint;
  std::vector<IdentifierType> m_SeedList;
  std::vector<IdentifierType> m_RegionList;
  std::vector<SizeValueType>  m_RegionSizes;

  std::vector<OffsetValueType>  m_Visited;
  SizeValueType                 m_NumberOfCellsInRegion;
  IdentifierType                m_RegionNumber;
  std::vector<IdentifierType> * m_Wave{ nullptr };
  std::vector<IdentifierType> * m_Wave2{ nullptr };
}; // class declaration
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConnectedRegionsMeshFilter.hxx"
#endif

#endif
