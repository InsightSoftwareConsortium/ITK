/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedRegionsMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConnectedRegionsMeshFilter_h
#define __itkConnectedRegionsMeshFilter_h

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
 */

template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT ConnectedRegionsMeshFilter :
    public MeshToMeshFilter<TInputMesh,TOutputMesh> 
{
public:
  /**
   * Standard class typedefs.
   */
  typedef ConnectedRegionsMeshFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh>   Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  //@{
  /**
   * Convenient typedefs for this filter.
   */
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename TInputMesh::Pointer InputMeshPointer;
  typedef typename TOutputMesh::Pointer OutputMeshPointer;
  itkStaticConstMacro(PointDimension, unsigned int,TInputMesh::PointDimension);
  typedef typename TInputMesh::PointType InputMeshPointType;
  typedef typename TInputMesh::PointIdentifier InputMeshPointIdentifier;
  typedef typename TInputMesh::PointsContainerPointer 
  InputMeshPointsContainerPointer;
  typedef typename TInputMesh::CellsContainer InputMeshCellsContainer;
  typedef typename TInputMesh::CellsContainerPointer 
  InputMeshCellsContainerPointer;
  typedef typename TInputMesh::CellDataContainer InputMeshCellDataContainer;
  typedef typename TInputMesh::CellDataContainerPointer 
  InputMeshCellDataContainerPointer;
  typedef typename InputMeshType::PointsContainer::ConstIterator 
  PointsContainerConstIterator;
  typedef typename InputMeshType::CellsContainer::ConstIterator
  CellsContainerConstIterator;
  typedef typename InputMeshType::CellDataContainer::ConstIterator
  CellDataContainerConstIterator;
  typedef typename TInputMesh::CellAutoPointer InputMeshCellPointer;
  typedef typename TInputMesh::CellTraits::PointIdConstIterator 
  InputMeshPointIdConstIterator;
  typedef typename TInputMesh::CellLinksContainerPointer
  InputMeshCellLinksContainerPointer;
  typedef typename TInputMesh::PointCellLinksContainer
  InputMeshCellLinksContainer;
  typedef typename TInputMesh::CellIdentifier InputMeshCellIdentifier;
  //@}

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
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
  itkSetMacro(ExtractionMode,int);
  itkGetMacro(ExtractionMode,int);
  void SetExtractionModeToPointSeededRegions ()
  {this->SetExtractionMode(Self::PointSeededRegions);}
  void SetExtractionModeToCellSeededRegions ()
  {this->SetExtractionMode(Self::CellSeededRegions);}
  void SetExtractionModeToSpecifiedRegions ()
  {this->SetExtractionMode(Self::SpecifiedRegions);}
  void SetExtractionModeToLargestRegion ()
  {this->SetExtractionMode(Self::LargestRegion);}
  void SetExtractionModeToAllRegions ()
  {this->SetExtractionMode(Self::AllRegions);}
  void SetExtractionModeToClosestPointRegion ()
  {this->SetExtractionMode(Self::ClosestPointRegion);}

  /**
   * Initialize list of point ids/cell ids used to seed regions.
   */
  void InitializeSeedList()
  {this->Modified(); m_SeedList.clear();}

  /**
   * Add a seed id (point or cell id). Note: ids are 0-offset.
   */
  void AddSeed(unsigned long id)
  {this->Modified(); m_SeedList.push_back(id);}

  /**
   * Delete a seed id (point or cell id). Note: ids are 0-offset.
   */
  void DeleteSeed(unsigned long id);

  /**
   * Initialize list of region ids to extract.
   */
  void InitializeSpecifiedRegionList()
  {this->Modified(); m_RegionList.clear();}

  /**
   * Add a region id to extract. Note: ids are 0-offset.
   */
  void AddSpecifiedRegion(unsigned long id)
  {this->Modified(); m_RegionList.push_back(id);}

  /**
   * Delete a region id to extract. Note: ids are 0-offset.
   */
  void DeleteSpecifiedRegion(unsigned long id);

  /**
   * Use to specify x-y-z point coordinates when extracting the region 
   * closest to a specified point.
   */
  void SetClosestPoint(InputMeshPointType& p)
  {
    if ( m_ClosestPoint != p )
      {
      m_ClosestPoint = p;
      this->Modified();
      }
  }
  InputMeshPointType& GetClosestPoint(InputMeshPointType& p)
  {return m_ClosestPoint;}

  /**
   * Obtain the number of connected regions.
   */
  unsigned long GetNumberOfExtractedRegions();

protected:
  ConnectedRegionsMeshFilter();
  virtual ~ConnectedRegionsMeshFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateData();
  void PropagateConnectedWave();

private:  
  ConnectedRegionsMeshFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  int                        m_ExtractionMode;
  InputMeshPointType         m_ClosestPoint;
  std::vector<unsigned long> m_SeedList;
  std::vector<unsigned long> m_RegionList;
  std::vector<unsigned long> m_RegionSizes;
  
  std::vector<long>          m_Visited;
  unsigned long              m_NumberOfCellsInRegion;
  unsigned long              m_RegionNumber;
  std::vector<unsigned long> *m_Wave;
  std::vector<unsigned long> *m_Wave2;
  
}; // class declaration

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedRegionsMeshFilter.txx"
#endif

#endif
