/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiDiagram2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkVoronoiDiagram2D_h
#define __itkVoronoiDiagram2D_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"
#include "itkMesh.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkPolygonCell.h"
#include <vector>

#ifndef NULL
#define NULL 0 
#endif

namespace itk
{
/**
 * \class VoronoiDiagram2D
 *
 *   The Mesh structure for storing 2D Voronoi Diagram.
 *
 * Template parameters for VoronoiDiagram2D:
 *
 * TCoordType: the type associated with the coordniation of the seeds and the 
 *  resulting vertices.
 */
template <typename TCoordType>
class VoronoiDiagram2D:
    public Mesh <TCoordType, 2,
                 DefaultDynamicMeshTraits<TCoordType, 2, 2, TCoordType> >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VoronoiDiagram2D   Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Mesh <TCoordType, 2,
                DefaultDynamicMeshTraits<TCoordType, 2, 2, TCoordType> >
                  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(VoronoiDiagram2D, Mesh);

  typedef DefaultDynamicMeshTraits<TCoordType, 2, 2, TCoordType> MeshTraits;

  /**
   * typedefs from itkMesh
   */
  typedef typename MeshTraits::PixelType                PixelType;  
  enum {PointDimension = MeshTraits::PointDimension};
  enum {MaxTopologicalDimension = MeshTraits::MaxTopologicalDimension};
  typedef typename MeshTraits::CoordRepType             CoordRepType;  
  typedef typename MeshTraits::InterpolationWeightType  InterpolationWeightType;
  typedef typename MeshTraits::PointIdentifier          PointIdentifier;
  typedef typename MeshTraits::CellIdentifier           CellIdentifier;
  typedef typename MeshTraits::BoundaryIdentifier       BoundaryIdentifier;
  typedef typename MeshTraits::CellFeatureIdentifier    CellFeatureIdentifier;
  typedef typename MeshTraits::PointType                PointType;
  typedef typename MeshTraits::PointsContainer          PointsContainer;
  typedef typename MeshTraits::CellTraits                 CellTraits;
  typedef typename MeshTraits::CellsContainer           CellsContainer;
  typedef typename MeshTraits::PointCellLinksContainer  PointCellLinksContainer;
  typedef typename MeshTraits::CellLinksContainer       CellLinksContainer;
  typedef typename MeshTraits::PointDataContainer       PointDataContainer;
  typedef typename MeshTraits::CellDataContainer        CellDataContainer;  
  typedef typename MeshTraits::BoundariesContainer      BoundariesContainer;
  typedef typename MeshTraits::BoundaryDataContainer    BoundaryDataContainer;
  typedef typename MeshTraits::CellPointer    genericCellPointer;

  typedef PointLocator<PointIdentifier,PointDimension,
                       CoordRepType,PointsContainer>  PointLocatorType;
  typedef BoundingBox<PointIdentifier,PointDimension,
                      CoordRepType,PointsContainer>   BoundingBoxType;

  typedef typename PointsContainer::Pointer        PointsContainerPointer;
  typedef typename CellsContainer::Pointer         CellsContainerPointer;
  typedef typename CellLinksContainer::Pointer     CellLinksContainerPointer;
  typedef typename PointDataContainer::Pointer     PointDataContainerPointer;
  typedef typename CellDataContainer::Pointer      CellDataContainerPointer;
  typedef typename BoundariesContainer::Pointer    BoundariesContainerPointer;
  typedef typename BoundaryDataContainer::Pointer  BoundaryDataContainerPointer;  
  typedef typename PointLocatorType::Pointer       PointLocatorPointer;
  typedef typename BoundingBoxType::Pointer        BoundingBoxPointer;
  
  typedef typename
          PointsContainer::ConstIterator        PointsContainerConstIterator;
  typedef typename
          PointsContainer::Iterator             PointsContainerIterator;
  typedef typename
          CellsContainer::ConstIterator         CellsContainerConstIterator;
  typedef typename
          CellsContainer::Iterator              CellsContainerIterator;
  typedef typename
          CellLinksContainer::ConstIterator     CellLinksContainerIterator;
  typedef typename
          PointDataContainer::ConstIterator     PointDataContainerIterator;
  typedef typename
          CellDataContainer::ConstIterator      CellDataContainerIterator;
  typedef typename
          BoundariesContainer::ConstIterator    BoundariesContainerIterator;
  typedef typename
          BoundaryDataContainer::ConstIterator  BoundaryDataContainerIterator;
  typedef typename
     PointCellLinksContainer::const_iterator  PointCellLinksContainerIterator;
  
  typedef CellFeatureIdentifier  CellFeatureCount;
  
  typedef PolygonCell<PixelType,CellTraits>  Cell;
  typedef typename PolygonCell<PixelType,CellTraits>::Pointer  CellPointer;
  typedef Point<int,2> EdgeInfo;
  typedef std::deque<EdgeInfo> EdgeInfoDQ;

  typedef Cell BoundaryType;
  typedef CellPointer BoundaryPointer;
  
  typedef typename Cell::MultiVisitor CellMultiVisitorType;


  typedef std::vector<PointType> SeedsType;
  typedef typename SeedsType::iterator SeedsIterator;

  typedef LineBoundary <PixelType, CellTraits> Edge;
  typedef typename Edge::Pointer EdgePointer;

  typedef std::list<PointType> PointList;
  typedef std::vector<int> INTvector;
  typedef typename INTvector::iterator NeighborIdIterator;
  typedef typename std::vector<PointType>::iterator VertexIterator;

  itkGetMacro(NumberOfSeeds,unsigned int);
	
  /**
   * Input the seeds information, will overwrite if seeds already
   * exists.
   */
  void SetSeeds (int num, SeedsIterator begin);

  /**
   * the boundary that enclose the whold voronoi diagram
   */
  void SetBoundary(PointType vorsize);
  void SetOrigin(PointType vorsize);

  /**
   * Iterators for the neiborhood cells around the given cell;
   */
  NeighborIdIterator NeighborIdsBegin(int seeds);
  NeighborIdIterator NeighborIdsEnd(int seeds);
  
  /**
   * Iterators for all the vertices of the voronoi diagram
   */ 
  VertexIterator VertexBegin(void);
  VertexIterator VertexEnd(void);
	
  /**
   * return the given indexed seed.
   */
  PointType getSeed(int SeedID);

  /**
   * return the required cell pointer
   */
  CellPointer GetCellId(CellIdentifier cellId);

  /**
   * return the given vertex of the voronoi Diagram 
   */ 
  void GetPointId(int pId,PointType *answer);

  class VorEdge{
  public:
    PointType m_left;
    PointType m_right;
    int m_leftID;
    int m_rightID;
    int m_LineID;
    VorEdge(){};
    ~VorEdge(){};
  };
	
  typedef typename std::vector<VorEdge>::iterator VorEdgeIterator;
  /* the Iterator of all the edges for the Voronoi Diagram */
  VorEdgeIterator EdgeBegin(void);
  VorEdgeIterator EdgeEnd(void);

  /* find the two seed point that around the given edge */
  EdgeInfo GetSeedsIDAroundEdge(VorEdge *task);
  /********************************************************/

  void Reset();
  void InsertCells();

  void AddCellNeighbor(EdgeInfo x){ 
     m_CellNeighborsID[x[0]].push_back(x[1]);
	   m_CellNeighborsID[x[1]].push_back(x[0]);};
  void ClearVDregion(int i){ VDregions[i]->clearPoints();};	
  void VDregionAddPointId(int id, int x){VDregions[id]->AddPointId(x);};
  void BuildEdge(int id){ VDregions[id]->BuildEdges();};


  void LineListClear(){ f_LineList.clear();};
  void EdgeListClear(){ f_EdgeList.clear();};
  void VertListClear(){ f_VertList.clear();};
  int LineListSize(){ return f_LineList.size();}; 
  int EdgeListSize(){ return f_EdgeList.size();}; 
  int VertListSize(){ return f_VertList.size();}; 
  void AddLine(EdgeInfo x){ f_LineList.push_back(x);};
  void AddEdge(VorEdge x){ f_EdgeList.push_back(x);};
  void AddVert(PointType x){ f_VertList.push_back(x);};
  EdgeInfo GetLine(int id){ return f_LineList[id];}; 
  VorEdge GetEdge(int id){ return f_EdgeList[id];}; 
  PointType GetVert(int id){ return f_VertList[id];}; 
  EdgeInfo GetEdgeEnd(int id){
    EdgeInfo x;
    x[0]=f_EdgeList[id].m_leftID;
    x[1]=f_EdgeList[id].m_rightID;
    return x;
  }
  int GetEdgeLineID(int id){ return f_EdgeList[id].m_LineID; };


protected:
  VoronoiDiagram2D();
  ~VoronoiDiagram2D();
  
private:
  SeedsType m_Seeds;
  unsigned int m_NumberOfSeeds;
  std::vector<CellPointer> VDregions;
  PointType m_VorBoundary;
  PointType m_VorBoundaryOrigin;
  std::vector< std::vector<int> > m_CellNeighborsID;
	
  std::vector< EdgeInfo > f_LineList;
  std::vector< PointType > f_VertList;
  std::vector< VorEdge > f_EdgeList;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiDiagram2D.txx"
#endif

#endif


