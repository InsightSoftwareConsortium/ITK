/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoi2DDiagram.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVoronoi2DDiagram_h
#define __itkVoronoi2DDiagram_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"
#include "itkMesh.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkDynamicPolygonCell.h"
#include <vector>

#ifndef NULL
#define NULL 0 
#endif

namespace itk
{
/**
 * \class Voronoi2DDiagram
 *  Using Fortune's Sweep line method to construct the 2D voronoi Diagram
 *   based on given seed points.
 *
 * Template parameters for Voronoi2DDiagram:
 *
 * TCoordType: the type associated with the coordniation of the seeds and the 
 *  resulting vertices.
 */
template <typename TCoordType>
class Voronoi2DDiagram:
    public Mesh <TCoordType, 2,
                 DefaultDynamicMeshTraits<TCoordType, 2, 2, TCoordType> >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Voronoi2DDiagram   Self;
  
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
  itkTypeMacro(Voronoi2DDiagram, Mesh);

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
  
  typedef DynamicPolygonCell<PixelType,CellTraits>  Cell;
  typedef typename DynamicPolygonCell<PixelType,CellTraits>::Pointer  CellPointer;
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
	typedef Point<int, 2> TwoINT;
	

	itkGetMacro(NumberOfSeeds,unsigned int);
	
	/**
	 * Input the seeds information, will overwrite if seeds already
	 * exists.
	 */
	void SetSeeds (int num, SeedsIterator begin);

	/**
	 * Add more seeds at one time.
	 */
	void AddSeeds(int num,SeedsIterator begin);
  void AddOneSeed(PointType);

	void SortSeeds(void);
	/**
	 * Generate Voronoi Diagram based on the current list of seeds.
	 */
	void GenerateDiagram(void);

	/**
	 * Update the Voronoi Diagram after adding seed(s).
	 */
	void UpdateDiagram(void);

	/**
	 * the boundary that enclose the whold voronoi diagram
	 */
	void SetBoundary(PointType vorsize);
	void SetOrigin(PointType vorsize);

	/**
	 * set the seeds points randomly.
	 */
	void SetRandomSeeds(int num);

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

  /********************************************************/
  /** public Data structure needed for Fortune's Method****/
	class FortuneEdgeInfo{
		public:
			PointType m_left;
			PointType m_right;
			int m_leftID;
			int m_rightID;
			int m_LineID;
			FortuneEdgeInfo(){};
			~FortuneEdgeInfo(){};
	};
	
	typedef typename std::vector<FortuneEdgeInfo>::iterator EdgeIterator;
	/* the Iterator of all the edges for the Voronoi Diagram */
	EdgeIterator EdgeBegin(void);
	EdgeIterator EdgeEnd(void);

	/* find the two seed point that around the given edge */
	TwoINT GetSeedsIDAroundEdge(FortuneEdgeInfo *task);
  /********************************************************/

protected:
	Voronoi2DDiagram();
	~Voronoi2DDiagram();

private:
	SeedsType m_Seeds;
	unsigned int m_NumberOfSeeds;
  std::vector<CellPointer> VDregions;
	PointType m_VorBoundary;
	std::vector< std::vector<int> > m_CellNeighborsID;

	static bool comp(PointType arg1,PointType arg2);

/**
 * small datastructures for Fortune's Method.
 * and some public variables/methods not for external access.
 */ 
	class FortuneSite{
		public:
			PointType m_coord;
			int m_sitenbr;
			FortuneSite(){};
			~FortuneSite(){};
	};

	class FortuneEdge{
		public:
			float m_a, m_b, m_c;  // explicit line function: ax + by = c;
			FortuneSite *m_ep[2];
			FortuneSite *m_reg[2];
			int m_edgenbr;
			FortuneEdge(){};
			~FortuneEdge(){};
	};

	class FortuneHalfEdge{
		public:
			FortuneHalfEdge *m_left;
			FortuneHalfEdge *m_right;
			FortuneEdge *m_edge;
			bool m_RorL;
			FortuneSite *m_vert;
			double m_ystar;
			FortuneHalfEdge *m_next;
			FortuneHalfEdge(){};
			~FortuneHalfEdge(){};
	};

	
	std::vector<FortuneSite> f_SeedSites;
	std::vector< Point<int,2> > f_LineList;
	std::vector<PointType> f_VertList;
	std::vector<FortuneEdgeInfo> f_EdgeList;
	double f_pxmin;
	double f_pxmax;
	double f_pymin;
	double f_pymax;
	double f_deltax;
	double f_deltay;
	double f_sqrtNSites;
	unsigned int f_PQcount;
	int f_PQmin;
	unsigned int f_PQhashsize;
	unsigned int f_nedges;
	unsigned int f_nvert;
	FortuneSite *f_bottomSite;
	std::vector<FortuneHalfEdge> f_PQHash;
	unsigned int f_ELhashsize;
	FortuneHalfEdge f_ELleftend;
	FortuneHalfEdge f_ELrightend;
	std::vector<FortuneHalfEdge *> f_ELHash;
	FortuneEdge f_DELETED;
	

	bool differentPoint(PointType p1,PointType p2);
	bool almostsame(CoordRepType p1,CoordRepType p2);
  unsigned char Pointonbnd(int VertID);

  void GenerateVDFortune(void);
  void ConstructDiagram(void);

	void createHalfEdge(FortuneHalfEdge *task, FortuneEdge *e,bool pm);
	void PQshowMin(PointType *task);
  FortuneHalfEdge *findLeftHE(PointType *p);
  FortuneHalfEdge *ELgethash(int b);
	bool right_of(FortuneHalfEdge *el, PointType *p);
	FortuneSite *getRightReg(FortuneHalfEdge *he);
	FortuneSite *getLeftReg(FortuneHalfEdge *he);
	void bisect(FortuneEdge *, FortuneSite *s1,FortuneSite *s2);
	void insertEdgeList(FortuneHalfEdge *lbase, FortuneHalfEdge *lnew);
	void intersect(FortuneSite *task,FortuneHalfEdge *el1,FortuneHalfEdge *el2);
	void deletePQ(FortuneHalfEdge *task);
	void deleteEdgeList(FortuneHalfEdge *task);
	int PQbucket(FortuneHalfEdge *task);
	void clip_line(FortuneEdge *task);
	void insertPQ(FortuneHalfEdge *he, FortuneSite *v, double offset);
  double dist(FortuneSite *s1,FortuneSite *s2);
	FortuneHalfEdge *getPQmin(void);
  void makeEndPoint(FortuneEdge *task, bool lr, FortuneSite *ends);

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoi2DDiagram.txx"
#endif

#endif


