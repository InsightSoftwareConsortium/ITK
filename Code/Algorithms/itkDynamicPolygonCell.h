/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicPolygonCell.h
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
#ifndef __itkDynamicPolygonCell_h
#define __itkDynamicPolygonCell_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"
#include <vector>
#include <queue>


namespace itk
{

template <
  typename TPixelType,
  typename TCellTraits
  >
class DynamicPolygonCell: public CellInterface< TPixelType , TCellTraits >
{
/** \class DynamicPolygonCell
 * DynamicPolygonCell represents a polygon for a Mesh.
 *  the points of the polygon can be dynamically changed.
 *
 * Template parameters for DynamicPolygonCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 *
 * TCellTraits =
 *     Type information of mesh containing cell.
 */

public:
  /**
   * Standard "Self" typedef.
   */
  typedef DynamicPolygonCell  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef CellInterface<TPixelType,TCellTraits>  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Save the PixelType template parameter.
   */
  typedef TPixelType                                PixelType;
  
  /**
   * Save the CellTraits template parameter.
   */
  typedef TCellTraits                                 CellTraits;

  /**
   * Pick-up typedefs from superclass
   */
  typedef typename CellTraits::CellFeatureIdentifier  CellFeatureIdentifier;
  typedef CellFeatureIdentifier  CellFeatureCount;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdIterator 
                   PointIdIterator;
  typedef typename CellInterface<TPixelType,TCellTraits>::PointIdConstIterator
                   PointIdConstIterator;

  /**
   * Save some template parameter information.
   */
  typedef typename CellTraits::CoordRepType         CoordRepType;
  typedef typename CellTraits::PointIdentifier  PointIdentifier;
  
  enum { PointDimension = CellTraits::PointDimension};
  enum { CellDimension = 2 };
  
  typedef typename CellInterface<TPixelType,TCellTraits>::Pointer CellPointer;

  /**
   * The type of boundary for this voronoi cell's vertices.
   */
  typedef VertexBoundary< TPixelType , TCellTraits >  Vertex;
  typedef typename Vertex::Pointer VertexPointer;

  /**
   * The type of boundary for this voronoi cell's edges.
   */
  typedef LineBoundary< TPixelType , TCellTraits >    Edge;
  typedef typename Edge::Pointer EdgePointer;
  typedef Point<int,2> EdgeInfo;
  typedef std::deque<EdgeInfo> EdgeInfoDQ;

   
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(DynamicPolygonCell, CellInterface);

  /**
   * Need to add DynamicPolygon_CELL into CellInterface.
   */
  itkCellVisitMacro(DYNAMICPOLYGON_CELL);

  /**
   * Implement the standard CellInterface.
   */
  virtual CellPointer MakeCopy(void);
  virtual int GetDimension(void);
  virtual int GetNumberOfPoints(void);
  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension);
  virtual CellPointer GetBoundaryFeature(int dimension, CellFeatureIdentifier);
  
  virtual void SetPointIds(PointIdConstIterator first); 
  virtual void SetPointIds(PointIdConstIterator first,
			   PointIdConstIterator last);

  void AddPointId(PointIdentifier);
  void SetPointIds(int dummy, int num, PointIdConstIterator first);
  void BuildEdges(void);
  void clearPoints(void);
  
  virtual void SetPointId(int localId, PointIdentifier);
  virtual PointIdIterator      PointIdsBegin(void);
  virtual PointIdConstIterator PointIdsBegin(void) const;
  virtual PointIdIterator      PointIdsEnd(void);
  virtual PointIdConstIterator PointIdsEnd(void) const; 

  /**
   * Polygon-specific interface.
   */
  virtual CellFeatureCount GetNumberOfVertices(void);
  virtual CellFeatureCount GetNumberOfEdges(void);
  virtual VertexPointer GetVertex(CellFeatureIdentifier);
  virtual EdgePointer GetEdge(CellFeatureIdentifier);

protected:
  std::vector<EdgeInfo> m_Edges;
  int m_NumberOfEdges;
  std::vector<PointIdentifier> m_PointIds;
  int m_NumberOfPoints;

  DynamicPolygonCell(){
		m_NumberOfEdges = 0;
		m_NumberOfPoints = 0;
  };
  ~DynamicPolygonCell(){};
};

/** \class DynamicPolygonBoundary
 * Create a boundary-wrapped version of the DynamicPolygonCell.
 */
template <typename TPixelType, typename TCellTraits>
class DynamicPolygonBoundary:
  public CellBoundary< DynamicPolygonCell< TPixelType , TCellTraits > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef DynamicPolygonBoundary  Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>     Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(DynamicPolygonBoundary, CellBoundary);
};

} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDynamicPolygonCell.txx"
#endif

#endif


