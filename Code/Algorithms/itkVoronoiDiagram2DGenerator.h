/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiDiagram2DGenerator.h
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
#ifndef __itkVoronoiDiagram2DGenerator_h
#define __itkVoronoiDiagram2DGenerator_h

#include "itkCellInterface.h"
#include "itkCellBoundary.h"
#include "itkLineCell.h"
#include "itkMeshSource.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkPolygonCell.h"
#include "itkVoronoiDiagram2D.h"

#include <vector>

#ifndef NULL
#define NULL 0 
#endif

namespace itk
{
/**
 * \class VoronoiDiagram2DGenerator
 *  Using Fortune's Sweep line method to construct the 2D voronoi Diagram
 *   based on given seed points.
 *
 * Template parameters for VoronoiDiagram2DGenerator:
 *
 * TCoordType: the type associated with the coordniation of the seeds and the 
 *  resulting vertices.
 */
template <typename TCoordType>
class VoronoiDiagram2DGenerator:
    public MeshSource <VoronoiDiagram2D<TCoordType> >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VoronoiDiagram2DGenerator   Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshSource <VoronoiDiagram2D<TCoordType> >    Superclass;

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
  itkTypeMacro(VoronoiDiagram2DGenerator, MeshSource);
  
  typedef VoronoiDiagram2D<TCoordType> VDMesh;
  typedef typename VDMesh::SeedsIterator SeedsIterator;
  typedef typename VDMesh::Pointer OutputType;
  typedef typename VDMesh::PointType PointType;
  typedef typename VDMesh::SeedsType SeedsType;
  typedef typename VDMesh::EdgeInfo EdgeInfo;
  typedef typename VDMesh::EdgeInfoDQ EdgeInfoDQ;
  typedef typename VDMesh::CoordRepType CoordRepType;
  typedef typename VDMesh::VorEdge VorEdge;


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
  void GenerateData(void);

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
   * return the given indexed seed.
   */
  PointType getSeed(int SeedID);

protected:
  VoronoiDiagram2DGenerator();
  ~VoronoiDiagram2DGenerator();

private:
  unsigned int m_NumberOfSeeds;
  PointType m_VorBoundary;
  OutputType m_OutputVD;
  SeedsType m_Seeds;

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
  std::vector<FortuneSite> f_SeedSites;
	
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
#include "itkVoronoiDiagram2DGenerator.txx"
#endif

#endif


