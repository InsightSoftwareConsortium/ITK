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
/** \class VoronoiDiagram2DGenerator
 * \brief Implement the Sweep Line Algorithm for the construction of the 
 *        2D Voronoi Diagram.
 *
 * Detailed informations of this method can be found in:
 * "A sweepline algorithm for Voronoi diagrams." 
 * S. Fortune, Algorithmica 2, 153-174, 1987.
 *
 * Input parameters are:
 * (1) Size of the region.
 * (2) Seed points coordinates. These coordinates can also be randomly set.
 *
 * Template parameters for VoronoiDiagram2DGenerator:
 *
 * TCoordType: the type associated with the coordination of the seeds and the 
 *  resulting vertices.
 */
template <typename TCoordType>
class VoronoiDiagram2DGenerator:
    public MeshSource <VoronoiDiagram2D<TCoordType> >
{
public:
  typedef VoronoiDiagram2DGenerator   Self;
  typedef MeshSource <VoronoiDiagram2D<TCoordType> >    Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(VoronoiDiagram2DGenerator, MeshSource);
  
  /** Convenient typedefs. */
  typedef VoronoiDiagram2D<TCoordType> VDMesh;
  typedef typename VDMesh::SeedsIterator SeedsIterator;
  typedef typename VDMesh::Pointer OutputType;
  typedef typename VDMesh::PointType PointType;
  typedef typename VDMesh::SeedsType SeedsType;
  typedef typename VDMesh::EdgeInfo EdgeInfo;
  typedef typename VDMesh::EdgeInfoDQ EdgeInfoDQ;
  typedef typename VDMesh::CoordRepType CoordRepType;
  typedef typename VDMesh::VoronoiEdge VoronoiEdge;

  /** Get the number of seed points. */
  itkGetMacro(NumberOfSeeds,unsigned int);
  
  /** Input the seeds information, will overwrite if seeds already
   * exists. */
  void SetSeeds (int num, SeedsIterator begin);

  /** Add more seeds at one time. */
  void AddSeeds(int num,SeedsIterator begin);
  void AddOneSeed(PointType);

  /** Sort the seeds by ____. */
  void SortSeeds(void);

  /** Produce the output information. */
  virtual void GenerateOutputInformation() {}

  /** Update the Voronoi Diagram after adding seed(s). */
  void UpdateDiagram(void);

  /** The boundary that enclose the whole voronoi diagram. */
  void SetBoundary(PointType vorsize);
  void SetOrigin(PointType vorsize);

  /** Set the seeds points randomly. */
  void SetRandomSeeds(int num);

  /** Return the given indexed seed. */
  PointType GetSeed(int SeedID);

protected:
  VoronoiDiagram2DGenerator();
  ~VoronoiDiagram2DGenerator();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Generate Voronoi Diagram based on the current list of seeds. */
  void GenerateData(void);

private:
  VoronoiDiagram2DGenerator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  unsigned int m_NumberOfSeeds;
  PointType m_VorBoundary;
  OutputType m_OutputVD;
  SeedsType m_Seeds;

  static bool comp(PointType arg1,PointType arg2);
  /** Small data structures for Fortune's Method
   * and some public variables/methods not for external access. */ 
  class FortuneSite{
  public:
    PointType m_Coord;
    int m_Sitenbr;
    FortuneSite(){};
    ~FortuneSite(){};
  };

  class FortuneEdge{
  public:
    float m_A, m_B, m_C;  // explicit line function: Ax + By = C;
    FortuneSite *m_Ep[2];
    FortuneSite *m_Reg[2];
    int m_Edgenbr;
    FortuneEdge(){};
    ~FortuneEdge(){};
  };

  class FortuneHalfEdge{
  public:
    FortuneHalfEdge *m_Left;
    FortuneHalfEdge *m_Right;
    FortuneEdge *m_Edge;
    bool m_RorL;
    FortuneSite *m_Vert;
    double m_Ystar;
    FortuneHalfEdge *m_Next;
    FortuneHalfEdge(){};
    ~FortuneHalfEdge(){};
  };

  double m_Pxmin;
  double m_Pxmax;
  double m_Pymin;
  double m_Pymax;
  double m_Deltax;
  double m_Deltay;
  double m_SqrtNSites;
  unsigned int m_PQcount;
  int m_PQmin;
  unsigned int m_PQhashsize;
  unsigned int m_Nedges;
  unsigned int m_Nvert;
  FortuneSite *m_BottomSite;
  std::vector<FortuneHalfEdge> m_PQHash;
  unsigned int m_ELhashsize;
  FortuneHalfEdge m_ELleftend;
  FortuneHalfEdge m_ELrightend;
  std::vector<FortuneHalfEdge *> m_ELHash;
  FortuneEdge m_DELETED;
  std::vector<FortuneSite> m_SeedSites;
  
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


