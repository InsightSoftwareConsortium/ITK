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
#ifndef itkVoronoiDiagram2DGenerator_h
#define itkVoronoiDiagram2DGenerator_h

#include "itkMeshSource.h"
#include "itkVoronoiDiagram2D.h"

#include <vector>

namespace itk
{
/** \class VoronoiDiagram2DGenerator
 * \brief Implement the Sweep Line Algorithm for the construction of the
 *        2D Voronoi Diagram.
 *
 * Detailed information on this method can be found in:
 * "A sweepline algorithm for Voronoi diagrams."
 * S. Fortune, Algorithmica 2, 153-174, 1987.
 *
 * Input parameters are:
 * (1) Size of the region.
 * (2) Seed points coordinates. These coordinates can also be randomly set.
 *
 * \tparam TCoordType The type associated with the coordination of the seeds
 * and the resulting vertices.
 *
 * \ingroup ITKVoronoi
 */
template< typename TCoordType >
class ITK_TEMPLATE_EXPORT VoronoiDiagram2DGenerator:
  public MeshSource< VoronoiDiagram2D< TCoordType > >
{
public:
  typedef VoronoiDiagram2DGenerator                    Self;
  typedef MeshSource< VoronoiDiagram2D< TCoordType > > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(VoronoiDiagram2DGenerator, MeshSource);

  /** Convenient typedefs. */
  typedef VoronoiDiagram2D< TCoordType > VoronoidDiagramType;
  typedef VoronoidDiagramType            VDMesh;
  typedef typename VDMesh::SeedsIterator SeedsIterator;
  typedef typename VDMesh::Pointer       OutputType;
  typedef typename VDMesh::PointType     PointType;
  typedef typename VDMesh::SeedsType     SeedsType;
  typedef typename VDMesh::EdgeInfo      EdgeInfo;
  typedef typename VDMesh::EdgeInfoDQ    EdgeInfoDQ;
  typedef typename VDMesh::CoordRepType  CoordRepType;
  typedef typename VDMesh::VoronoiEdge   VoronoiEdge;

  /** Get the number of seed points. */
  itkGetConstMacro(NumberOfSeeds, unsigned int);

  /** Set the seed points.
   *  The first argument explicits the number of seeds. Will overwrite if seeds
   *  already exist. */
  void SetSeeds(int num, SeedsIterator begin);

  /** Add more seeds. Specify the number of seeds to be added as "num". */
  void AddSeeds(int num, SeedsIterator begin);

  /** Add one seed. */
  void AddOneSeed(PointType);

  /** Sort the seeds with their y, then x, coordinates. */
  void SortSeeds();

  /** Produce the output information. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE {}

  /** Update the Voronoi Diagram after adding seed(s). */
  void UpdateDiagram();

  /** Set the rectangle that encloses the whole Voronoi Diagram. */
  void SetBoundary(PointType vorsize);

  void SetOrigin(PointType vorsize);

  /** Set the seeds points randomly. */
  void SetRandomSeeds(int num);

  /** Return the given indexed seed. */
  PointType GetSeed(int SeedID);

protected:
  VoronoiDiagram2DGenerator();
  ~VoronoiDiagram2DGenerator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Voronoi Diagram based on the current list of seeds. */
  void GenerateData(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoronoiDiagram2DGenerator);

  unsigned int m_NumberOfSeeds;
  PointType    m_VorBoundary;
  OutputType   m_OutputVD;
  SeedsType    m_Seeds;

  /** Compare point coordinates in the y direction. */
  static bool comp(PointType arg1, PointType arg2);

  /** \class FortuneSite
   * \brief Small data structures for Fortune's Method and some public
   * variables/methods not for external access.
   *
   * \ingroup ITKVoronoi
   */
  class FortuneSite;
  class FortuneEdge;
  class FortuneHalfEdge;

  // All private nested classes must be friend classes to work with SunOS-CC
  // compiler. If not, the private nested classes will not be able to access
  // each other.
  friend class FortuneSite;
  friend class FortuneEdge;
  friend class FortuneHalfEdge;

  class FortuneSite
  {
  public:
    PointType m_Coord;
    int       m_Sitenbr;

    FortuneSite() :
      m_Sitenbr( NumericTraits< int >::max() )
    {
      m_Coord.Fill( NumericTraits< CoordRepType >::max() );
    }

    ~FortuneSite(){}
  };

  class FortuneEdge
  {
  public:
    float        m_A, m_B, m_C;    // explicit line function: Ax + By = C;
    FortuneSite *m_Ep[2];
    FortuneSite *m_Reg[2];
    int          m_Edgenbr;

    FortuneEdge() :
      m_A(0.0),
      m_B(0.0),
      m_C(0.0),
      m_Edgenbr(0)
    {
      m_Ep[0] = m_Ep[1] = m_Reg[0] = m_Reg[1] = ITK_NULLPTR;
    }

    ~FortuneEdge(){}
  };

  class FortuneHalfEdge
  {
  public:
    FortuneHalfEdge *m_Left;
    FortuneHalfEdge *m_Right;
    FortuneEdge *    m_Edge;
    bool             m_RorL;
    FortuneSite *    m_Vert;
    double           m_Ystar;
    FortuneHalfEdge *m_Next;

    FortuneHalfEdge() :
      m_Left(ITK_NULLPTR),
      m_Right(ITK_NULLPTR),
      m_Edge(ITK_NULLPTR),
      m_RorL(false),
      m_Vert(ITK_NULLPTR),
      m_Ystar(0.0),
      m_Next(ITK_NULLPTR)
    {}

    FortuneHalfEdge(const FortuneHalfEdge & edge) :
      m_Left(edge.m_Left),
      m_Right(edge.m_Right),
      m_Edge(edge.m_Edge),
      m_RorL(edge.m_RorL),
      m_Vert(edge.m_Vert),
      m_Ystar(edge.m_Ystar),
      m_Next(edge.m_Next)
    {}

    ~FortuneHalfEdge() {}
  };

  double m_Pxmin;
  double m_Pxmax;
  double m_Pymin;
  double m_Pymax;
  double m_Deltax;
  double m_Deltay;
  double m_SqrtNSites;

  unsigned int                   m_PQcount;
  int                            m_PQmin;
  unsigned int                   m_PQhashsize;
  unsigned int                   m_Nedges;
  unsigned int                   m_Nvert;
  FortuneSite *                  m_BottomSite;
  std::vector< FortuneHalfEdge > m_PQHash;

  unsigned int                     m_ELhashsize;
  FortuneHalfEdge                  m_ELleftend;
  FortuneHalfEdge                  m_ELrightend;
  std::vector< FortuneHalfEdge * > m_ELHash;

  FortuneEdge                m_DELETED;
  std::vector< FortuneSite > m_SeedSites;

  /** Methods to convert the result from Fortune Algorithm into itkMesh
   * structure.
   */
  bool differentPoint(PointType p1, PointType p2);

  bool almostsame(CoordRepType p1, CoordRepType p2);

  unsigned char Pointonbnd(int VertID);

  void GenerateVDFortune();

  void ConstructDiagram();

  void createHalfEdge(FortuneHalfEdge *task, FortuneEdge *e, bool pm);

  void PQshowMin(PointType *task);

  FortuneHalfEdge * findLeftHE(PointType *p);

  FortuneHalfEdge * ELgethash(int b);

  /** Generate Voronoi Diagram using Fortune's Method. (Sweep Line)
   *
   * Information is stored in m_VertexList, m_EdgeList and m_LineList.
   */
  bool right_of(FortuneHalfEdge *el, PointType *p);

  FortuneSite * getRightReg(FortuneHalfEdge *he);

  FortuneSite * getLeftReg(FortuneHalfEdge *he);

  void bisect(FortuneEdge *, FortuneSite *s1, FortuneSite *s2);

  void insertEdgeList(FortuneHalfEdge *lbase, FortuneHalfEdge *lnew);

  void intersect(FortuneSite *task, FortuneHalfEdge *el1, FortuneHalfEdge *el2);

  void deletePQ(FortuneHalfEdge *task);

  void deleteEdgeList(FortuneHalfEdge *task);

  int PQbucket(FortuneHalfEdge *task);

  void clip_line(FortuneEdge *task);

  void insertPQ(FortuneHalfEdge *he, FortuneSite *v, double offset);

  double dist(FortuneSite *s1, FortuneSite *s2);

  FortuneHalfEdge * getPQmin();

  void makeEndPoint(FortuneEdge *task, bool lr, FortuneSite *ends);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiDiagram2DGenerator.hxx"
#endif

#endif
