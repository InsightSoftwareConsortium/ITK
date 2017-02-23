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
#ifndef itkVoronoiDiagram2DGenerator_hxx
#define itkVoronoiDiagram2DGenerator_hxx
#include "itkVoronoiDiagram2DGenerator.h"

#include "itkIntTypes.h"
#include <algorithm>
#include "vnl/vnl_sample.h"
#include "itkMath.h"

namespace itk
{
const double NUMERIC_TOLERENCE = 1.0e-10;
const double DIFF_TOLERENCE = 0.001;

template< typename TCoordRepType >
VoronoiDiagram2DGenerator< TCoordRepType >::VoronoiDiagram2DGenerator() :
  m_NumberOfSeeds( 0 ),
  m_OutputVD( Self::GetOutput() ), // Note: this line is suspicious
  m_Pxmin( 0.0 ),
  m_Pxmax( 0.0 ),
  m_Pymin( 0.0 ),
  m_Pymax( 0.0 ),
  m_Deltax( 0.0 ),
  m_Deltay( 0.0 ),
  m_SqrtNSites( 0.0 ),
  m_PQcount( 0 ),
  m_PQmin( 0 ),
  m_PQhashsize( 0 ),
  m_Nedges( 0 ),
  m_Nvert( 0 ),
  m_BottomSite( ITK_NULLPTR ),
  m_ELhashsize( 0 )
{
  m_VorBoundary.Fill( 0.0 );
}

template< typename TCoordRepType >
VoronoiDiagram2DGenerator< TCoordRepType >::
~VoronoiDiagram2DGenerator()
{}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::SetRandomSeeds(int num)
{
  PointType curr;

  m_Seeds.clear();
  double ymax = (double)( m_VorBoundary[1] );
  double xmax = (double)( m_VorBoundary[0] );
  for ( int i = 0; i < num; ++i )
    {
    curr[0] = (CoordRepType)( vnl_sample_uniform(0, xmax) );
    curr[1] = (CoordRepType)( vnl_sample_uniform(0, ymax) );
    m_Seeds.push_back(curr);
    }
  m_NumberOfSeeds = num;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::SetSeeds(int num,  SeedsIterator begin)
{
  m_Seeds.clear();
  SeedsIterator ii(begin);
  for ( int i = 0; i < num; ++i )
    {
    m_Seeds.push_back(*ii++);
    }
  m_NumberOfSeeds = num;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::SetBoundary(PointType vorsize)
{
  m_VorBoundary[0] = vorsize[0];
  m_VorBoundary[1] = vorsize[1];
  m_OutputVD->SetBoundary(vorsize);
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::SetOrigin(PointType vorsize)
{
  m_Pxmin = vorsize[0];
  m_Pymin = vorsize[1];
  m_OutputVD->SetOrigin(vorsize);
}

template< typename TCoordRepType >
bool
VoronoiDiagram2DGenerator< TCoordRepType >::comp(PointType arg1, PointType arg2)
{
  if ( arg1[1] < arg2[1] )
    {
    return 1;
    }
  else if ( arg1[1] > arg2[1] )
    {
    return 0;
    }
  // arg1[1] == arg2[1]
  else if ( arg1[0] < arg2[0] )
    {
    return 1;
    }
  else if ( arg1[0] > arg2[0] )
    {
    return 0;
    }
  // arg1[0] == arg2[0]
  else
    {
    return 0;
    }
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::SortSeeds(void)
{
  std::sort(m_Seeds.begin(), m_Seeds.end(), comp);
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::AddSeeds(int num, SeedsIterator begin)
{
  SeedsIterator ii(begin);

  for ( int i = 0; i < num; ++i )
    {
    m_Seeds.push_back(*ii++);
    }
  m_NumberOfSeeds += num;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::AddOneSeed(PointType inputSeed)
{
  m_Seeds.push_back(inputSeed);
  m_NumberOfSeeds++;
}

template< typename TCoordRepType >
typename VoronoiDiagram2DGenerator< TCoordRepType >::PointType
VoronoiDiagram2DGenerator< TCoordRepType >::GetSeed(int SeedID)
{
  PointType answer;

  answer[0] = m_Seeds[SeedID][0];
  answer[1] = m_Seeds[SeedID][1];
  return answer;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::GenerateData(void)
{
  SortSeeds();
  m_OutputVD->SetSeeds( m_NumberOfSeeds, m_Seeds.begin() );
  this->GenerateVDFortune();
  this->ConstructDiagram();
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::UpdateDiagram(void)
{
  this->GenerateData();
}

template< typename TCoordRepType >
bool
VoronoiDiagram2DGenerator< TCoordRepType >::differentPoint(PointType p1, PointType p2)
{
  double diffx = p1[0] - p2[0];
  double diffy = p1[1] - p2[1];

  return ( ( diffx < -DIFF_TOLERENCE ) || ( diffx > DIFF_TOLERENCE )
           || ( diffy < -DIFF_TOLERENCE ) || ( diffy > DIFF_TOLERENCE ) );
}

template< typename TCoordRepType >
bool
VoronoiDiagram2DGenerator< TCoordRepType >::almostsame(CoordRepType p1, CoordRepType p2)
{
  double diff = p1 - p2;
  bool   save;

  save = ( ( diff < -DIFF_TOLERENCE ) || ( diff > DIFF_TOLERENCE ) );
  return ( !save );
}

template< typename TCoordRepType >
unsigned char
VoronoiDiagram2DGenerator< TCoordRepType >::Pointonbnd(int VertID)
{
  PointType currVert = m_OutputVD->GetVertex(VertID);

  if ( almostsame(currVert[0], m_Pxmin) )
    {
    return 1;
    }
  else if ( almostsame(currVert[1], m_Pymax) )
    {
    return 2;
    }
  else if ( almostsame(currVert[0], m_Pxmax) )
    {
    return 3;
    }
  else if ( almostsame(currVert[1], m_Pymin) )
    {
    return 4;
    }
  else
    {
    return 0;
    }
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::ConstructDiagram(void)
{
  EdgeInfoDQ *rawEdges = new EdgeInfoDQ[m_NumberOfSeeds];

  m_OutputVD->Reset();

  EdgeInfo currentPtID;
  int      edges = m_OutputVD->EdgeListSize();

  EdgeInfo LRsites;
  for ( int i = 0; i < edges; i++ )
    {
    currentPtID = m_OutputVD->GetEdgeEnd(i);
    LRsites = m_OutputVD->GetLine( m_OutputVD->GetEdgeLineID(i) );
    rawEdges[LRsites[0]].push_back(currentPtID);
    rawEdges[LRsites[1]].push_back(currentPtID);
    m_OutputVD->AddCellNeighbor(LRsites);
    }

  PointType corner[4];
  int       cornerID[4];

  corner[0][0] = m_Pxmin;
  corner[0][1] = m_Pymin;
  cornerID[0] = m_Nvert;
  m_Nvert++;
  m_OutputVD->AddVert(corner[0]);
  corner[1][0] = m_Pxmin;
  corner[1][1] = m_Pymax;
  cornerID[1] = m_Nvert;
  m_Nvert++;
  m_OutputVD->AddVert(corner[1]);
  corner[2][0] = m_Pxmax;
  corner[2][1] = m_Pymax;
  cornerID[2] = m_Nvert;
  m_Nvert++;
  m_OutputVD->AddVert(corner[2]);
  corner[3][0] = m_Pxmax;
  corner[3][1] = m_Pymin;
  cornerID[3] = m_Nvert;
  m_Nvert++;
  m_OutputVD->AddVert(corner[3]);

  std::list< EdgeInfo > buildEdges;
  typename std::list< EdgeInfo >::iterator BEiter;
  EdgeInfo curr;
  EdgeInfo curr1;
  EdgeInfo curr2;

  unsigned char                frontbnd;
  unsigned char                backbnd;
  std::vector< IdentifierType > cellPoints;
  for ( unsigned int i = 0; i < m_NumberOfSeeds; i++ )
    {
    buildEdges.clear();
    curr = rawEdges[i].front();
    rawEdges[i].pop_front();
    buildEdges.push_back(curr);
    EdgeInfo front = curr;
    EdgeInfo back = curr;
    int      maxStop = static_cast< int >( rawEdges[i].size() );
    while ( !( rawEdges[i].empty() ) )
      {
      maxStop--;
      curr = rawEdges[i].front();
      rawEdges[i].pop_front();
      frontbnd = Pointonbnd(front[0]);
      backbnd = Pointonbnd(back[1]);
      if ( curr[0] == back[1] )
        {
        buildEdges.push_back(curr);
        back = curr;
        }
      else if ( curr[1] == front[0] )
        {
        buildEdges.push_front(curr);
        front = curr;
        }
      else if ( curr[1] == back[1] )
        {
        curr1[1] = curr[0];
        curr1[0] = curr[1];
        buildEdges.push_back(curr1);
        back = curr1;
        }
      else if ( curr[0] == front[0] )
        {
        curr1[0] = curr[1];
        curr1[1] = curr[0];
        buildEdges.push_front(curr1);
        front = curr1;
        }
      else if ( ( frontbnd != 0 ) || ( backbnd != 0 ) )
        {
        unsigned char cfrontbnd = Pointonbnd(curr[0]);
        unsigned char cbackbnd = Pointonbnd(curr[1]);

        if ( ( cfrontbnd == backbnd ) && ( backbnd ) )
          {
          curr1[0] = back[1];
          curr1[1] = curr[0];
          buildEdges.push_back(curr1);
          buildEdges.push_back(curr);
          back = curr;
          }
        else if ( ( cbackbnd == frontbnd ) && ( frontbnd ) )
          {
          curr1[0] = curr[1];
          curr1[1] = front[0];
          buildEdges.push_front(curr1);
          buildEdges.push_front(curr);
          front = curr;
          }
        else if ( ( cfrontbnd == frontbnd ) && ( frontbnd ) )
          {
          curr1[0] = curr[0];
          curr1[1] = front[0];
          buildEdges.push_front(curr1);
          curr1[0] = curr[1];
          curr1[1] = curr[0];
          buildEdges.push_front(curr1);
          front = curr1;
          }
        else if ( ( cbackbnd == backbnd ) && ( backbnd ) )
          {
          curr1[0] = back[1];
          curr1[1] = curr[1];
          buildEdges.push_back(curr1);
          curr1[0] = curr[1];
          curr1[1] = curr[0];
          buildEdges.push_back(curr1);
          back = curr1;
          }
        else
          {
          rawEdges[i].push_back(curr);
          }
        }
      else
        {
        rawEdges[i].push_back(curr);
        }
      }

    curr = buildEdges.front();
    curr1 = buildEdges.back();
    if ( curr[0] != curr1[1] )
      {
      frontbnd = Pointonbnd(curr[0]);
      backbnd = Pointonbnd(curr1[1]);
      if ( ( frontbnd != 0 ) && ( backbnd != 0 ) )
        {
        if ( frontbnd == backbnd )
          {
          curr2[0] = curr1[1];
          curr2[1] = curr[0];
          buildEdges.push_back(curr2);
          }
        else if ( ( frontbnd == backbnd + 1 ) || ( frontbnd == backbnd - 3 ) )
          {
          curr2[0] = cornerID[frontbnd - 1];
          curr2[1] = curr[0];
          buildEdges.push_front(curr2);
          curr2[1] = curr2[0];
          curr2[0] = curr1[1];
          buildEdges.push_front(curr2);
          }
        else if ( ( frontbnd == backbnd - 1 ) || ( frontbnd == backbnd + 3 ) )
          {
          curr2[0] = cornerID[backbnd - 1];
          curr2[1] = curr[0];
          buildEdges.push_front(curr2);
          curr2[1] = curr2[0];
          curr2[0] = curr1[1];
          buildEdges.push_front(curr2);
          }
        else
          {
          itkDebugMacro(<< "Numerical problem 1" << curr[0] << " " << curr1[1]);
          }
        }
      }

    EdgeInfo pp;

    m_OutputVD->ClearRegion(i);

    for ( BEiter = buildEdges.begin(); BEiter != buildEdges.end(); ++BEiter )
      {
      pp = *BEiter;
      m_OutputVD->VoronoiRegionAddPointId(i, pp[0]);
      }
    m_OutputVD->BuildEdge(i);
    }
  m_OutputVD->InsertCells();

  delete[] rawEdges;
}

template< typename TCoordRepType >
bool
VoronoiDiagram2DGenerator< TCoordRepType >::right_of(FortuneHalfEdge *el, PointType *p)
{
  FortuneEdge *e = el->m_Edge;
  FortuneSite *topsite = e->m_Reg[1];

  bool right_of_site = ( ( ( *p )[0] ) > ( topsite->m_Coord[0] ) );

  if ( right_of_site && ( !( el->m_RorL ) ) ) { return ( 1 ); }
  if ( ( !right_of_site ) && ( el->m_RorL ) ) { return ( 0 ); }
  bool above;
  bool fast;
  if ( e->m_A == 1.0 )
    {
    double dyp = ( ( *p )[1] ) - ( topsite->m_Coord[1] );
    double dxp = ( ( *p )[0] ) - ( topsite->m_Coord[0] );
    fast = 0;
    if ( ( ( !right_of_site ) && ( ( e->m_B ) < 0.0 ) )
         || ( right_of_site && ( ( e->m_B ) >= 0.0 ) ) )
      {
      above = ( dyp >= ( e->m_B ) * dxp );
      fast = above;
      }
    else
      {
      above = ( ( ( ( *p )[0] ) + ( ( *p )[1] ) * ( e->m_B ) ) > e->m_C );
      if ( e->m_B < 0.0 ) { above = !above; }
      if ( !above ) { fast = 1; }
      }
    if ( !fast )
      {
      double dxs = topsite->m_Coord[0] - ( ( e->m_Reg[0] )->m_Coord[0] );
      above =
        ( ( ( e->m_B ) * ( dxp * dxp - dyp * dyp ) ) < ( dxs * dyp * ( 1.0 + 2.0 * dxp / dxs + ( e->m_B ) * ( e->m_B ) ) ) );
      if ( ( e->m_B ) < 0.0 ) { above = !above; }
      }
    }
  else
    { // e->m_B == 1.0
    double y1 = ( e->m_C ) - ( e->m_A ) * ( ( *p )[0] );
    double t1 = ( ( *p )[1] ) - y1;
    double t2 = ( ( *p )[0] ) - topsite->m_Coord[0];
    double t3 = y1 - topsite->m_Coord[1];
    above = ( ( t1 * t1 ) > ( t2 * t2 + t3 * t3 ) );
    }
  return ( el->m_RorL ? ( !above ) : above );
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::createHalfEdge(FortuneHalfEdge *task, FortuneEdge *e, bool pm)
{
  task->m_Edge = e;
  task->m_RorL = pm;
  task->m_Next = ITK_NULLPTR;
  task->m_Vert = ITK_NULLPTR;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::PQshowMin(PointType *answer)
{
  while ( ( m_PQHash[m_PQmin].m_Next ) == ITK_NULLPTR )
    {
    m_PQmin += 1;
    }
  ( *answer )[0] = m_PQHash[m_PQmin].m_Next->m_Vert->m_Coord[0];
  ( *answer )[1] = m_PQHash[m_PQmin].m_Next->m_Ystar;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::deletePQ(FortuneHalfEdge *task)
{
  FortuneHalfEdge *last;

  if ( ( task->m_Vert ) != ITK_NULLPTR )
    {
    last = &( m_PQHash[PQbucket(task)] );
    while ( ( last->m_Next ) != task )
      {
      last = last->m_Next;
      }
    last->m_Next = ( task->m_Next );
    m_PQcount--;
    task->m_Vert = ITK_NULLPTR;
    }
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::deleteEdgeList(FortuneHalfEdge *task)
{
  ( task->m_Left )->m_Right = task->m_Right;
  ( task->m_Right )->m_Left = task->m_Left;
  task->m_Edge = &( m_DELETED );
}

template< typename TCoordRepType >
int
VoronoiDiagram2DGenerator< TCoordRepType >::PQbucket(FortuneHalfEdge *task)
{
  int bucket;

  bucket = (int)( ( task->m_Ystar - m_Pymin ) / m_Deltay * m_PQhashsize );
  if ( bucket < 0 )
    {
    bucket = 0;
    }
  if ( bucket >= static_cast< int >( m_PQhashsize ) )
    {
    bucket = m_PQhashsize - 1;
    }
  if ( bucket < static_cast< int >( m_PQmin ) )
    {
    m_PQmin = bucket;
    }
  return ( bucket );
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::insertPQ(FortuneHalfEdge *he, FortuneSite *v, double offset)
{
  he->m_Vert = v;
  he->m_Ystar = ( v->m_Coord[1] ) + offset;
  FortuneHalfEdge *last = &( m_PQHash[PQbucket(he)] );
  FortuneHalfEdge *enext;

  while ( ( ( enext = ( last->m_Next ) ) != ITK_NULLPTR )
          && ( ( ( he->m_Ystar ) > ( enext->m_Ystar ) )
               || ( ( Math::ExactlyEquals(( he->m_Ystar ), ( enext->m_Ystar )) )
                    && ( ( v->m_Coord[0] ) > ( enext->m_Vert->m_Coord[0] ) ) ) ) )
    {
    last = enext;
    }
  he->m_Next = ( last->m_Next );
  last->m_Next = he;
  m_PQcount += 1;
}

template< typename TCoordRepType >
double
VoronoiDiagram2DGenerator< TCoordRepType >::dist(FortuneSite *s1, FortuneSite *s2)
{
  double dx = ( s1->m_Coord[0] ) - ( s2->m_Coord[0] );
  double dy = ( s1->m_Coord[1] ) - ( s2->m_Coord[1] );

  return ( std::sqrt(dx * dx + dy * dy) );
}

template< typename TCoordRepType >
typename VoronoiDiagram2DGenerator< TCoordRepType >::FortuneHalfEdge *
VoronoiDiagram2DGenerator< TCoordRepType >::ELgethash(int b)
{
  if ( ( b < 0 ) || ( b >= static_cast< int >( m_ELhashsize ) ) )
    {
    return ( ITK_NULLPTR );
    }
  FortuneHalfEdge *he = m_ELHash[b];
  if ( he == ITK_NULLPTR )
    {
    return ( he );
    }
  if ( he->m_Edge == ITK_NULLPTR )
    {
    return ( he );
    }
  if ( ( he->m_Edge ) != ( &m_DELETED ) )
    {
    return ( he );
    }
  m_ELHash[b] = ITK_NULLPTR;

  return ( ITK_NULLPTR );
}

template< typename TCoordRepType >
typename VoronoiDiagram2DGenerator< TCoordRepType >::FortuneHalfEdge *
VoronoiDiagram2DGenerator< TCoordRepType >::findLeftHE(PointType *p)
{
  int i;
  int bucket = (int)( ( ( ( *p )[0] ) - m_Pxmin ) / m_Deltax * m_ELhashsize );

  if ( bucket < 0 )
    {
    bucket = 0;
    }
  if ( bucket >= static_cast< int >( m_ELhashsize ) )
    {
    bucket = static_cast< int >( m_ELhashsize ) - 1;
    }
  FortuneHalfEdge *he = ELgethash(bucket);
  if ( he == ITK_NULLPTR )
    {
    for ( i = 1; 1; i++ )
      {
      if ( ( he = ELgethash(bucket - i) ) != ITK_NULLPTR ) { break; }
      if ( ( he = ELgethash(bucket + i) ) != ITK_NULLPTR ) { break; }
      }
    }

  if ( ( he == ( &m_ELleftend ) ) || ( ( he != ( &m_ELrightend ) ) && right_of(he, p) ) )
    {
    do
      {
      he = he->m_Right;
      }
    while ( ( he != ( &m_ELrightend ) ) && ( right_of(he, p) ) );
    he = he->m_Left;
    }
  else
    {
    do
      {
      he = he->m_Left;
      }
    while ( ( he != ( &m_ELleftend ) ) && ( !right_of(he, p) ) );
    }

  if ( ( bucket > 0 ) && ( bucket < static_cast< int >( m_ELhashsize ) - 1 ) )
    {
    m_ELHash[bucket] = he;
    }
  return ( he );
}

template< typename TCoordRepType >
typename VoronoiDiagram2DGenerator< TCoordRepType >::FortuneSite *
VoronoiDiagram2DGenerator< TCoordRepType >::getRightReg(FortuneHalfEdge *he)
{
  if ( ( he->m_Edge ) == ITK_NULLPTR )
    {
    return ( m_BottomSite );
    }
  else if ( he->m_RorL )
    {
    return ( he->m_Edge->m_Reg[0] );
    }
  else
    {
    return ( he->m_Edge->m_Reg[1] );
    }
}

template< typename TCoordRepType >
typename VoronoiDiagram2DGenerator< TCoordRepType >::FortuneSite *
VoronoiDiagram2DGenerator< TCoordRepType >::getLeftReg(FortuneHalfEdge *he)
{
  if ( ( he->m_Edge ) == ITK_NULLPTR )
    {
    return ( m_BottomSite );
    }
  else if ( he->m_RorL )
    {
    return ( he->m_Edge->m_Reg[1] );
    }
  else
    {
    return ( he->m_Edge->m_Reg[0] );
    }
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::insertEdgeList(FortuneHalfEdge *lbase, FortuneHalfEdge *lnew)
{
  lnew->m_Left = lbase;
  lnew->m_Right = lbase->m_Right;
  ( lbase->m_Right )->m_Left = lnew;
  lbase->m_Right = lnew;
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::bisect(FortuneEdge *answer, FortuneSite *s1, FortuneSite *s2)
{
  answer->m_Reg[0] = s1;
  answer->m_Reg[1] = s2;
  answer->m_Ep[0] = ITK_NULLPTR;
  answer->m_Ep[1] = ITK_NULLPTR;

  double dx = ( s2->m_Coord[0] ) - ( s1->m_Coord[0] );
  double dy = ( s2->m_Coord[1] ) - ( s1->m_Coord[1] );
  double adx = ( dx > 0 ) ? dx : -dx;
  double ady = ( dy > 0 ) ? dy : -dy;

  answer->m_C = ( s1->m_Coord[0] ) * dx + ( s1->m_Coord[1] ) * dy + ( dx * dx + dy * dy ) * 0.5;
  if ( adx > ady )
    {
    answer->m_A = 1.0;
    answer->m_B = dy / dx;
    answer->m_C /= dx;
    }
  else
    {
    answer->m_A = dx / dy;
    answer->m_B = 1.0;
    answer->m_C /= dy;
    }
  answer->m_Edgenbr = m_Nedges;
  m_Nedges++;
  Point< int, 2 > outline;
  outline[0] = answer->m_Reg[0]->m_Sitenbr;
  outline[1] = answer->m_Reg[1]->m_Sitenbr;
  m_OutputVD->AddLine(outline);
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::intersect(FortuneSite *newV, FortuneHalfEdge *el1, FortuneHalfEdge *el2)
{
  FortuneEdge *    e1 = el1->m_Edge;
  FortuneEdge *    e2 = el2->m_Edge;
  FortuneHalfEdge *saveHE;
  FortuneEdge *    saveE;

  if ( e1 == ITK_NULLPTR )
    {
    newV->m_Sitenbr = -1;
    return;
    }
  if ( e2 == ITK_NULLPTR )
    {
    newV->m_Sitenbr = -2;
    return;
    }
  if ( ( e1->m_Reg[1] ) == ( e2->m_Reg[1] ) )
    {
    newV->m_Sitenbr = -3;
    return;
    }

  double d = ( e1->m_A ) * ( e2->m_B ) - ( e1->m_B ) * ( e2->m_A );

  if ( ( d > -NUMERIC_TOLERENCE ) && ( d < NUMERIC_TOLERENCE ) )
    {
    newV->m_Sitenbr = -4;
    return;
    }

  double xmeet = ( ( e1->m_C ) * ( e2->m_B ) - ( e2->m_C ) * ( e1->m_B ) ) / d;
  double ymeet = ( ( e2->m_C ) * ( e1->m_A ) - ( e1->m_C ) * ( e2->m_A ) ) / d;

  if ( comp(e1->m_Reg[1]->m_Coord, e2->m_Reg[1]->m_Coord) )
    {
    saveHE = el1;
    saveE = e1;
    }
  else
    {
    saveHE = el2;
    saveE = e2;
    }

  bool right_of_site = ( xmeet >= ( saveE->m_Reg[1]->m_Coord[0] ) );
  if ( ( right_of_site && ( !( saveHE->m_RorL ) ) )
       || ( ( !right_of_site ) && ( saveHE->m_RorL ) ) )
    {
    newV->m_Sitenbr = -4;
    return;
    }

  newV->m_Coord[0] = xmeet;
  newV->m_Coord[1] = ymeet;
  newV->m_Sitenbr = -5;
}

template< typename TCoordRepType >
typename VoronoiDiagram2DGenerator< TCoordRepType >::FortuneHalfEdge *
VoronoiDiagram2DGenerator< TCoordRepType >::getPQmin(void)
{
  FortuneHalfEdge *curr = m_PQHash[m_PQmin].m_Next;

  m_PQHash[m_PQmin].m_Next = curr->m_Next;
  m_PQcount--;
  return ( curr );
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::clip_line(FortuneEdge *task)
{
  // Clip line
  FortuneSite *s1;
  FortuneSite *s2;
  double       x1, y1, x2, y2;

  if ( ( ( task->m_A ) == 1.0 ) && ( ( task->m_B ) >= 0.0 ) )
    {
    s1 = task->m_Ep[1];
    s2 = task->m_Ep[0];
    }
  else
    {
    s2 = task->m_Ep[1];
    s1 = task->m_Ep[0];
    }

  int id1;
  int id2;
  if ( ( task->m_A ) == 1.0 )
    {
    if ( ( s1 != ITK_NULLPTR ) && ( ( s1->m_Coord[1] ) > m_Pymin ) )
      {
      y1 = s1->m_Coord[1];
      if ( y1 > m_Pymax )
        {
        return;
        }
      x1 = s1->m_Coord[0];
      id1 = s1->m_Sitenbr;
      }
    else
      {
      y1 = m_Pymin;
      x1 = ( task->m_C ) - ( task->m_B ) * y1;
      id1 = -1;
      }

    if ( ( s2 != ITK_NULLPTR ) && ( ( s2->m_Coord[1] ) < m_Pymax ) )
      {
      y2 = s2->m_Coord[1];
      if ( y2 < m_Pymin )
        {
        return;
        }
      x2 = s2->m_Coord[0];
      id2 = s2->m_Sitenbr;
      }
    else
      {
      y2 = m_Pymax;
      x2 = ( task->m_C ) - ( task->m_B ) * y2;
      id2 = -1;
      }

    if ( ( x1 > m_Pxmax ) && ( x2 > m_Pxmax ) )
      {
      return;
      }
    if ( ( x1 < m_Pxmin ) && ( x2 < m_Pxmin ) )
      {
      return;
      }
    if ( x1 > m_Pxmax )
      {
      x1 = m_Pxmax;
      y1 = ( ( task->m_C ) - x1 ) / ( task->m_B );
      id1 = -1;
      }
    if ( x1 < m_Pxmin )
      {
      x1 = m_Pxmin;
      y1 = ( ( task->m_C ) - x1 ) / ( task->m_B );
      id1 = -1;
      }
    if ( x2 > m_Pxmax )
      {
      x2 = m_Pxmax;
      y2 = ( ( task->m_C ) - x2 ) / ( task->m_B );
      id2 = -1;
      }
    if ( x2 < m_Pxmin )
      {
      x2 = m_Pxmin;
      y2 = ( ( task->m_C ) - x2 ) / ( task->m_B );
      id2 = -1;
      }
    }
  else
    {
    if ( ( s1 != ITK_NULLPTR ) && ( ( s1->m_Coord[0] ) > m_Pxmin ) )
      {
      x1 = s1->m_Coord[0];
      if ( x1 > m_Pxmax )
        {
        return;
        }
      y1 = s1->m_Coord[1];
      id1 = s1->m_Sitenbr;
      }
    else
      {
      x1 = m_Pxmin;
      y1 = ( task->m_C ) - ( task->m_A ) * x1;
      id1 = -1;
      }
    if ( ( s2 != ITK_NULLPTR ) && ( ( s2->m_Coord[0] ) < m_Pxmax ) )
      {
      x2 = s2->m_Coord[0];
      if ( x2 < m_Pxmin )
        {
        return;
        }
      y2 = s2->m_Coord[1];
      id2 = s2->m_Sitenbr;
      }
    else
      {
      x2 = m_Pxmax;
      y2 = ( task->m_C ) - ( task->m_A ) * x2;
      id2 = -1;
      }
    if ( ( y1 > m_Pymax ) && ( y2 > m_Pymax ) )
      {
      return;
      }
    if ( ( y1 < m_Pymin ) && ( y2 < m_Pymin ) )
      {
      return;
      }
    if ( y1 > m_Pymax )
      {
      y1 = m_Pymax;
      x1 = ( ( task->m_C ) - y1 ) / ( task->m_A );
      id1 = -1;
      }
    if ( y1 < m_Pymin )
      {
      y1 = m_Pymin;
      x1 = ( ( task->m_C ) - y1 ) / ( task->m_A );
      id1 = -1;
      }
    if ( y2 > m_Pymax )
      {
      y2 = m_Pymax;
      x2 = ( ( task->m_C ) - y2 ) / ( task->m_A );
      id2 = -1;
      }
    if ( y2 < m_Pymin )
      {
      y2 = m_Pymin;
      x2 = ( ( task->m_C ) - y2 ) / ( task->m_A );
      id2 = -1;
      }
    }

  VoronoiEdge newInfo;
  newInfo.m_Left[0] = x1;
  newInfo.m_Left[1] = y1;
  newInfo.m_Right[0] = x2;
  newInfo.m_Right[1] = y2;
  newInfo.m_LineID = task->m_Edgenbr;

  if ( id1 > -1 )
    {
    newInfo.m_LeftID = id1;
    }
  else
    {
    newInfo.m_LeftID = m_Nvert;
    m_Nvert++;
    PointType newv;
    newv[0] = x1;
    newv[1] = y1;
    m_OutputVD->AddVert(newv);
    }

  if ( id2 > -1 )
    {
    newInfo.m_RightID = id2;
    }
  else
    {
    newInfo.m_RightID = m_Nvert;
    m_Nvert++;
    PointType newv;
    newv[0] = x2;
    newv[1] = y2;
    m_OutputVD->AddVert(newv);
    }
  m_OutputVD->AddEdge(newInfo);
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::makeEndPoint(FortuneEdge *task, bool lr, FortuneSite *ends)
{
  task->m_Ep[lr] = ends;
  if ( ( task->m_Ep[1 - lr] ) == ITK_NULLPTR )
    {
    return;
    }

  clip_line(task);
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::GenerateVDFortune(void)
{
  unsigned int i;

  // Build seed sites
  m_SeedSites.resize(m_NumberOfSeeds);
  for ( i = 0; i < m_NumberOfSeeds; i++ )
    {
    m_SeedSites[i].m_Coord = m_Seeds[i];
    m_SeedSites[i].m_Sitenbr = i;
    }
  // Initialize boundary
  m_Pxmax = m_VorBoundary[0];
  m_Pymax = m_VorBoundary[1];

  m_Deltay = m_Pymax - m_Pymin;
  m_Deltax = m_Pxmax - m_Pxmin;
  m_SqrtNSites = std::sqrt( (float)( m_NumberOfSeeds + 4 ) );

  // Initialize outputLists
  m_Nedges = 0;
  m_Nvert = 0;
  m_OutputVD->LineListClear();
  m_OutputVD->EdgeListClear();
  m_OutputVD->VertexListClear();

  // Initialize the hash table for circle event and point event
  m_PQcount = 0;
  m_PQmin = 0;
  m_PQhashsize = (int)( 4 * m_SqrtNSites );
  m_PQHash.resize(m_PQhashsize);
  for ( i = 0; i < m_PQhashsize; i++ )
    {
    m_PQHash[i].m_Next = ITK_NULLPTR;
    }
  m_ELhashsize = (int)( 2 * m_SqrtNSites );
  m_ELHash.resize(m_ELhashsize);
  for ( i = 0; i < m_ELhashsize; i++ )
    {
    m_ELHash[i] = ITK_NULLPTR;
    }
  createHalfEdge(&( m_ELleftend ), ITK_NULLPTR, 0);
  createHalfEdge(&( m_ELrightend ), ITK_NULLPTR, 0);
  m_ELleftend.m_Left = ITK_NULLPTR;
  m_ELleftend.m_Right = &( m_ELrightend );
  m_ELrightend.m_Left = &( m_ELleftend );
  m_ELrightend.m_Right = ITK_NULLPTR;
  m_ELHash[0] = &( m_ELleftend );
  m_ELHash[m_ELhashsize - 1] = &( m_ELrightend );

  m_BottomSite = &( m_SeedSites[0] );
  FortuneSite *currentSite = &( m_SeedSites[1] );

  PointType currentCircle;
  currentCircle.Fill(0.0);
  FortuneHalfEdge *leftHalfEdge;
  FortuneHalfEdge *rightHalfEdge;
  FortuneHalfEdge *left2HalfEdge;
  FortuneHalfEdge *right2HalfEdge;
  FortuneHalfEdge *newHE;
  FortuneSite *    findSite;
  FortuneSite *    topSite;
  FortuneSite *    saveSite;
  FortuneEdge *    newEdge;
  FortuneSite *    meetSite;
  FortuneSite *    newVert;
  bool             saveBool;

  std::vector< FortuneHalfEdge > HEpool;
  std::vector< FortuneEdge >     Edgepool;
  std::vector< FortuneSite >     Sitepool;
  HEpool.resize(5 * m_NumberOfSeeds);
  Edgepool.resize(5 * m_NumberOfSeeds);
  Sitepool.resize(5 * m_NumberOfSeeds);

  int HEid = 0;
  int Edgeid = 0;
  int Siteid = 0;

  i = 2;
  bool ok = 1;
  while ( ok )
    {
    if ( m_PQcount != 0 )
      {
      PQshowMin(&currentCircle);
      }
    if ( ( i <= m_NumberOfSeeds ) && ( ( m_PQcount == 0 ) || comp(currentSite->m_Coord, currentCircle) ) )
      {
      // Handling site event
      leftHalfEdge = findLeftHE( &( currentSite->m_Coord ) );
      rightHalfEdge = leftHalfEdge->m_Right;

      findSite = getRightReg(leftHalfEdge);

      bisect(&( Edgepool[Edgeid] ), findSite, currentSite);
      newEdge = &( Edgepool[Edgeid] );
      Edgeid++;

      createHalfEdge(&( HEpool[HEid] ), newEdge, 0);
      newHE = &( HEpool[HEid] );
      HEid++;

      insertEdgeList(leftHalfEdge, newHE);

      intersect(&( Sitepool[Siteid] ), leftHalfEdge, newHE);
      meetSite = &( Sitepool[Siteid] );

      if ( ( meetSite->m_Sitenbr ) == -5 )
        {
        deletePQ(leftHalfEdge);
        insertPQ( leftHalfEdge, meetSite, dist(meetSite, currentSite) );
        Siteid++;
        }

      leftHalfEdge = newHE;
      createHalfEdge(&( HEpool[HEid] ), newEdge, 1);
      newHE = &( HEpool[HEid] );
      HEid++;

      insertEdgeList(leftHalfEdge, newHE);

      intersect(&( Sitepool[Siteid] ), newHE, rightHalfEdge);
      meetSite = &( Sitepool[Siteid] );
      if ( ( meetSite->m_Sitenbr ) == -5 )
        {
        Siteid++;
        insertPQ( newHE, meetSite, dist(meetSite, currentSite) );
        }
      if ( i < m_SeedSites.size() )
        {
        currentSite = &( m_SeedSites[i] );
        }
      i++;
      }
    else if ( m_PQcount != 0 )
      {
      // Handling circle event

      leftHalfEdge = getPQmin();
      left2HalfEdge = leftHalfEdge->m_Left;
      rightHalfEdge = leftHalfEdge->m_Right;
      right2HalfEdge = rightHalfEdge->m_Right;
      findSite = getLeftReg(leftHalfEdge);
      topSite = getRightReg(rightHalfEdge);

      newVert = leftHalfEdge->m_Vert;
      newVert->m_Sitenbr = m_Nvert;
      m_Nvert++;
      m_OutputVD->AddVert(newVert->m_Coord);

      makeEndPoint(leftHalfEdge->m_Edge, leftHalfEdge->m_RorL, newVert);
      makeEndPoint(rightHalfEdge->m_Edge, rightHalfEdge->m_RorL, newVert);
      deleteEdgeList(leftHalfEdge);
      deletePQ(rightHalfEdge);
      deleteEdgeList(rightHalfEdge);

      saveBool = 0;
      if ( ( findSite->m_Coord[1] ) > ( topSite->m_Coord[1] ) )
        {
        saveSite = findSite;
        findSite = topSite;
        topSite = saveSite;
        saveBool = 1;
        }

      bisect(&( Edgepool[Edgeid] ), findSite, topSite);
      newEdge = &( Edgepool[Edgeid] );
      Edgeid++;

      createHalfEdge(&( HEpool[HEid] ), newEdge, saveBool);
      newHE = &( HEpool[HEid] );
      HEid++;

      insertEdgeList(left2HalfEdge, newHE);
      makeEndPoint(newEdge, 1 - saveBool, newVert);

      intersect(&( Sitepool[Siteid] ), left2HalfEdge, newHE);
      meetSite = &( Sitepool[Siteid] );

      if ( ( meetSite->m_Sitenbr ) == -5 )
        {
        Siteid++;
        deletePQ(left2HalfEdge);
        insertPQ( left2HalfEdge, meetSite, dist(meetSite, findSite) );
        }

      intersect(&( Sitepool[Siteid] ), newHE, right2HalfEdge);
      meetSite = &( Sitepool[Siteid] );
      if ( ( meetSite->m_Sitenbr ) == -5 )
        {
        Siteid++;
        insertPQ( newHE, meetSite, dist(meetSite, findSite) );
        }
      }
    else
      {
      ok = 0;
      }
    }

  for ( ( leftHalfEdge = m_ELleftend.m_Right );
        ( leftHalfEdge != ( &m_ELrightend ) );
        ( leftHalfEdge = ( leftHalfEdge->m_Right ) ) )
    {
    newEdge = leftHalfEdge->m_Edge;
    clip_line(newEdge);
    }
}

template< typename TCoordRepType >
void
VoronoiDiagram2DGenerator< TCoordRepType >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent  );

  os << indent << "Number Of Seeds: " << m_NumberOfSeeds << std::endl;
  os << indent << "VorBoundary: " <<
    static_cast< typename NumericTraits< PointType >::PrintType >( m_VorBoundary )
    << std::endl;
  os << indent << "OutputVD: " <<
    static_cast< typename NumericTraits< OutputType >::PrintType >( m_OutputVD )
    << std::endl;

  os << indent << "Pxmin: " << m_Pxmin << std::endl;
  os << indent << "Pxmax: " << m_Pxmax << std::endl;
  os << indent << "Pymin: " << m_Pymin << std::endl;
  os << indent << "Pymax: " << m_Pymax << std::endl;
  os << indent << "Deltax: " << m_Deltax << std::endl;
  os << indent << "Deltay: " << m_Deltay << std::endl;
  os << indent << "SqrtNSites: " << m_SqrtNSites << std::endl;

  os << indent << "PQcount: " << m_PQcount << std::endl;
  os << indent << "PQmin: " << m_PQmin << std::endl;
  os << indent << "PQhashsize: " << m_PQhashsize << std::endl;
  os << indent << "Nedges: " << m_Nedges << std::endl;
  os << indent << "Nvert: " << m_Nvert << std::endl;
  os << indent << "BottomSite: " << m_BottomSite << std::endl;

  os << indent << "ELhashsize: " << m_ELhashsize << std::endl;

  os << indent << "ELHash: " << std::endl;
  for( unsigned int i = 0; i < m_ELHash.size(); ++i )
    {
    os << indent << "[" << i << "]: " << m_ELHash[i] << std::endl;
    }
}
} //end namespace

#endif
