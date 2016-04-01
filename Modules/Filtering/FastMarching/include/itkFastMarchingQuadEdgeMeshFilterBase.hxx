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

#ifndef itkFastMarchingQuadEdgeMeshFilterBase_hxx
#define itkFastMarchingQuadEdgeMeshFilterBase_hxx

#include "itkFastMarchingQuadEdgeMeshFilterBase.h"

#include "itkMath.h"
#include "itkVector.h"
#include "itkMatrix.h"

namespace itk
{
template< typename TInput, typename TOutput >
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::FastMarchingQuadEdgeMeshFilterBase() : Superclass()
{
  this->m_InputMesh = ITK_NULLPTR;
}

template< typename TInput, typename TOutput >
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::~FastMarchingQuadEdgeMeshFilterBase()
{}

template< typename TInput, typename TOutput >
IdentifierType
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::GetTotalNumberOfNodes() const
{
  return this->GetInput()->GetNumberOfPoints();
}

template< typename TInput, typename TOutput >
void
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::SetOutputValue( OutputMeshType* oMesh,
                  const NodeType& iNode,
                  const OutputPixelType& iValue )
{
  oMesh->SetPointData( iNode, iValue );
}

template< typename TInput, typename TOutput >
const typename
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::OutputPixelType
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::GetOutputValue( OutputMeshType* oMesh, const NodeType& iNode ) const
{
  OutputPixelType outputValue = NumericTraits< OutputPixelType >::ZeroValue();
  oMesh->GetPointData( iNode, &outputValue );
  return outputValue;
}


template< typename TInput, typename TOutput >
unsigned char
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::GetLabelValueForGivenNode( const NodeType& iNode ) const
{
  NodeLabelMapConstIterator it = m_Label.find( iNode );

  if( it != m_Label.end() )
    {
    return it->second;
    }
  else
    {
    return Traits::Far;
    }
}

template< typename TInput, typename TOutput >
void
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::SetLabelValueForGivenNode( const NodeType& iNode,
                             const LabelType& iLabel )
{
   m_Label[iNode] = iLabel;
}

template< typename TInput, typename TOutput >
void
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::UpdateNeighbors( OutputMeshType* oMesh,
                   const NodeType& iNode )
{
  OutputPointType p;
  oMesh->GetPoint( iNode, &p );

  OutputQEType* qe = p.GetEdge();

  if( qe )
    {
    OutputQEType *qe_it = qe;
    this->m_InputMesh = this->GetInput();
    do
      {
      if( qe_it )
        {
        OutputPointIdentifierType neigh_id = qe_it->GetDestination();

        const char label = this->GetLabelValueForGivenNode( neigh_id );

        if ( ( label != Traits::Alive ) &&
             ( label != Traits::InitialTrial ) &&
             ( label != Traits::Forbidden ) )
          {
          this->UpdateValue( oMesh, neigh_id );
          }
        }
      else
        {
        itkGenericExceptionMacro( <<"qe_it is ITK_NULLPTR" );
        }
      qe_it = qe_it->GetOnext();
      }
    while( qe_it != qe );
    }
  else
    {
    itkGenericExceptionMacro( <<"qe is ITK_NULLPTR" );
    }
}

template< typename TInput, typename TOutput >
void
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::UpdateValue( OutputMeshType* oMesh,
               const NodeType& iNode )
  {
  OutputPointType p;
  oMesh->GetPoint( iNode, &p );

  InputPixelType F = NumericTraits< InputPixelType >::ZeroValue();
  this->m_InputMesh->GetPointData( iNode, &F );

  if( F < 0. )
    {
    itkGenericExceptionMacro( << "F < 0." );
    }

  OutputQEType* qe = p.GetEdge();

  OutputPixelType outputPixel = this->m_LargeValue;

  if( qe )
    {
    OutputQEType *qe_it = qe;
    qe_it = qe_it->GetOnext();

    do
      {
      OutputQEType *qe_it2 = qe_it->GetOnext();

      if( qe_it2 )
        {
        if( qe_it->GetLeft() != OutputMeshType::m_NoFace )
          {
          OutputPointIdentifierType id1 = qe_it->GetDestination();
          OutputPointIdentifierType id2 = qe_it2->GetDestination();

          const LabelType label1 =
              static_cast< LabelType >( this->GetLabelValueForGivenNode( id1 ) );
          const LabelType label2 =
              static_cast< LabelType >( this->GetLabelValueForGivenNode( id2 ) );

          bool IsFar1 = ( label1 != Traits::Far );
          bool IsFar2 = ( label2 != Traits::Far );

          if( IsFar1 || IsFar2 )
            {
            OutputPointType q1 = oMesh->GetPoint( id1 );
            OutputPointType q2 = oMesh->GetPoint( id2 );

            OutputVectorRealType val1 =
                static_cast< OutputVectorRealType >(
                  this->GetOutputValue( oMesh, id1 ) );

            OutputVectorRealType val2 =
                static_cast< OutputVectorRealType >(
                  this->GetOutputValue( oMesh, id2 ) );

            if( val1 > val2 )
              {
              OutputPointType temp_pt( q1 );
              q1 = q2;
              q2 = temp_pt;

              std::swap( val1, val2 );
              std::swap( IsFar1, IsFar2 );
              std::swap( id1, id2 );
              }

            const OutputVectorRealType temp =
                this->Solve( oMesh, iNode, p, F,
                            id1, q1, IsFar1, val1,
                            id2, q2, IsFar2, val2 );

            outputPixel =
                std::min( outputPixel,
                              static_cast< OutputPixelType >( temp ) );
            }
          }

        qe_it = qe_it2;
        }
      else
        {
        // throw one exception here
        itkGenericExceptionMacro( << "qe_it2 is ITK_NULLPTR" );
        }
      }
    while( qe_it != qe );

    if( outputPixel < this->m_LargeValue )
      {
      this->SetOutputValue( oMesh, iNode, outputPixel );

      this->SetLabelValueForGivenNode( iNode, Traits::Trial );

      this->m_Heap.push( NodePairType( iNode, outputPixel ) );
      }
    }
  else
    {
    // throw one exception
    itkGenericExceptionMacro( << "qe_it is ITK_NULLPTR" );
    }
  }

template< typename TInput, typename TOutput >
const typename
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::OutputVectorRealType
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::Solve( OutputMeshType* oMesh,
       const NodeType& iId, const OutputPointType& iCurrentPoint,
       const OutputVectorRealType& iF,
       const NodeType& iId1, const OutputPointType& iP1,
       const bool& iIsFar1, const OutputVectorRealType iVal1,
       const NodeType& iId2, const OutputPointType& iP2,
       const bool& iIsFar2, const OutputVectorRealType& iVal2 ) const
  {
  OutputVectorType Edge1 = iP1 - iCurrentPoint;
  OutputVectorType Edge2 = iP2 - iCurrentPoint;

  OutputVectorRealType sq_norm1 = Edge1.GetSquaredNorm();
  OutputVectorRealType norm1 = 0.;

  OutputVectorRealType epsilon =
      NumericTraits< OutputVectorRealType >::epsilon();

  if( sq_norm1 > epsilon )
    {
    norm1 = std::sqrt( sq_norm1 );

    OutputVectorRealType inv_norm1 = 1. / norm1;
    Edge1 *= inv_norm1;
    }

  OutputVectorRealType sq_norm2 = Edge2.GetSquaredNorm();
  OutputVectorRealType norm2 = 0.;

  if( sq_norm2 > epsilon )
    {
    norm2 = std::sqrt( sq_norm2 );

    OutputVectorRealType inv_norm2 = 1. / norm2;
    Edge2 *= inv_norm2;
    }

  if( !iIsFar1 && iIsFar2 )
    {
    // only one point is a contributor
    return iVal2 + norm2 * iF;
    }
  if( iIsFar1 && !iIsFar2 )
    {
    // only one point is a contributor
    return iVal1 + norm1 * iF;
    }

  if( iIsFar1 && iIsFar2 )
    {
    OutputVectorRealType dot =
        static_cast< OutputVectorRealType >( Edge1 * Edge2 );

    if( dot >= 0. )
      {
      return ComputeUpdate( iVal1, iVal2,
                           norm2, sq_norm2,
                           norm1, sq_norm1, dot, iF );
      }
    else
      {
      OutputVectorRealType sq_norm3, norm3, dot1, dot2;
      OutputPointIdentifierType new_id;

      bool unfolded =
        UnfoldTriangle( oMesh, iId, iCurrentPoint, iId1, iP1,
                        iId2, iP2, norm3, sq_norm3, dot1, dot2 , new_id );

      if( unfolded )
        {
        OutputVectorRealType t_sq_norm3 = norm3 * norm3;

        OutputVectorRealType val3 =
            static_cast< OutputVectorRealType >(
              this->GetOutputValue( oMesh, new_id ) );
        OutputVectorRealType t1 = ComputeUpdate( iVal1, val3, norm3, t_sq_norm3,
                                                norm1, sq_norm1, dot1, iF );
        OutputVectorRealType t2 = ComputeUpdate( iVal2, val3, norm3, t_sq_norm3,
                                                norm2, sq_norm2, dot2, iF );

        return std::min( t1, t2 );
        }
      else
        {
        return ComputeUpdate( iVal1, iVal2,
                             norm2, sq_norm2,
                             norm1, sq_norm1, dot, iF );
        }
      }


    }

  return this->m_LargeValue;
  }

template< typename TInput, typename TOutput >
const typename
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::OutputVectorRealType
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::ComputeUpdate(
  const OutputVectorRealType& iVal1, const OutputVectorRealType& iVal2,
  const OutputVectorRealType& iNorm1, const OutputVectorRealType& iSqNorm1,
  const OutputVectorRealType& iNorm2, const OutputVectorRealType& iSqNorm2,
  const OutputVectorRealType& iDot, const OutputVectorRealType& iF )
  const
{
  OutputVectorRealType large_value =
      static_cast< OutputVectorRealType >( this->m_LargeValue );

  OutputVectorRealType t = large_value;

  OutputVectorRealType CosAngle = iDot;
  OutputVectorRealType SinAngle = std::sqrt( 1. - iDot * iDot );

  OutputVectorRealType u = iVal2 - iVal1;

  OutputVectorRealType sq_u = u * u;
  OutputVectorRealType f2 = iSqNorm1 + iSqNorm2 - 2. * iNorm1 * iNorm2 * CosAngle;
  OutputVectorRealType f1 = iNorm2 * u * ( iNorm1 * CosAngle - iNorm2 );
  OutputVectorRealType f0 = iSqNorm2 * ( sq_u - iF * iF * iSqNorm1 * SinAngle * SinAngle );

  OutputVectorRealType delta = f1 * f1 - f0 * f2;

  OutputVectorRealType epsilon =
      NumericTraits< OutputVectorRealType >::epsilon();

  if( delta >= 0. )
    {
    if( itk::Math::abs( f2 ) > epsilon )
      {
      t = ( -f1 - std::sqrt( delta ) ) / f2;

      // test if we must must choose the other solution
      if( ( t < u ) ||
          ( iNorm2 * ( t - u ) / t < iNorm1 * CosAngle ) ||
          ( iNorm1 / CosAngle < iNorm2 * ( t - u ) / t ) )
        {
        t = ( -f1 + std::sqrt( delta ) ) / f2;
        }
      }
    else
      {
      if( itk::Math::abs( f1 ) > epsilon )
        {
        t = -f0 / f1;
        }
      else
        {
        t = - large_value;
        }
      }
    }
  else
    {
    t = - large_value;
    }

  // choose the update from the 2 vertex only if upwind criterion is met
  if( ( u < t ) &&
      ( iNorm1 * CosAngle < iNorm2 * ( t - u ) / t ) &&
      ( iNorm2 * ( t - u ) / t < iNorm1 / CosAngle ) )
    {
    return t + iVal1;
    }
  else
    {
    return std::min( iNorm2 * iF + iVal1, iNorm1 * iF + iVal2 );
    }
}

template< typename TInput, typename TOutput >
bool
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::UnfoldTriangle(
  OutputMeshType* oMesh,
  const OutputPointIdentifierType& iId, const OutputPointType& iP,
  const OutputPointIdentifierType& iId1, const OutputPointType& iP1,
  const OutputPointIdentifierType& iId2, const OutputPointType &iP2,
  OutputVectorRealType& oNorm, OutputVectorRealType& oSqNorm,
  OutputVectorRealType& oDot1, OutputVectorRealType& oDot2,
  OutputPointIdentifierType& oId ) const
  {
  (void) iId;

  OutputVectorType Edge1 = iP1 - iP;
  OutputVectorRealType Norm1 = Edge1.GetNorm();

  OutputVectorRealType epsilon =
    NumericTraits< OutputVectorRealType >::epsilon();

  if( Norm1 > epsilon )
    {
    OutputVectorRealType inv_Norm = 1. / Norm1;
    Edge1 *= inv_Norm;
    }

  OutputVectorType Edge2 = iP2 - iP;
  OutputVectorRealType Norm2 = Edge2.GetNorm();
  if( Norm2 > epsilon )
    {
    OutputVectorRealType inv_Norm = 1. / Norm2;
    Edge2 *= inv_Norm;
    }

  OutputVectorRealType dot =
      static_cast< OutputVectorRealType >( Edge1 * Edge2 );

  // the equation of the lines defining the unfolding region
  // [e.g. line 1 : {x ; <x,eq1>=0} ]
  typedef Vector< OutputVectorRealType, 2 >     Vector2DType;
  typedef Matrix< OutputVectorRealType, 2, 2 >  Matrix2DType;

  Vector2DType v1;
  v1[0] = dot;
  v1[1] = std::sqrt( 1. - dot * dot );

  Vector2DType v2;
  v2[0] = 1.;
  v2[1] = 0.;

  Vector2DType x1;
  x1[0] = Norm1;
  x1[1] = 0.;

  Vector2DType x2 = Norm2 * v1;

  // keep track of the starting point
  Vector2DType x_start1( x1 );
  Vector2DType x_start2( x2 );

  OutputPointIdentifierType id1 = iId1;
  OutputPointIdentifierType id2 = iId2;

  OutputQEType *qe = oMesh->FindEdge( id1, id2 );
  qe = qe->GetSym();

  OutputPointType t_pt1 = iP1;
  OutputPointType t_pt2 = iP2;
  OutputPointType t_pt = t_pt1;

  unsigned int nNum = 0;
  while( nNum<50 && qe->GetLeft() != OutputMeshType::m_NoFace )
    {
    OutputQEType* qe_Lnext = qe->GetLnext();
    OutputPointIdentifierType t_id = qe_Lnext->GetDestination();

    oMesh->GetPoint( t_id, &t_pt );

    Edge1 = t_pt2 - t_pt1;
    Norm1 = Edge1.GetNorm();
    if( Norm1 > epsilon )
      {
      OutputVectorRealType inv_Norm = 1. / Norm1;
      Edge1 *= inv_Norm;
      }

    Edge2 = t_pt - t_pt1;
    Norm2 = Edge2.GetNorm();
    if( Norm2 > epsilon )
      {
      OutputVectorRealType inv_Norm = 1. / Norm2;
      Edge2 *= inv_Norm;
      }

    /* compute the position of the new point x on the unfolding plane (via a rotation of -alpha on (x2-x1)/rNorm1 )
            | cos(alpha) sin(alpha)|
        x = |-sin(alpha) cos(alpha)| * [x2-x1]*rNorm2/rNorm1 + x1   where cos(alpha)=dot
    */
    Vector2DType vv;
    if( Norm1 > epsilon )
      {
      vv = (x2 - x1) * Norm2 / Norm1;
      }
    else
      {
      vv.Fill( 0. );
      }

    dot = Edge1 * Edge2;

    Matrix2DType rotation;
    rotation[0][0] = dot;
    rotation[0][1] = std::sqrt( 1. - dot * dot );
    rotation[1][0] = - rotation[0][1];
    rotation[1][1] = dot;

    Vector2DType x = rotation * vv + x1;


    /* compute the intersection points.
       We look for x=x1+lambda*(x-x1) or x=x2+lambda*(x-x2) with <x,eqi>=0, so */
    OutputVectorRealType lambda11 = - ( x1 * v1 ) / ( ( x - x1 ) * v1 );  // left most
    OutputVectorRealType lambda12 = - ( x1 * v2 ) / ( ( x - x1 ) * v2 );  // right most
    OutputVectorRealType lambda21 = - ( x2 * v1 ) / ( ( x - x2 ) * v1 );  // left most
    OutputVectorRealType lambda22 = - ( x2 * v2 ) / ( ( x - x2 ) * v2 );  // right most
    bool bIntersect11 = (lambda11>=0.) && (lambda11<=1.);
    bool bIntersect12 = (lambda12>=0.) && (lambda12<=1.);
    bool bIntersect21 = (lambda21>=0.) && (lambda21<=1.);
    bool bIntersect22 = (lambda22>=0.) && (lambda22<=1.);

    if( bIntersect11 && bIntersect12 )
      {
      qe = oMesh->FindEdge( id1, t_id );
      qe = qe->GetSym();
      id2 = t_id;
      t_pt2 = t_pt;
      x2 = x;
      }
    else
      {
      if( bIntersect21 && bIntersect22 )
        {
        qe = oMesh->FindEdge( id2, t_id );
        qe = qe->GetSym();
        id1 = t_id;
        t_pt1 = t_pt;
        x1 = x;
        }
      else
        { // bIntersect11 && !bIntersect12 && !bIntersect21 && bIntersect22

        // that's it, we have found the point
        oSqNorm = x.GetSquaredNorm();

        if( oSqNorm > epsilon )
          {
          oNorm = std::sqrt( oSqNorm );
          OutputVectorRealType temp_norm = x_start1.GetNorm();
          if( temp_norm > epsilon )
            {
            oDot1 = x * x_start1 / ( oNorm * temp_norm );
            }
          else
            {
            oDot1 = 0.;
            }
          temp_norm = x_start2.GetNorm();
          if( temp_norm > epsilon )
            {
            oDot2 = x * x_start2 / ( oNorm * temp_norm );
            }
          else
            {
            oDot2 = 0.;
            }
          }
        else
          {
          oNorm = 0.;
          oDot1 = 0.;
          oDot2 = 0.;
          }

        oId = t_id;
        return true;
        }
      }
    ++nNum;
    }

    return false;
  }

template< typename TInput, typename TOutput >
bool
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::CheckTopology( OutputMeshType* oMesh,
                    const NodeType& iNode )
  {
  (void) oMesh;
  (void) iNode;

  return true;
  }

template< typename TInput, typename TOutput >
void
FastMarchingQuadEdgeMeshFilterBase< TInput, TOutput >
::InitializeOutput( OutputMeshType* oMesh )
  {
  this->CopyInputMeshToOutputMeshGeometry();

  // Check that the input mesh is made of triangles
    {
    OutputCellsContainerPointer cells = oMesh->GetCells();
    OutputCellsContainerConstIterator c_it = cells->Begin();
    OutputCellsContainerConstIterator c_end = cells->End();

    while( c_it != c_end )
      {
      OutputCellType* cell = c_it.Value();

      if( cell->GetNumberOfPoints() != 3 )
        {
        itkGenericExceptionMacro( << "Input mesh has non triangular faces" );
        }
      ++c_it;
      }
    }

  OutputPointsContainerPointer points = oMesh->GetPoints();

  OutputPointDataContainerPointer pointdata =
      OutputPointDataContainer::New();
  pointdata->Reserve( points->Size() );

  OutputPointsContainerIterator p_it = points->Begin();
  OutputPointsContainerIterator p_end = points->End();

  while( p_it != p_end )
    {
    pointdata->SetElement( p_it->Index(), this->m_LargeValue );
    ++p_it;
    }

  oMesh->SetPointData( pointdata );

  m_Label.clear();

  if ( this->m_AlivePoints )
    {
    NodePairContainerConstIterator pointsIter = this->m_AlivePoints->Begin();
    NodePairContainerConstIterator pointsEnd = this->m_AlivePoints->End();

    while( pointsIter != pointsEnd )
      {
      // get node from alive points container
      NodeType idx = pointsIter->Value().GetNode();

      if( points->IndexExists( idx ) )
        {
        OutputPixelType outputPixel = pointsIter->Value().GetValue();

        this->SetLabelValueForGivenNode( idx, Traits::Alive );
        this->SetOutputValue( oMesh, idx, outputPixel );
        }

      ++pointsIter;
      }
    }
  if( this->m_ForbiddenPoints )
    {
    NodePairContainerConstIterator pointsIter = this->m_ForbiddenPoints->Begin();
    NodePairContainerConstIterator pointsEnd = this->m_ForbiddenPoints->End();

    OutputPixelType zero = NumericTraits< OutputPixelType >::ZeroValue();

    while( pointsIter != pointsEnd )
      {
      NodeType idx = pointsIter->Value().GetNode();

      if( points->IndexExists( idx ) )
        {
        this->SetLabelValueForGivenNode( idx, Traits::Forbidden );
        this->SetOutputValue( oMesh, idx, zero );
        }

      ++pointsIter;
      }
    }
  if ( this->m_TrialPoints )
    {
    NodePairContainerConstIterator pointsIter = this->m_TrialPoints->Begin();
    NodePairContainerConstIterator pointsEnd = this->m_TrialPoints->End();

    while( pointsIter != pointsEnd )
      {
      NodeType idx = pointsIter->Value().GetNode();

      if( points->IndexExists( idx ) )
        {
        OutputPixelType outputPixel = pointsIter->Value().GetValue();

        this->SetLabelValueForGivenNode( idx, Traits::InitialTrial );
        this->SetOutputValue( oMesh, idx, outputPixel );

        this->m_Heap.push( pointsIter->Value() );
        }

      ++pointsIter;
      }
    }
  }
}

#endif // itkFastMarchingQuadEdgeMeshFilterBase_hxx
