/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkDeformableSimplexMesh3DFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformableSimplexMesh3DFilter_txx
#define __itkDeformableSimplexMesh3DFilter_txx

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkNumericTraits.h"

#include <set>

#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL > 20040406
# include <vnl/vnl_cross.h>
# define itk_cross_3d vnl_cross_3d
#else
# define itk_cross_3d cross_3d
#endif

namespace itk
{

/* Constructor. */
template <typename TInputMesh, typename TOutputMesh>
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh> 
::DeformableSimplexMesh3DFilter()
{
  m_Step = 0;
  m_Iterations = 20;
  m_Alpha = 0.2;
  m_Beta  = 0.01;
  m_Gamma = 0.05;
  m_Damping = 0.65;
  m_Rigidity = 1;
  m_Locality = 0;
  m_Range = 1;
  m_MaxMagnitude = 1.0;

  this->ProcessObject::SetNumberOfRequiredInputs(1);

  OutputMeshPointer output = OutputMeshType::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

  m_Data = NULL;
}

template <typename TInputMesh, typename TOutputMesh>
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::~DeformableSimplexMesh3DFilter()
{
}


/* PrintSelf. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "alpha = " << m_Alpha << std::endl;
  os << indent << "beta = " << m_Beta << std::endl;
  os << indent << "gamma = " << m_Gamma << std::endl;
  os << indent << "rigidity = " << m_Rigidity << std::endl;
  os << indent << "No. of Iterations = " << m_Iterations << std::endl;

}/* End PrintSelf. */


/* Generate Data */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::GenerateData()
{
  this->Initialize();

  m_Step = 0;

  while ( m_Step < m_Iterations )
    {
    const float progress = static_cast<float>( m_Step ) / 
                           static_cast<float>( m_Iterations );

    this->UpdateProgress( progress );

    this->ComputeGeometry();

    if ( m_Step % 10 == 0 && m_Step > 0 )
      {
      this->UpdateReferenceMetrics();
      }

    this->ComputeDisplacement();
    m_Step++;
    }

  this->ComputeOutput();
}


/* Set default value of parameters and initialize local data container 
 *  such as forces, displacements and displacement derivatives. */
template <typename TInputMesh, typename TOutputMesh>
void 
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::Initialize()
{
  InputPointsContainerPointer      myPoints = this->GetInput(0)->GetPoints();
  InputPointsContainerIterator     points = myPoints->Begin();

  if ( m_Gradient.IsNotNull() ) 
    {
    GradientImageSizeType imageSize = m_Gradient->GetBufferedRegion().GetSize();
    m_ImageWidth  = imageSize[0];
    m_ImageHeight = imageSize[1];
    m_ImageDepth  = imageSize[2];
    }
  else
    {
    m_ImageWidth  = 0;
    m_ImageHeight = 0;
    m_ImageDepth  = 0;
    }

  if (m_Data.IsNull() )
    {
    m_Data = this->GetInput(0)->GetGeometryData();
    }

  while( points != myPoints->End() ) 
    {
    SimplexMeshGeometry * data;
    unsigned long idx = points.Index();

    data = m_Data->GetElement(idx);
    data->pos = points.Value();

    //        InputMeshType::ArrayType neighbors = this->GetInput(0)->GetNeighbors( points.Index() );

    data->neighbors[0] = myPoints->GetElement(data->neighborIndices[0]);
    data->neighbors[1] = myPoints->GetElement(data->neighborIndices[1]);
    data->neighbors[2] = myPoints->GetElement(data->neighborIndices[2]);

    // store neighborset with a specific radius
    InputNeighbors* neighborsList = this->GetInput(0)->GetNeighbors( points.Index() , m_Rigidity);
    InputNeighborsIterator neighborIt = neighborsList->begin();

    NeighborSetType * neighborSet = new NeighborSetType();
    while ( neighborIt != neighborsList->end() )
      {
      neighborSet->insert( *neighborIt++ );
      }
    data->neighborSet =  neighborSet;

    points++;
    }

  OutputMeshPointer outputMesh = this->GetOutput();
}

/* Compute normals. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeGeometry() 
{
  PointType Foot;
  CovariantVectorType normal;
  CovariantVectorType z;
  VectorType tmp;
  //   unsigned long idx = 0;

  InputMeshPointer inputMesh = this->GetInput(0);

  typename GeometryMapType::Iterator  dataIt = m_Data->Begin();

  SimplexMeshGeometry* data;

  while ( dataIt != m_Data->End() )
    {
    //      idx = dataIt.Index();
    data = dataIt.Value();
    
    data->neighbors[0] = inputMesh->GetPoints()->GetElement(data->neighborIndices[0]);
    data->neighbors[1] = inputMesh->GetPoints()->GetElement(data->neighborIndices[1]);
    data->neighbors[2] = inputMesh->GetPoints()->GetElement(data->neighborIndices[2]);
    
    // compute normal
    normal.Fill(0.0);

    z.SetVnlVector( itk_cross_3d( (data->neighbors[1] - data->neighbors[0]).GetVnlVector() ,
                                    (data->neighbors[2] - data->neighbors[0]).GetVnlVector()) );
    z.Normalize();
    normal += z;

    // copy normal
    data->normal = normal;

    // compute the simplex angle
    data->ComputeGeometry();

    tmp = data->neighbors[0] - data->pos;

    double D = 1.0/(2*data->sphereRadius); /* */

    double tmpNormalProd = dot_product(tmp.GetVnlVector(),data->normal.GetVnlVector());

    double sinphi =  2 * data->circleRadius * D * vnl_math_sgn( tmpNormalProd );
    double phi = asin(sinphi);

    data->phi = phi;
    data->meanCurvature = vcl_abs(sinphi/data->circleRadius);
    tmp = data->pos - data->neighbors[0];

    //compute the foot of p projection of p onto the triangle spanned by its neighbors
    double distance = -tmpNormalProd;
    tmp.SetVnlVector((data->pos).GetVnlVector() - distance * normal.GetVnlVector() );
    Foot.Fill(0.0);
    Foot += tmp;

    data->distance = ((data->circleCenter)-Foot).GetNorm();

    data->eps = ComputeBarycentricCoordinates( Foot, data);
    dataIt.Value() = data;
    dataIt++;
    }
}

template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeDisplacement()
{
  InputMeshPointer inputMesh = this->GetInput(0);
  typename GeometryMapType::Iterator dataIt = m_Data->Begin();
  SimplexMeshGeometry * data;
  VectorType displacement;
 
  // iterator going through each vertex (point) in Mesh
  
  while( dataIt != m_Data->End() ) 
    {
     
    data = dataIt.Value();
    
    this->ComputeInternalForce( data );
    this->ComputeExternalForce( data );      

    displacement.SetVnlVector( m_Locality    * (m_Alpha * (data->internalForce).GetVnlVector()  +
                    m_Beta  * (data->externalForce).GetVnlVector()) + 
             ((1-m_Locality) *  m_Beta  * (data->externalForce).GetVnlVector()) );

    
    //std::cout<< " DISplacement is " << displacement << std::endl; 
    data->pos += displacement;
    inputMesh->GetPoints()->InsertElement( dataIt.Index(), data->pos );

    dataIt++;
    }
}

/* */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeInternalForce(SimplexMeshGeometry *data)
{
  InputMeshPointer inputMesh = this->GetInput(0);
  VectorType tangentForce, normalForce;
  double eps1Diff, eps2Diff, eps3Diff;
  //    double diffAbsSum;
  double d, phi, r; 
  NeighborSetType * neighborSet;
  PointType xOrig;
  PointType eps, epsRef;
  double phiRef;
  PointType f_int;

  xOrig = data->pos;
  eps = data->eps;
  epsRef = data->referenceMetrics;

  eps1Diff = epsRef[0]-eps[0];
  eps2Diff = epsRef[1]-eps[1];
  eps3Diff = epsRef[2]-eps[2];
  //    diffAbsSum = vcl_abs(eps1Diff)+vcl_abs(eps2Diff)+vcl_abs(eps3Diff);

  tangentForce.SetVnlVector( eps1Diff * (data->neighbors[0]).GetVnlVector() +
                               eps2Diff * (data->neighbors[1]).GetVnlVector() +
                               eps3Diff * (data->neighbors[2]).GetVnlVector()
    );

  r = data->circleRadius;
  d = data->distance;
  phi = data->phi;

  neighborSet = data->neighborSet;

  NeighborSetIterator neighborIt = neighborSet->begin();
  phiRef = 0.0;

  while ( neighborIt != neighborSet->end() )
    {
    phiRef += m_Data->GetElement(*neighborIt++)->phi;
    }
  phiRef /= (double) neighborSet->size();

  double L = L_Func(r,d,phi);
  double L_Ref = L_Func(r,d,phiRef);

  normalForce.SetVnlVector(-1.0 * ( L_Ref - L ) * (data->normal).GetVnlVector());

  data->internalForce.Fill(0.0);

  // quick hack fixing for div by zero error
  if (L_Ref != (double) NumericTraits<unsigned long>::max() && L != (double) NumericTraits<unsigned long>::max())
    {
    data->internalForce += tangentForce + normalForce;
    }


}

/** Compute model Displacement according to image gradient forces */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeExternalForce( SimplexMeshGeometry * data)
{
  PointType vec_for;
  GradientIndexType coord, coord2;
  
  // probe the data i.e mesh vertex data->pos find the corresponding 
  // value for the gradient image 
  coord[0] = static_cast<GradientIndexValueType>(data->pos[0]);
  coord[1] = static_cast<GradientIndexValueType>(data->pos[1]);
  coord[2] = static_cast<GradientIndexValueType>(data->pos[2]);

  coord2[0] = static_cast<GradientIndexValueType>( ceil (m_Range * data->normal[0]) );
  coord2[1] = static_cast<GradientIndexValueType>( ceil (m_Range * data->normal[1]) );
  coord2[2] = static_cast<GradientIndexValueType>( ceil (m_Range * data->normal[2]) );

  // Translate the index of normal ( coord2) to the mesh vertex (coord) 
  coord2[0] += coord[0];
  coord2[1] += coord[1];
  coord2[2] += coord[2];

  if ( (coord[0] == coord2[0] && coord[1] == coord2[1] && coord[2] == coord2[2]) ||
      ( (coord[0] < 0) && (coord[1] < 0) && (coord[2] < 0) && 
  (coord2[0] > m_ImageWidth) && (coord2[1] > m_ImageHeight) && (coord2[2] > m_ImageDepth) ) )
    {
      vec_for.Fill(0);
    }
  else
    {
      
      GradientIndexType index;
  
      index  = this->BresenhamLine(coord, coord2);

      vec_for[0] = m_Gradient->GetPixel(index)[0];
      vec_for[1] = m_Gradient->GetPixel(index)[1];
      vec_for[2] = m_Gradient->GetPixel(index)[2];
  
      // check to make sure we are consistent with the direction of gradient from lighter-->darker
      /*  double mag = dot_product(data->normal.GetVnlVector(),vec_for.GetVnlVector());
      
       if (mag > 0)
       {*/
     
          vec_for[0] -= m_Gradient->GetPixel(coord)[0];
    vec_for[1] -= m_Gradient->GetPixel(coord)[1];
    vec_for[2] -= m_Gradient->GetPixel(coord)[2];
    
  
    double mag = dot_product(data->normal.GetVnlVector(),vec_for.GetVnlVector());
      
    vec_for[0] += mag*(data->normal)[0];
    vec_for[1] += mag*(data->normal)[1];
    vec_for[2] += mag*(data->normal)[2];
      
    mag = vec_for.GetVectorFromOrigin().GetNorm();

    if (mag > 0.5) 
      {
        for (int i=0; i<3; i++) 
    {
      vec_for[i] = (0.5 * vec_for[i])/mag;
    }
      }
    /*
   }
       else
   {
     vec_for.Fill(0);
     }*/
    }
  data->externalForce[0] = vec_for[0];
  data->externalForce[1] = vec_for[1];
  data->externalForce[2] = vec_for[2];
  
}



/* Copy the content of m_Location into the Output. */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeOutput() 
{
  OutputMeshType * output = this->GetOutput();
  output->SetPoints(this->GetInput(0)->GetPoints());
  output->SetPointData(this->GetInput(0)->GetPointData());
  output->SetCells(this->GetInput(0)->GetCells());
  output->SetGeometryData(m_Data);
  output->SetLastCellId( this->GetInput(0)->GetLastCellId() );
}


/*  */
template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::UpdateReferenceMetrics()
{
  InputMeshPointer inputMesh = this->GetInput(0);
  InputPointsContainerPointer   points = inputMesh->GetPoints();

  double H;
  double H_N1;
  double H_N2;
  double H_N3;
  double H_Mean;

  GeometryMapIterator dataIt = m_Data->Begin();

  SimplexMeshGeometry * data;

  while ( dataIt != m_Data->End() )
    {
    data = dataIt->Value();
    H_N1 =((SimplexMeshGeometry*)(m_Data->GetElement(data->neighborIndices[0])))->meanCurvature;
    H_N2 =((SimplexMeshGeometry*)(m_Data->GetElement(data->neighborIndices[1])))->meanCurvature;
    H_N3 =((SimplexMeshGeometry*)(m_Data->GetElement(data->neighborIndices[2])))->meanCurvature;
    H = data->meanCurvature;

    H_Mean = (H_N1 + H_N2 + H_N3)/3.0;

    PointType deltaH;

    deltaH[0] = (H_N1 - H_Mean)/H_Mean;
    deltaH[1] = (H_N2 - H_Mean)/H_Mean;
    deltaH[2] = (H_N3 - H_Mean)/H_Mean;

    //deltaH[0] = (H_N1 - H_Mean)/H;
    //deltaH[1] = (H_N2 - H_Mean)/H;
    //deltaH[2] = (H_N3 - H_Mean)/H;

    PointType eps, eps_opt;
    // compute optimal reference metrics
    eps_opt[0] = (1.0/3.0) + m_Gamma * deltaH[0];
    eps_opt[1] = (1.0/3.0) + m_Gamma * deltaH[1];
    eps_opt[2] = (1.0/3.0) + m_Gamma * deltaH[2];

    eps = data->referenceMetrics;

    eps[0] = eps[0] + 0.5 * (eps_opt[0] - eps[0]) ;
    eps[1] = eps[1] + 0.5 * (eps_opt[1] - eps[1]) ;
    eps[2] = eps[2] + 0.5 * (eps_opt[2] - eps[2]) ;

    // set current reference metrics
    data->referenceMetrics = eps;
    inputMesh->SetPointData( dataIt->Index() , H );
    dataIt.Value() = data;
    //      m_Data->InsertElement(dataIt->Index(),data);
    dataIt++;
    }
}


template <typename TInputMesh, typename TOutputMesh>
double DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::L_Func(double r,double d, double phi)
{
  double r2 = r*r;
  double d2 = d*d;
  double r2Minusd2 = r2-d2;
  double tanPhi = tan(phi);

  double eps = 1.0;
  if (phi*vnl_math_sgn(phi) > vnl_math::pi_over_2)
    {
    eps = -1.0;
    }
  double L;
  double tmpSqr = r2 + r2Minusd2 * tanPhi*tanPhi;
  if (tmpSqr > 0)
    {
    double denom = eps*(sqrt(tmpSqr) + r);
    if ( denom != 0 ) 
      {
      L = (r2Minusd2 * tanPhi) / denom;
      }
    else
      {
      L = (double) NumericTraits<unsigned long>::max();
      //          L = 0.0;
      }
    }
  else 
    {
    L = (double) NumericTraits<unsigned long>::max();      
    }
  return L;
}


template <typename TInputMesh, typename TOutputMesh>
typename DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>::PointType
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::ComputeBarycentricCoordinates( PointType p, SimplexMeshGeometry* data)
{
  PointType a,b,c;
  a = data->neighbors[0];
  b = data->neighbors[1];
  c = data->neighbors[2];

  VectorType n,na,nb,nc;
  n.SetVnlVector( itk_cross_3d((b-a).GetVnlVector(), (c-a).GetVnlVector()) );
  na.SetVnlVector( itk_cross_3d((c-b).GetVnlVector(), (p-b).GetVnlVector()) );
  nb.SetVnlVector( itk_cross_3d((a-c).GetVnlVector(), (p-c).GetVnlVector()) );
  nc.SetVnlVector( itk_cross_3d((b-a).GetVnlVector(), (p-a).GetVnlVector()) );

  PointType eps;
  eps[0] = dot_product(n.GetVnlVector(),na.GetVnlVector()) / n.GetSquaredNorm();
  eps[1] = dot_product(n.GetVnlVector(),nb.GetVnlVector()) / n.GetSquaredNorm();
  eps[2] = dot_product(n.GetVnlVector(),nc.GetVnlVector()) / n.GetSquaredNorm();

  return eps;
}


template <typename TInputMesh, typename TOutputMesh>
typename DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>::GradientIndexType
DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
::BresenhamLine(GradientIndexType a,GradientIndexType b)
{
  int xchange = 1;
  int ychange = 1;
  int zchange = 1;
  int i = 0;
  int e1 = 0;
  int e2 = 0;
  int length;
  double magnitude = 0;
  double mag;
  GradientIndexType c;
  int dx = b[0] - a[0];
  int dy = b[1] - a[1];
  int dz = b[2] - a[2];

  if( dx < 0 ) 
    {
    xchange = -1;
    dx = - dx;
    }
  if( dy < 0 ) 
    {
    ychange = -1;
    dy = -dy;
    }
  if( dz < 0 ) 
    {
    zchange = -1;
    dz = -dz;
    }

  if( dz >= dy && dz >= dx ) 
    {
    length = dz;
    while( i < length) 
      {
      a[2] += zchange;
      e1 += dx;
      e2 += dy;
      if( e1 > dz ) 
        {
        a[0] += xchange;
        e1 -= dz;
        }
      if( e2 > dz ) 
        {
        a[1] += ychange;
        e2 -= dz;
        }
      mag = sqrt(m_Gradient->GetPixel(a)[0] * m_Gradient->GetPixel(a)[0] +
         m_Gradient->GetPixel(a)[1] * m_Gradient->GetPixel(a)[1] +
         m_Gradient->GetPixel(a)[2] * m_Gradient->GetPixel(a)[2]);
      if (mag > magnitude)
        {
        magnitude = mag;
        c[0]=a[0];
        c[1]=a[1];
        c[2]=a[2];
        }
       ++i;
       }  
     return c;
    }
  else if( dy >= dz && dy >= dx ) 
    {
     length = dy;
     while( i < length) 
       {
       a[1] += ychange;
       e1 += dz;
       e2 += dx;
       if( e1 > dy ) 
         {
         a[2] += zchange;
         e1 -= dy;
         }
       if( e2 > dy ) 
         {
         a[0] += xchange;
         e2 -= dy;
         }
       mag = sqrt(m_Gradient->GetPixel(a)[0] * m_Gradient->GetPixel(a)[0] +
         m_Gradient->GetPixel(a)[1] * m_Gradient->GetPixel(a)[1] +
         m_Gradient->GetPixel(a)[2] * m_Gradient->GetPixel(a)[2]);
       if (mag > magnitude)
         {
         magnitude = mag;
         c[0]=a[0];
         c[1]=a[1];
         c[2]=a[2];
         }
       ++i;
       }
     return c;
    }
  else if ( dx >= dz && dx >= dy )
    {  
     length = dx;
     while( i < length) 
       {
       a[0] += xchange;
       e1 += dz;
       e2 += dy;
       if( e1 > dx ) 
         {
         a[2] += zchange;
         e1 -= dx;
         }
       if( e2 > dx ) 
         {
         a[1] += ychange;
         e2 -= dx;
         }
       mag = sqrt(m_Gradient->GetPixel(a)[0] * m_Gradient->GetPixel(a)[0] +
         m_Gradient->GetPixel(a)[1] * m_Gradient->GetPixel(a)[1] +
         m_Gradient->GetPixel(a)[2] * m_Gradient->GetPixel(a)[2]);
       if (mag > magnitude)
         {
         magnitude = mag;
         c[0]=a[0];
         c[1]=a[1];
         c[2]=a[2];
         }
    
       ++i;
       }
     return c;
    }
  else
    {
    // draw one point only        
   
    c[0]=a[0];
    c[1]=a[1];
    c[2]=a[2];
    return c;
    }
  }
} /* end namespace itk. */

#endif //__itkDeformableSimplexMesh3DFilter_TXX
