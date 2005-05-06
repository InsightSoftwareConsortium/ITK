/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableSimplexMesh3DGradientConstraintForceFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __DeformableSimplexMesh3DGradientConstraintForceFilter_txx
#define __DeformableSimplexMesh3DGradientConstraintForceFilter_txx

#include "itkDeformableSimplexMesh3DGradientConstraintForceFilter.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"

#include <set>

namespace itk
{

  /* Constructore  */
template <typename TInputMesh, typename TOutputMesh>
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh> 
::DeformableSimplexMesh3DGradientConstraintForceFilter()
{
  m_Range = 1;
}

template <typename TInputMesh, typename TOutputMesh>
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>
::~DeformableSimplexMesh3DGradientConstraintForceFilter()
{

}


template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent); 
}


template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>
::ComputeExternalForce(SimplexMeshGeometry * data)
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

  // check to make sure both point sit within the gradient image borders
  // and also the points are *different* from each other
  if ( !(coord[0]==coord2[0] && coord[1]==coord2[1] && coord[2]==coord2[2]) &&
       (coord[0] >= 0) && (coord[1] >= 0) && (coord[2] >= 0) && 
       (coord2[0] < this->m_ImageWidth) && 
       (coord2[1] < this->m_ImageHeight) &&
       (coord2[2] < this->m_ImageDepth)  )
    {
      
    GradientIndexType index;
      
    index  = this->BresenhamLine(coord, coord2);
      

    vec_for[0] = this->m_Gradient->GetPixel(index)[0];
    vec_for[1] = this->m_Gradient->GetPixel(index)[1];
    vec_for[2] = this->m_Gradient->GetPixel(index)[2];
  
      
    vec_for[0] -= this->m_Gradient->GetPixel(coord)[0];
    vec_for[1] -= this->m_Gradient->GetPixel(coord)[1];
    vec_for[2] -= this->m_Gradient->GetPixel(coord)[2];
      
  
    double mag = dot_product(data->normal.Get_vnl_vector(),vec_for.Get_vnl_vector());
      
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
    }
  else
    {
    vec_for.Fill(0);
    }

  data->externalForce[0] = vec_for[0];
  data->externalForce[1] = vec_for[1];
  data->externalForce[2] = vec_for[2];
  
}

template <class TInputMesh,class TOutputMesh >
typename DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::GradientIndexType
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>
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
      mag = sqrt(this->m_Gradient->GetPixel(a)[0] * this->m_Gradient->GetPixel(a)[0] +
         this->m_Gradient->GetPixel(a)[1] * this->m_Gradient->GetPixel(a)[1] +
         this->m_Gradient->GetPixel(a)[2] * this->m_Gradient->GetPixel(a)[2]);
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
       mag = sqrt(this->m_Gradient->GetPixel(a)[0] * this->m_Gradient->GetPixel(a)[0] +
         this->m_Gradient->GetPixel(a)[1] * this->m_Gradient->GetPixel(a)[1] +
         this->m_Gradient->GetPixel(a)[2] * this->m_Gradient->GetPixel(a)[2]);
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
       mag = sqrt(this->m_Gradient->GetPixel(a)[0] * this->m_Gradient->GetPixel(a)[0] +
         this->m_Gradient->GetPixel(a)[1] * this->m_Gradient->GetPixel(a)[1] +
         this->m_Gradient->GetPixel(a)[2] * this->m_Gradient->GetPixel(a)[2]);
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

} // namespace itk

#endif //__itkDeformableSimplexMesh3DGradientConstraintForceFilter_TXX

