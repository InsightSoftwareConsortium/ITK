/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimplexMeshGeometry.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkSimplexMeshGeometry.h"
#include "itkNumericTraits.h"

#include <vxl_version.h>
#if VXL_VERSION_DATE_FULL > 20040406
# include <vnl/vnl_cross.h>
# define cross_3d vnl_cross_3d
#endif

namespace itk
{
  

SimplexMeshGeometry
::SimplexMeshGeometry()
{
  double c = 1.0/3.0;
  PointType p;
  p.Fill(0.0);
  
  pos.Fill(0);
  oldPos.Fill(0);
  referenceMetrics.Fill(c);
  eps.Fill(c);
  normal.Fill(0); 
  externalForce.Fill(0);
  internalForce.Fill(0);
  circleRadius = 0;
  circleCenter.Fill(0);
  sphereRadius = 0;
  distance = 0;
  phi = 0;
  
  neighborIndices.Fill((unsigned long) NumericTraits<unsigned long>::max());
  neighbors.Fill(p);
  meanCurvature = c;
}


SimplexMeshGeometry
::~SimplexMeshGeometry()
{
}

void 
SimplexMeshGeometry
::ComputeGeometry()
{
  VectorType b,c,cXb, tmp;

  //compute the circum circle (center and radius)
  b = this->neighbors[2] - this->neighbors[0];
  c = this->neighbors[1] - this->neighbors[0];
        
  cXb.Set_vnl_vector( cross_3d<double>(c.Get_vnl_vector(),b.Get_vnl_vector()) );
 
  tmp.Set_vnl_vector( b.GetSquaredNorm() * 
                        cross_3d<double>( cXb.Get_vnl_vector(), c.Get_vnl_vector() ) +
                      c.GetSquaredNorm() * 
                        cross_3d<double>( b.Get_vnl_vector() , cXb.Get_vnl_vector() ) );

  double cXbSquaredNorm = 2 * cXb.GetSquaredNorm();
  
  circleRadius = tmp.GetNorm()/(cXbSquaredNorm);
  tmp[0] /= (cXbSquaredNorm);
  tmp[1] /= (cXbSquaredNorm);
  tmp[2] /= (cXbSquaredNorm);
  circleCenter = this->neighbors[0] + tmp;

  // Compute the circum sphere (center and radius) of a point
  VectorType d,dXc,bXd,sphereTmp, denom;

  d = pos - this->neighbors[0];
  dXc.Set_vnl_vector( cross_3d<double>(d.Get_vnl_vector(),c.Get_vnl_vector()) );
  bXd.Set_vnl_vector( cross_3d<double>(b.Get_vnl_vector(),d.Get_vnl_vector()) );

  sphereTmp.Set_vnl_vector( d.GetSquaredNorm()* cXb.Get_vnl_vector() +
                            b.GetSquaredNorm()* dXc.Get_vnl_vector() +
                            c.GetSquaredNorm()* bXd.Get_vnl_vector()
                          );
  
  double val = 2 * (c[0]*(b[1]*d[2]-b[2]*d[1]) - 
                    c[1]*( b[0]*d[2]-b[2]*d[0] ) + 
                    c[2]*( b[0]*d[1]-b[1]*d[0] ));

  // fix for points which lay on their neighbors plane
  // necessary ??
  if (val == 0) 
    {
    val = 1; //  assert (val != 0 );
    }

  sphereRadius = sphereTmp.GetNorm()/val;

  if (sphereRadius < 0) {
    sphereRadius = -1 * sphereRadius;
  }
}



}  // end namespace itk

