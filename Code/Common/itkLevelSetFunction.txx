/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetFunction_txx_
#define __itkLevelSetFunction_txx_

#include "itkLevelSetFunction.h"

namespace itk {

template< class TImageType >
double LevelSetFunction<TImageType>::m_WaveDT = 1.0/(2.0 * ImageDimension);

template < class TImageType >
double LevelSetFunction<TImageType>::m_DT     = 1.0/(2.0 * ImageDimension);

template< class TImageType >
typename LevelSetFunction< TImageType >::TimeStepType
LevelSetFunction<TImageType>
::ComputeGlobalTimeStep(void *GlobalData) const
{
  TimeStepType dt;

  GlobalDataStruct *d = (GlobalDataStruct *)GlobalData;

  d->m_MaxAdvectionChange += d->m_MaxPropagationChange;
  
  if (vnl_math_abs(m_CurvatureWeight) > 0.0)
    {
    if (d->m_MaxAdvectionChange > 0.0)
      {
        dt = vnl_math_min((m_WaveDT/d->m_MaxAdvectionChange),
                        ( m_DT/ vnl_math_abs(m_CurvatureWeight) ));
      }
    else
      {
      dt = m_DT / vnl_math_abs(m_CurvatureWeight);
      }
    }
  else
    {
    if (d->m_MaxAdvectionChange > 0.0)
      {
      dt = m_WaveDT / d->m_MaxAdvectionChange;
      }
    else 
      {
      dt = 0.0;
      }
    }

  return dt;
}
 
template< class TImageType >
void
LevelSetFunction< TImageType>
::Initialize(const RadiusType &r)
{
  this->SetRadius(r);
  
  // Dummy neighborhood.
  NeighborhoodType it;
  it.SetRadius( r );
  
  // Find the center index of the neighborhood.
  m_Center =  it.Size() / 2;

  // Get the stride length for each axis.
  for(unsigned int i = 0; i < ImageDimension; i++)
    {  m_xStride[i] = it.GetStride(i); }
}
  
template< class TImageType >
typename LevelSetFunction< TImageType >::PixelType
LevelSetFunction< TImageType >
::ComputeUpdate(const NeighborhoodType &it, void *gd,
                const FloatOffsetType& offset) const
{
  unsigned int i, j;  
  const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
  const ScalarValueType center_value  = it.GetCenterPixel();

  ScalarValueType dxy, gradMagSqr, laplacian, x_energy, laplacian_term, propagation_term,
    curvature_term, advection_term, propagation_gradient;
  ScalarValueType dx[ImageDimension], dxx[ImageDimension],
    dx_forward[ImageDimension], dx_backward[ImageDimension];
  VectorType advection_field;

  // Global data structure
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  // Calculate the mean curvature
  gradMagSqr = 1.0e-6;
  for( i = 0 ; i < ImageDimension; i++)
    {
    const unsigned int positionA = 
      static_cast<unsigned int>( m_Center + m_xStride[i]);    
    const unsigned int positionB = 
      static_cast<unsigned int>( m_Center - m_xStride[i]);    
    dx[i] = 0.5 * (it.GetPixel( positionA ) - 
                   it.GetPixel( positionB )    );
      
    dxx[i] = it.GetPixel( positionA )
      + it.GetPixel( positionB ) - 2.0 * center_value;
    
    dx_forward[i]  = it.GetPixel( positionA ) - center_value;
    dx_backward[i] = center_value - it.GetPixel( positionB );
    gradMagSqr += dx[i] * dx[i];
    }
  
  curvature_term = ZERO;
  
  for (i = 0; i < ImageDimension; i++)
    {
    for(j = i+1; j < ImageDimension; j++)
      {
      const unsigned int positionA = static_cast<unsigned int>( 
        m_Center - m_xStride[i] - m_xStride[j] );    
      const unsigned int positionB = static_cast<unsigned int>( 
        m_Center - m_xStride[i] + m_xStride[j] );    
      const unsigned int positionC = static_cast<unsigned int>( 
        m_Center + m_xStride[i] - m_xStride[j] );    
      const unsigned int positionD = static_cast<unsigned int>( 
        m_Center + m_xStride[i] + m_xStride[j] );    
      dxy = 0.25 *( it.GetPixel( positionA )
                       - it.GetPixel( positionB )
                       - it.GetPixel( positionC )
                       + it.GetPixel( positionD )  );
         
      curvature_term -= 2.0 * dx[i] * dx[j] * dxy; 
      }
    }

  for (i = 0; i < ImageDimension; i++)
    {      
    for(j = 0; j < ImageDimension; j++)
      {      
      if(j != i)
        {
        curvature_term += dxx[j] * dx[i] * dx[i];
        }
      }
    }
  
  //  curvature_term = ( curvature_term / (gradMagSqr * vcl_sqrt(gradMagSqr)) )
  //    * m_CurvatureWeight * this->CurvatureSpeed(it, offset);

  curvature_term = ( curvature_term / gradMagSqr )
    * m_CurvatureWeight * this->CurvatureSpeed(it, offset);;
  
  // Calculate the advection term.
  //  $\alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi $
  //
  // Here we can use a simple upwinding scheme since we know the
  // sign of each directional component of the advective force.
  //
  if (m_AdvectionWeight != ZERO)
    {
    
    advection_field = this->AdvectionField(it, offset);
    advection_term = ZERO;
    
    for(i = 0; i < ImageDimension; i++)
      {
      
      x_energy = m_AdvectionWeight * advection_field[i];
      
      if (x_energy > ZERO)
        {
        advection_term += advection_field[i] * dx_backward[i];
        }
      else
        {
        advection_term += advection_field[i] * dx_forward[i];
        }
        
      globalData->m_MaxAdvectionChange
        = vnl_math_max(globalData->m_MaxAdvectionChange, vnl_math_abs(x_energy)); 
      }
    advection_term *= m_AdvectionWeight;
    
    }
  else advection_term = ZERO;

  if (m_PropagationWeight != ZERO)
    {
    // Get the propagation speed
    propagation_term = m_PropagationWeight * this->PropagationSpeed(it, offset);
      
    //
    // Construct upwind gradient values for use in the propagation speed term:
    //  $\beta G(\mathbf{x})\mid\nabla\phi\mid$
    //
    // The following scheme for ``upwinding'' in the normal direction is taken
    // from Sethian, Ch. 6 as referenced above.
    //
    propagation_gradient = ZERO;
    
    if ( propagation_term > ZERO )
      {
      for(i = 0; i< ImageDimension; i++)
        {
        propagation_gradient += vnl_math_sqr( vnl_math_max(dx_backward[i], ZERO) )
          + vnl_math_sqr( vnl_math_min(dx_forward[i],  ZERO) );
        }
      }
    else
      {
      for(i = 0; i< ImageDimension; i++)
        {
        propagation_gradient += vnl_math_sqr( vnl_math_min(dx_backward[i], ZERO) )
          + vnl_math_sqr( vnl_math_max(dx_forward[i],  ZERO) );
        }        
      }
      
    // Collect energy change from propagation term.  This will be used in
    // calculating the maximum time step that can be taken for this iteration.
    globalData->m_MaxPropagationChange =
      vnl_math_max(globalData->m_MaxPropagationChange,
                   vnl_math_abs(propagation_term));
    
    propagation_term *= vcl_sqrt( propagation_gradient );
    }
  else propagation_term = ZERO;

  if(m_LaplacianSmoothingWeight != ZERO)
    {
    laplacian = ZERO;
    
    // Compute the laplacian using the existing second derivative values
    for(i = 0;i < ImageDimension; i++)
      {
      laplacian += dxx[i];
      }

    // Scale the laplacian by its speed and weight
    laplacian_term = 
      laplacian * m_LaplacianSmoothingWeight 
      * LaplacianSmoothingSpeed(it,offset);
    }
  else 
    laplacian_term = ZERO;

  // Return the combination of all the terms.
  return ( PixelType ) ( curvature_term - propagation_term 
                         - advection_term - laplacian_term );
} 

// Print self
template<class TImageType>
void
LevelSetFunction< TImageType>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent );
  os << indent << "WaveDT: " << m_WaveDT << std::endl;
  os << indent << "DT: " << m_DT << std::endl;
}
  
} // end namespace itk

#endif
