/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSet2DFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSet2DFunction_txx_
#define __itkLevelSet2DFunction_txx_

#include "itkLevelSet2DFunction.h"

namespace itk {

template< class TImageType >
double LevelSet2DFunction<TImageType>::m_WaveDT = 0.20;

template < class TImageType >
double LevelSet2DFunction<TImageType>::m_DT     = 0.25;

template< class TImageType >
typename LevelSet2DFunction< TImageType >::TimeStepType
LevelSet2DFunction<TImageType>
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
                        (m_DT/m_CurvatureWeight));
      }
    else
      {
      dt = m_DT / m_CurvatureWeight;
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
LevelSet2DFunction< TImageType>
::Initialize(const RadiusType &r)
{
  this->SetRadius(r);
  
  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius( r );
  
  // Slice the neighborhood
  m_Center =  it.Size() / 2;

  // Get the y-axis stride length
  m_YStride = it.GetStride(1);

  // Set up the derivative operators
  dx_op.SetOrder(1);
  dx_op.SetDirection(0);
  dx_op.CreateDirectional();
  
  dxx_op.SetOrder(2);
  dxx_op.SetDirection(0);
  dxx_op.CreateDirectional();

  // SANITY CHECK !
  if (dxx_op.Size() != 3)
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }
  
  //
  // Slices are calculated relative to the center pixel of the
  // neighborhood.  This way, the subclass can define the neighborhood
  // to be of arbitrary size (with the understanding that the size must
  // be at least 3x3 for derivative calculations).
  //
  // Assumes that 1st AND 2nd order derivative kernels have length 3.  This
  // is valid for itk::DerivativeOperator. Otherwise we need another set
  // of slices for 2nd order derivative.
  //
  // slice = std::slice(start_offset, length, step_size);
  //
  x_slice = std::slice(m_Center - 1        , 3, 1);
  y_slice = std::slice(m_Center - m_YStride, 3, m_YStride);
}
  
template< class TImageType >
typename LevelSet2DFunction< TImageType >::PixelType
LevelSet2DFunction< TImageType >
::ComputeUpdate(const NeighborhoodType &it, void *gd,
                const FloatOffsetType& offset) const
{
  /*
    ToDo:
    1. Implement vanishing viscosity for curvature term (replace epsilon)?
    2. Add in max_curvature term to calculation of dt.
  */
  const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
  ScalarValueType temp_value;
  ScalarValueType dx, dy, dxx, dyy, dxy;
  ScalarValueType dx_forward, dx_backward, dy_forward, dy_backward;
  ScalarValueType grad_mag_sq, propagation_gradient;
  ScalarValueType propagation_term, curvature_term, advection_term;
  VectorType advection_field;
  ScalarValueType x_energy, y_energy;

  // Global data structure
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  // Calculate derivatives.
  //  dx  = m_InnerProduct(x_slice, it, dx_op);
  //  dxx = m_InnerProduct(x_slice, it, dxx_op);
  //  dy  = m_InnerProduct(y_slice, it, dx_op);
  //  dyy = m_InnerProduct(y_slice, it, dxx_op);

  dx =0.5* (it.GetPixel(m_Center + 1) - it.GetPixel(m_Center - 1)) ;
  dy = 0.5 *( it.GetPixel(m_Center + m_YStride)
              - it.GetPixel(m_Center - m_YStride) );
  dxx = it.GetPixel(m_Center + 1) + it.GetPixel(m_Center - 1)
    - 2.0 * it.GetPixel(m_Center);
  dyy = it.GetPixel(m_Center + m_YStride)
    + it.GetPixel(m_Center - m_YStride)
    - 2.0 * it.GetPixel(m_Center);
  
  dxy = 0.25* (  it.GetPixel(m_Center + 1 + m_YStride)
               + it.GetPixel(m_Center - 1 - m_YStride)
               - it.GetPixel(m_Center - 1 + m_YStride)
               - it.GetPixel(m_Center + 1 - m_YStride) );

  grad_mag_sq = dx * dx + dy * dy;
    
  //
  // Calculate mean curvature times gradient magnitude.
  //
  // Better to use some sort of vanishing viscosity scheme (jc 7/3/01)
  // instead of the current Epsilon switch.
  //
  // Mean curvature (kappa) is calculated as
  // $\kappa = \nabla \cdot \frac{\nabla\phi}{\mid \nabla\phi \mid}$
  //
  //  if (grad_mag_sq > m_EpsilonMagnitude) 
  //    curvature_term = (dx*dx*dyy - 2.0f*dx*dy*dxy + dy*dy*dxx) / grad_mag_sq;
  //  else curvature_term = dxx + dyy; // This is a carryover from vispack and
                                   // not necessary if vanishing viscosity
  // is implemented.
  curvature_term = (dx*dx*dyy - 2.0f*dx*dy*dxy + dy*dy*dxx +
                      m_EpsilonMagnitude * (dxx+dyy) ) / (m_EpsilonMagnitude + grad_mag_sq);
  
  curvature_term *= m_CurvatureWeight * this->CurvatureSpeed(it, offset);

  //
  // Calculate upwind derivatives.  These are used in calculating upwind
  // gradient values and in computing the inner product with the advection
  // field term:
  // $\alpha\stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi$
  //
  temp_value  = it.GetPixel(m_Center);
  dx_forward  = it.GetPixel(m_Center + 1) - temp_value;
  dx_backward = temp_value - it.GetPixel(m_Center - 1);
  dy_forward  = it.GetPixel(m_Center + m_YStride) - temp_value;
  dy_backward = temp_value - it.GetPixel(m_Center - m_YStride);

  // 
  // Calculate the advection term.
  //  $\alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi $
  //
  // Here we can use a simple upwinding scheme since we know the
  // sign of each directional component of the advective force.
  //
  if (m_AdvectionWeight != ZERO)
    {
  
      advection_field = this->AdvectionField(it, offset);
      
      x_energy = m_AdvectionWeight * advection_field[0];
      y_energy = m_AdvectionWeight * advection_field[1];
      
      if (x_energy > ZERO) advection_term = advection_field[0] * dx_backward;
      else                 advection_term = advection_field[0] * dx_forward;
      
      if (y_energy > ZERO) advection_term += advection_field[1] * dy_backward;
      else                 advection_term += advection_field[1] * dy_forward;
      
      advection_term *= m_AdvectionWeight;

      // Collect energy change from the advection term.  This will be used
      // in calculating the maximum time step that can be taken this iteration.
      globalData->m_MaxAdvectionChange
        = vnl_math_max(globalData->m_MaxAdvectionChange, vnl_math_abs(x_energy) +
                       vnl_math_abs(y_energy)); 
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
      if ( propagation_term > ZERO )
        {
          propagation_gradient = vnl_math_sqr( vnl_math_max(dx_backward, ZERO) )
            + vnl_math_sqr( vnl_math_min(dx_forward,  ZERO) )
            + vnl_math_sqr( vnl_math_max(dy_backward, ZERO) )
            + vnl_math_sqr( vnl_math_min(dy_forward,  ZERO) );
        }
      else
        {
          propagation_gradient = vnl_math_sqr( vnl_math_min(dx_backward, ZERO) )
            + vnl_math_sqr( vnl_math_max(dx_forward,  ZERO) )
            + vnl_math_sqr( vnl_math_min(dy_backward, ZERO) )
            + vnl_math_sqr( vnl_math_max(dy_forward,  ZERO) );
        }
      
      // Collect energy change from propagation term.  This will be used in
      // calculating the maximum time step that can be taken for this iteration.
      globalData->m_MaxPropagationChange =
        vnl_math_max(globalData->m_MaxPropagationChange,
                                         vnl_math_abs(propagation_term));
      
      
      propagation_term *= vnl_math_sqrt( propagation_gradient );
    }
  else propagation_term = ZERO;
  
  //
  // Return the combination of all the terms.
  //
  return ( PixelType ) ( curvature_term - propagation_term - advection_term );
} 


template< class TImageType >
typename LevelSet2DFunction< TImageType >::PixelType
LevelSet2DFunction< TImageType >
::ComputeUpdate(const BoundaryNeighborhoodType &it, void *gd,
                const FloatOffsetType& offset) const
{
  /*
    ToDo:
    1. Implement vanishing viscosity for curvature term (replace epsilon)?
  */
  const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
  ScalarValueType temp_value;
  ScalarValueType dx, dy, dxx, dyy, dxy;
  ScalarValueType dx_forward, dx_backward, dy_forward, dy_backward;
  ScalarValueType grad_mag_sq, propagation_gradient;
  ScalarValueType propagation_term, curvature_term, advection_term;
  VectorType advection_field;
  ScalarValueType x_energy, y_energy;

  // Global data structure
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  // Calculate derivatives.
  //  dx  = m_SmartInnerProduct(x_slice, it, dx_op);
  //  dxx = m_SmartInnerProduct(x_slice, it, dxx_op);
  //  dy  = m_SmartInnerProduct(y_slice, it, dx_op);
  //  dyy = m_SmartInnerProduct(y_slice, it, dxx_op);

  dx = 0.5* (it.GetPixel(m_Center + 1) - it.GetPixel(m_Center - 1)) ;
  dy = 0.5 *(  it.GetPixel(m_Center + m_YStride)
               - it.GetPixel(m_Center - m_YStride) );
  dxx = it.GetPixel(m_Center + 1) + it.GetPixel(m_Center - 1)
    - 2.0 * it.GetPixel(m_Center);
  dyy = it.GetPixel(m_Center + m_YStride)
    + it.GetPixel(m_Center - m_YStride) - 2.0 * it.GetPixel(m_Center);
  
  dxy = 0.25* (it.GetPixel(m_Center + 1 + m_YStride)
               + it.GetPixel(m_Center - 1 - m_YStride)
               - it.GetPixel(m_Center - 1 + m_YStride)
               - it.GetPixel(m_Center + 1 - m_YStride) );
  
  grad_mag_sq = dx * dx + dy * dy;

  //
  // Calculate mean curvature times gradient magnitude.
  //
  // Better to use some sort of vanishing viscosity scheme (jc 7/3/01)
  // instead of the current Epsilon switch.
  //
  // Mean curvature (kappa) is calculated as
  // $\kappa = \nabla \cdot \frac{\nabla\phi}{\mid \nabla\phi \mid}$
  //
  //if (grad_mag_sq > m_EpsilonMagnitude) 
  //    curvature_term = (dx*dx*dyy - 2.0f*dx*dy*dxy + dy*dy*dxx) / grad_mag_sq;
  //  else curvature_term = dxx + dyy; // This is a carryover from vispack and
                                   // not necessary if vanishing viscosity
                                   // is implemented.
  //  curvature_term *= m_CurvatureWeight * this->CurvatureSpeed(it, offset);


  curvature_term = (dx*dx*dyy - 2.0f*dx*dy*dxy + dy*dy*dxx +
                      m_EpsilonMagnitude * (dxx+dyy) ) / (m_EpsilonMagnitude + grad_mag_sq);
  
  curvature_term *= m_CurvatureWeight * this->CurvatureSpeed(it, offset);

  
  //
  // Calculate upwind derivatives.  These are using in calculating upwind
  // gradient values and in computing the inner product with the advection
  // field term:
  // $\alpha\stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi$
  //
  temp_value  = it.GetPixel(m_Center);
  dx_forward  = it.GetPixel(m_Center + 1) - temp_value;
  dx_backward = temp_value - it.GetPixel(m_Center - 1);
  dy_forward  = it.GetPixel(m_Center + m_YStride) - temp_value;
  dy_backward = temp_value - it.GetPixel(m_Center - m_YStride);

  // 
  // Calculate the advection term.
  //  $\alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi $
  //
  // Here we can use a simple upwinding scheme since we know the
  // sign of each directional component of the advective force.
  //
  if (m_AdvectionWeight != ZERO)
    {
  
      advection_field = this->AdvectionField(it, offset);
      
      x_energy = m_AdvectionWeight * advection_field[0];
      y_energy = m_AdvectionWeight * advection_field[1];
      
      if (x_energy > ZERO) advection_term = advection_field[0] * dx_backward;
      else              advection_term = advection_field[0] * dx_forward;
      
      if (y_energy > ZERO) advection_term += advection_field[1] * dy_backward;
      else              advection_term += advection_field[1] * dy_forward;
      
      advection_term *= m_AdvectionWeight;

      // Collect energy change from the advection term.  This will be used
      // in calculating the maximum time step that can be taken this iteration.
      globalData->m_MaxAdvectionChange
        = vnl_math_max(globalData->m_MaxAdvectionChange, vnl_math_abs(x_energy) +
                       vnl_math_abs(y_energy)); 
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
      if ( propagation_term > ZERO )
        {
          propagation_gradient = vnl_math_sqr( vnl_math_max(dx_backward, ZERO) )
            + vnl_math_sqr( vnl_math_min(dx_forward,  ZERO) )
            + vnl_math_sqr( vnl_math_max(dy_backward, ZERO) )
            + vnl_math_sqr( vnl_math_min(dy_forward,  ZERO) );
        }
      else
        {
          propagation_gradient = vnl_math_sqr( vnl_math_min(dx_backward, ZERO) )
            + vnl_math_sqr( vnl_math_max(dx_forward,  ZERO) )
            + vnl_math_sqr( vnl_math_min(dy_backward, ZERO) )
            + vnl_math_sqr( vnl_math_max(dy_forward,  ZERO) );
        }
      
      // Collect energy change from propagation term.  This will be used in
      // calculating the maximum time step that can be taken for this iteration.
      globalData->m_MaxPropagationChange =
        vnl_math_max(globalData->m_MaxPropagationChange,
                                         vnl_math_abs(propagation_term));
      
      
      propagation_term *= vnl_math_sqrt( propagation_gradient );
    }
  else propagation_term = ZERO;
  
  //
  // Return the combination of all the terms.
  //
  return ( PixelType ) ( curvature_term - propagation_term - advection_term );
} 

} // end namespace itk

#endif
