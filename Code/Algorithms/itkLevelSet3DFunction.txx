/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkLevelSet3DFunction_txx_
#define __itkLevelSet3DFunction_txx_

#include "itkLevelSet3DFunction.h"

namespace itk {

template< class TImageType >
double LevelSet3DFunction<TImageType>::m_WaveDT = 1.0/(2.0 * ImageDimension);

template < class TImageType >
double LevelSet3DFunction<TImageType>::m_DT     = 1.0/(2.0 * ImageDimension) ;

template< class TImageType >
typename LevelSet3DFunction< TImageType >::TimeStepType
LevelSet3DFunction<TImageType>
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
LevelSet3DFunction< TImageType>
::Initialize(const RadiusType &r)
{
  this->SetRadius(r);
  
  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius( r );
  
  // Slice the neighborhood
  m_Center =  it.Size() / 2;

  // Get the y-axis stride length
  for(int i = 0; i < ImageDimension; i++)
    m_xStride[i] = it.GetStride(i);


  //The following stuff not necessary

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
  for( int i = 0; i< ImageDimension; i++)
    x_slice[i] = std::slice(m_Center - m_xStride[i], 3, m_xStride[i]);

}
  
template< class TImageType >
typename LevelSet3DFunction< TImageType >::PixelType
LevelSet3DFunction< TImageType >
::ComputeUpdate(const NeighborhoodType &it, void *gd,
                const FloatOffsetType& offset) const
{
  /*
    ToDo:
    1. Implement vanishing viscosity for curvature term (replace epsilon)?
    2. Add in max_curvature term to calculation of dt.
  */

  
  
  int i;
  
  const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
  ScalarValueType temp_value;
  ScalarValueType dx[ImageDimension];

  ScalarValueType dx_forward[ImageDimension], 
                  dx_backward[ImageDimension];

  ScalarValueType dxP[ImageDimension][ImageDimension], 
                  dxM[ImageDimension][ImageDimension];

  ScalarValueType grad_mag_sq, propagation_gradient;
  ScalarValueType propagation_term, curvature_term, advection_term;
  VectorType advection_field;
  ScalarValueType x_energy[ImageDimension];

  ScalarValueType MIN_NORM = 1.0e-6;

  // Global data structure
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  temp_value  = it.GetCenterPixel();  

  //calculate mean curvature
  //First calculate derivatives
  for( i = 0 ; i < ImageDimension; i++)
    {
      dx[i] = 0.5 * (it.GetPixel(m_Center + m_xStride[i]) - 
                     it.GetPixel(m_Center - m_xStride[i]));
      
      dx_forward[i] = it.GetPixel(m_Center + m_xStride[i]) - 
        temp_value;
    
      dx_backward[i] =  temp_value - 
        it.GetPixel(m_Center - m_xStride[i]);  
      
      for( int j = 0; j < ImageDimension; j++)
        {
          //calculate the partial derivatives  
          if(j != i)
            {
              dxP[i][j] = 0.5 * (it.GetPixel(m_Center + m_xStride[i] +
                                             m_xStride[j]) - 
                                 it.GetPixel(m_Center + m_xStride[i] - 
                                             m_xStride[j]) );
          
              dxM[i][j] = 0.5 * (it.GetPixel(m_Center - m_xStride[i] + 
                                             m_xStride[j]) - 
                                 it.GetPixel(m_Center - m_xStride[i] -
                                             m_xStride[j]) );
            }
          else
            {
              dxP[i][j] = 0;
              dxM[i][j] = 0;
            }
          
        }//for

    }//for
  
  float F[ImageDimension],B[ImageDimension];
  float normF, normB; 
  curvature_term = 0.0;
  
  //calculate the vectors used to calculate the mean curvature
  for(int k = 0; k < ImageDimension; k ++)
    {

      for( i = 0; i < ImageDimension; i ++)
        {
          F[i] = 0.5 *(dxP[k][i] + dx[i]);
          B[i] = 0.5 *(dxM[k][i] + dx[i]);

        }
      F[k] = dx_forward[k];
      B[k] = dx_backward[k];

      normF = MIN_NORM;
      normB = MIN_NORM;
      for(i = 0; i < ImageDimension; i++)
        {
          normF += F[i] * F[i];
          normB += B[i] * B[i];
        }

      curvature_term += F[k]/vnl_math_sqrt(normF) - B[k]/vnl_math_sqrt(normB);
      
    }
      
  curvature_term *= m_CurvatureWeight * this->CurvatureSpeed(it, offset);


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

          x_energy[i] = m_AdvectionWeight * advection_field[i];
          
          if (x_energy[i] > ZERO) advection_term += advection_field[i] * dx_backward[i];
          else                 advection_term += advection_field[i] * dx_forward[i];

        }

      advection_term *= m_AdvectionWeight;

          
      // Collect energy change from the advection term.  This will be used
      // in calculating the maximum time step that can be taken this iteration.

      PixelType totalEnergy = ZERO;
      
      for(int i = 0; i < ImageDimension; i++)
        totalEnergy += vnl_math_abs(x_energy[i]);
      
      globalData->m_MaxAdvectionChange
        = vnl_math_max(globalData->m_MaxAdvectionChange, totalEnergy); 
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
            propagation_gradient += vnl_math_sqr( vnl_math_max(dx_backward[i], ZERO) )
              + vnl_math_sqr( vnl_math_min(dx_forward[i],  ZERO) );

        }
      else
        {
          for(i = 0; i< ImageDimension; i++)
            propagation_gradient += vnl_math_sqr( vnl_math_min(dx_backward[i], ZERO) )
              + vnl_math_sqr( vnl_math_max(dx_forward[i],  ZERO) );

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
typename LevelSet3DFunction< TImageType >::PixelType
LevelSet3DFunction< TImageType >
::ComputeUpdate(const BoundaryNeighborhoodType &it, void *gd,
                const FloatOffsetType& offset) const
{
 
  /*
    ToDo:
    1. Implement vanishing viscosity for curvature term (replace epsilon)?
    2. Add in max_curvature term to calculation of dt.
  */

  int i;
  
  const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
  ScalarValueType temp_value;
  ScalarValueType dx[ImageDimension];

  ScalarValueType dxx[ImageDimension];
  ScalarValueType dxy[ImageDimension * (ImageDimension-1)/2];
  ScalarValueType curve, gradMag;
  
  
  ScalarValueType dx_forward[ImageDimension], 
                  dx_backward[ImageDimension];

  ScalarValueType dxP[ImageDimension][ImageDimension], 
                  dxM[ImageDimension][ImageDimension];

  ScalarValueType grad_mag_sq, propagation_gradient;
  ScalarValueType propagation_term, curvature_term, advection_term;
  VectorType advection_field;
  ScalarValueType x_energy[ImageDimension];

  ScalarValueType MIN_NORM = 1.0e-6;

  // Global data structure
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  temp_value  = it.GetPixel(m_Center);  


  //calculate mean curvature
  //First calculate derivatives
  for( i = 0 ; i < ImageDimension; i++)
    {
      dx[i] = 0.5 * (it.GetPixel(m_Center + m_xStride[i]) - 
                     it.GetPixel(m_Center - m_xStride[i]));
      
      dx_forward[i] = it.GetPixel(m_Center + m_xStride[i]) - 
        temp_value;
    
      dx_backward[i] =  temp_value - 
        it.GetPixel(m_Center - m_xStride[i]);  
      
      for( int j = 0; j < ImageDimension; j++)
        {
          //calculate the partial derivatives  
          if(j != i)
            {
              dxP[i][j] = 0.5 * (it.GetPixel(m_Center + m_xStride[i] +
                                             m_xStride[j]) - 
                                 it.GetPixel(m_Center + m_xStride[i] - 
                                             m_xStride[j]) );
          
              dxM[i][j] = 0.5 * (it.GetPixel(m_Center - m_xStride[i] + 
                                             m_xStride[j]) - 
                                 it.GetPixel(m_Center - m_xStride[i] -
                                             m_xStride[j]) );
            }
          else
            {
              dxP[i][j] = 0;
              dxM[i][j] = 0;
            }
          
        }//for

    }//for
  
  float F[ImageDimension],B[ImageDimension];
  float normF, normB; 
  curvature_term = 0.0;
  
  //calculate the vectors used to calculate the mean curvature
  for(int k = 0; k < ImageDimension; k ++)
    {

      for( i = 0; i < ImageDimension; i ++)
        {
          F[i] = 0.5 *(dxP[k][i] + dx[i]);
          B[i] = 0.5 *(dxM[k][i] + dx[i]);

        }
      F[k] = dx_forward[k];
      B[k] = dx_backward[k];

      normF = MIN_NORM;
      normB = MIN_NORM;
      for(i = 0; i < ImageDimension; i++)
        {
          normF += F[i] * F[i];
          normB += B[i] * B[i];
        }

      curvature_term += F[k]/vnl_math_sqrt(normF) - B[k]/vnl_math_sqrt(normB);
      
    }
      
  curvature_term *= m_CurvatureWeight * this->CurvatureSpeed(it, offset);

  //
  // Calculate upwind derivatives.  These are used in calculating upwind
  // gradient values and in computing the inner product with the advection
  // field term:
  // $\alpha\stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi$
  //

  /*
  dx_forward  = it.GetPixel(m_Center + 1) - temp_value;
  dx_backward = temp_value - it.GetPixel(m_Center - 1);
  dy_forward  = it.GetPixel(m_Center + m_yStride) - temp_value;
  dy_backward = temp_value - it.GetPixel(m_Center - m_yStride);
  */

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
      advection_term = ZERO;
      
      for(i = 0; i < ImageDimension; i++)
        {

          x_energy[i] = m_AdvectionWeight * advection_field[i];
          
          if (x_energy[i] > ZERO) advection_term += advection_field[i] * dx_backward[i];
          else                 advection_term += advection_field[i] * dx_forward[i];

        }

      advection_term *= m_AdvectionWeight;

      PixelType totalEnergy = ZERO;
      
      for(int i = 0; i < ImageDimension; i++)
        totalEnergy += vnl_math_abs(x_energy[i]);
          
      // Collect energy change from the advection term.  This will be used
      // in calculating the maximum time step that can be taken this iteration.
      globalData->m_MaxAdvectionChange
        = vnl_math_max(globalData->m_MaxAdvectionChange, totalEnergy); 
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
            propagation_gradient += vnl_math_sqr( vnl_math_max(dx_backward[i], ZERO) )
              + vnl_math_sqr( vnl_math_min(dx_forward[i],  ZERO) );

        }
      else
        {
          for(i = 0; i< ImageDimension; i++)
            propagation_gradient += vnl_math_sqr( vnl_math_min(dx_backward[i], ZERO) )
              + vnl_math_sqr( vnl_math_max(dx_forward[i],  ZERO) );

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
