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
  /*
    ToDo:
    1. Implement vanishing viscosity for curvature term (replace epsilon)?
    2. Add in max_curvature term to calculation of dt.
  */
  unsigned int i;  
  const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
  const ScalarValueType MIN_NORM = 1.0e-6;
  
  ScalarValueType curve, gradMag;
  ScalarValueType temp_value;
  ScalarValueType dx[ImageDimension];
  ScalarValueType dx_forward[ImageDimension], dx_backward[ImageDimension];
  ScalarValueType propagation_gradient;
  ScalarValueType propagation_term, curvature_term, advection_term;
  VectorType advection_field;
  ScalarValueType x_energy[ImageDimension];

  // Global data structure
  GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

  temp_value  = it.GetCenterPixel();  

  // Calculate the mean curvature
  ScalarValueType dxx[ImageDimension];
  ScalarValueType dxy[ImageDimension * (ImageDimension-1)/2];

  for( i = 0 ; i < ImageDimension; i++)
    {
    const unsigned int positionA = 
       static_cast<unsigned int>( m_Center + m_xStride[i]);    
    const unsigned int positionB = 
       static_cast<unsigned int>( m_Center - m_xStride[i]);    
    dx[i] = 0.5 * (it.GetPixel( positionA ) - 
                   it.GetPixel( positionB )    );
      
    dx_forward[i]  = it.GetPixel( positionA ) - temp_value;
    dx_backward[i] = temp_value - it.GetPixel( positionB );
    }
  
  int k = 0;

  curve = ZERO;
  
  for (i = 0; i < ImageDimension; i++)
    {
    const unsigned int positionAI = 
       static_cast<unsigned int>( m_Center + m_xStride[i]);    
    const unsigned int positionBI = 
       static_cast<unsigned int>( m_Center - m_xStride[i]);    
      dxx[i] = it.GetPixel( positionAI )
             + it.GetPixel( positionBI ) - 2.0 * temp_value;
      
      for(unsigned int j = i+1; j < ImageDimension; j++)
        {
        const unsigned int positionA = static_cast<unsigned int>( 
                                m_Center - m_xStride[i] - m_xStride[j] );    
        const unsigned int positionB = static_cast<unsigned int>( 
                                m_Center - m_xStride[i] + m_xStride[j] );    
        const unsigned int positionC = static_cast<unsigned int>( 
                                m_Center + m_xStride[i] - m_xStride[j] );    
        const unsigned int positionD = static_cast<unsigned int>( 
                                m_Center + m_xStride[i] + m_xStride[j] );    
        dxy[k] = 0.25 *( it.GetPixel( positionA )
                       - it.GetPixel( positionB )
                       - it.GetPixel( positionC )
                       + it.GetPixel( positionD )  );
         
        curve -= 2.0 * dx[i] * dx[j] * dxy[k]; 
        k++;
        }
    }

  gradMag = MIN_NORM;
  for (i = 0; i < ImageDimension; i++)
    {
      
      for(unsigned int j = 0; j < ImageDimension; j++)
        {
          
          if(j != i)
            curve += dxx[j] * dx[i] * dx[i];
        }
      gradMag += dx[i] * dx[i];
    }

  curve /= gradMag * vcl_sqrt(gradMag);
  
  curvature_term = curve;
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
      
      for(unsigned int i = 0; i < ImageDimension; i++)
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
      
      propagation_term *= vcl_sqrt( propagation_gradient );
    }
  else propagation_term = ZERO;

  // Return the combination of all the terms.
  return ( PixelType ) ( curvature_term - propagation_term - advection_term );
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
