/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorCurvatureNDAnisotropicDiffusionEquation.txx
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
#ifndef __itkVectorCurvatureNDAnisotropicDiffusionEquation_txx_
#define __itkVectorCurvatureNDAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::VectorCurvatureNDAnisotropicDiffusionEquation()
{
  unsigned int i;
  RadiusType r;
  r[0] = 2;
  for (i = 0; i < ImageDimension; ++i)
    {
      r[i] = 1;
    }
  this->SetRadius(r);

  // Dummy neighborhood used to set up the slices.
  Neighborhood<PixelType, ImageDimension> it;
  it.SetRadius(r);
  
 // Slice the neighborhood
  m_Center =  it.Size() / 2;

  for (i = 0; i< ImageDimension; ++i)
    {
      m_Stride[i]   = it.GetStride(i);
      x_slice[i]  = std::slice( m_Center - m_Stride[i],  3, m_Stride[i]);
      xa_slice[i] = std::slice((m_Center+1)-m_Stride[i], 3, m_Stride[i]);
      xd_slice[i] = std::slice((m_Center-1)-m_Stride[i], 3, m_Stride[i]);
    }

  // Allocate the derivative operator.
  dx_op.SetDirection(0);
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template<class TImage>
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, void *globalData,
                const FloatOffsetType& offset) const
{
  const ScalarValueType Zero = NumericTraits<ScalarValueType>::Zero;
  
  unsigned int i, j, k;
  ScalarValueType speed, dx_forward_Cn, dx_backward_Cn, propagation_gradient;
  ScalarValueType grad_mag_sq, grad_mag_sq_d, grad_mag, grad_mag_d;
  ScalarValueType Cx[ImageDimension], Cxd[ImageDimension];
  
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug[ImageDimension];
  PixelType dx_dim[ImageDimension];
  PixelType ans;

  // Calculate the partial derivatives for each dimension
  for (i = 0; i < ImageDimension; i++)
    {
      // ``Half'' derivatives
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]= it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);
      
      // Centralized differences
      dx[i]         = m_InnerProduct(x_slice[i], it, dx_op);
      dx_aug[i]     = m_InnerProduct(xa_slice[i],it, dx_op);
      dx_dim[i]     = m_InnerProduct(xd_slice[i],it, dx_op);
    }

  for (i = 0; i < ImageDimension; i++)
    {
      grad_mag_sq   = 0.0;
      grad_mag_sq_d = 0.0;
      for (k = 0; k < VectorDimension; k++)
        {
          // Gradient magnitude approximations
          grad_mag_sq   += dx_forward[i][k]  * dx_forward[i][k];
          grad_mag_sq_d += dx_backward[i][k] * dx_backward[i][k];
          for (j = 0; j < ImageDimension; j++)
            {
              if (j != i)
                {
                  grad_mag_sq   += 0.25f * (dx[j][k]+dx_aug[j][k]) * (dx[j][k]+dx_aug[j][k]);
                  grad_mag_sq_d += 0.25f * (dx[j][k]+dx_dim[j][k]) * (dx[j][k]+dx_dim[j][k]);
                }
            }
        }
      grad_mag   = vnl_math_sqrt(grad_mag_sq);
      grad_mag_d = vnl_math_sqrt(grad_mag_sq_d);
      
      // Conductance Terms
      Cx[i]  = ::exp( grad_mag_sq   / m_k );
      Cxd[i] = ::exp( grad_mag_sq_d / m_k );
      
      if (grad_mag != Zero)    Cx[i] = Cx[i] / grad_mag;
      else    Cx[i] = Zero;
      if (grad_mag_d != Zero)  Cxd[i] = Cxd[i] / grad_mag_d;
      else    Cxd[i] = Zero;
    }


  for (k = 0; k < VectorDimension; k++)
    {
      // First order normalized finite-difference conductance products
      speed = Zero;
      for (i = 0; i < ImageDimension; i++)
        {
          dx_forward_Cn  = dx_forward[i][k]  * Cx[i];
          dx_backward_Cn = dx_backward[i][k] * Cxd[i];
      
          // Second order conductance-modified curvature
          speed += (dx_forward_Cn - dx_backward_Cn);
        }
      
      // ``Upwind'' gradient magnitude term
      propagation_gradient = Zero;
      if (speed > Zero)
        {  
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                  vnl_math_sqr( vnl_math_min(dx_backward[i][k], Zero) )
                + vnl_math_sqr( vnl_math_max(dx_forward[i][k],  Zero) );
            }
        }
      else
        {
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_max(dx_backward[i][k], Zero) )
                + vnl_math_sqr( vnl_math_min(dx_forward[i][k],  Zero) );
            }
        }
  
  
      ans[k] = vnl_math_sqrt(propagation_gradient) * speed;
    }
  
  return ans;    
}

template<class TImage>
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void *globalData,
                const FloatOffsetType& offset) const
{
    const ScalarValueType Zero = NumericTraits<ScalarValueType>::Zero;
  
  unsigned int i, j, k;
  ScalarValueType speed, dx_forward_Cn, dx_backward_Cn, propagation_gradient;
  ScalarValueType grad_mag_sq, grad_mag_sq_d, grad_mag, grad_mag_d;
  ScalarValueType Cx[ImageDimension], Cxd[ImageDimension];
  
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug[ImageDimension];
  PixelType dx_dim[ImageDimension];
  PixelType ans;

  // Calculate the partial derivatives for each dimension
  for (i = 0; i < ImageDimension; i++)
    {
      // ``Half'' derivatives
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]= it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);
      
      // Centralized differences
      dx[i]         = m_SmartInnerProduct(x_slice[i], it, dx_op);
      dx_aug[i]     = m_SmartInnerProduct(xa_slice[i],it, dx_op);
      dx_dim[i]     = m_SmartInnerProduct(xd_slice[i],it, dx_op);
    }

  for (i = 0; i < ImageDimension; i++)
    {
      grad_mag_sq   = 0.0;
      grad_mag_sq_d = 0.0;
      for (k = 0; k < VectorDimension; k++)
        {
          // Gradient magnitude approximations
          grad_mag_sq   += dx_forward[i][k]  * dx_forward[i][k];
          grad_mag_sq_d += dx_backward[i][k] * dx_backward[i][k];
          for (j = 0; j < ImageDimension; j++)
            {
              if (j != i)
                {
                  grad_mag_sq   += 0.25f * (dx[j][k]+dx_aug[j][k]) * (dx[j][k]+dx_aug[j][k]);
                  grad_mag_sq_d += 0.25f * (dx[j][k]+dx_dim[j][k]) * (dx[j][k]+dx_dim[j][k]);
                }
            }
        }
      grad_mag   = vnl_math_sqrt(grad_mag_sq);
      grad_mag_d = vnl_math_sqrt(grad_mag_sq_d);
      
      // Conductance Terms
      Cx[i]  = ::exp( grad_mag_sq   / m_k );
      Cxd[i] = ::exp( grad_mag_sq_d / m_k );
      
      if (grad_mag != Zero)   Cx[i] = Cx[i] / grad_mag;
      else    Cx[i] = Zero;
      if (grad_mag_d != Zero)  Cxd[i] = Cxd[i] / grad_mag_d;
      else    Cxd[i] = Zero;
    }


  for (k = 0; k < VectorDimension; k++)
    {
      // First order normalized finite-difference conductance products
      speed = Zero;
      for (i = 0; i < ImageDimension; i++)
        {
          dx_forward_Cn  = dx_forward[i][k]  * Cx[i];
          dx_backward_Cn = dx_backward[i][k] * Cxd[i];
      
          // Second order conductance-modified curvature
          speed += (dx_forward_Cn - dx_backward_Cn);
        }
      
      // ``Upwind'' gradient magnitude term
      propagation_gradient = Zero;
      if (speed > Zero)
        {  
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_min(dx_backward[i][k], Zero) )
                + vnl_math_sqr( vnl_math_max(dx_forward[i][k],  Zero) );
            }
        }
      else
        {
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_max(dx_backward[i][k], Zero) )
                + vnl_math_sqr( vnl_math_min(dx_forward[i][k],  Zero) );
            }
        }
  
  
      ans[k] = vnl_math_sqrt(propagation_gradient) * speed;
    }
  
  return ans;    
}

} // end namespace itk

#endif
