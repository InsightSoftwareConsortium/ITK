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
double VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::m_MIN_NORM = 1.0e-10;
  
template<class TImage>
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::VectorCurvatureNDAnisotropicDiffusionEquation()
{
  unsigned int i, j;
  RadiusType r;

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
    { m_Stride[i] = it.GetStride(i); }

  for (i = 0; i< ImageDimension; ++i)
    { x_slice[i]  = std::slice( m_Center - m_Stride[i], 3, m_Stride[i]); }
  
  for (i = 0; i< ImageDimension; ++i)
    {
      for (j = 0; j < ImageDimension; ++j)
        {
          // For taking derivatives in the i direction that are offset one
          // pixel in the j direction.
          xa_slice[i][j]
            = std::slice((m_Center + m_Stride[j])-m_Stride[i], 3, m_Stride[i]); 
          xd_slice[i][j]
            = std::slice((m_Center - m_Stride[j])-m_Stride[i], 3, m_Stride[i]);
        }
    }
  
  // Allocate the derivative operator.
  dx_op.SetDirection(0); // Not relelevant, we'll apply in a slice-based
                         // fashion 
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
}

template<class TImage>
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, void *globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j, k;
  double speed, dx_forward_Cn[ImageDimension][VectorDimension],
    dx_backward_Cn[ImageDimension][VectorDimension], 
    propagation_gradient;
  double grad_mag_sq[VectorDimension], grad_mag_sq_d[VectorDimension], 
    grad_mag[ImageDimension], grad_mag_d[ImageDimension];
  double Cx[ImageDimension], Cxd[ImageDimension];
  
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;
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
    }

  for (k = 0; k < VectorDimension; k++)
    {
      grad_mag_sq[k]   = 0.0;
      grad_mag_sq_d[k] = 0.0;
      for (i = 0; i < ImageDimension; i++)
        {
          // Gradient magnitude approximations
          grad_mag_sq[k]   += dx_forward[i][k]  * dx_forward[i][k];
          grad_mag_sq_d[k] += dx_backward[i][k] * dx_backward[i][k];
          for (j = 0; j < ImageDimension; j++)
            {
              if (j != i)
                {
                  dx_aug     = m_InnerProduct(xa_slice[j][i],it, dx_op);
                  dx_dim     = m_InnerProduct(xd_slice[j][i],it, dx_op);
                  grad_mag_sq[k]   += 0.25f * (dx[j][k]+dx_aug[k]) * (dx[j][k]+dx_aug[k]);
                  grad_mag_sq_d[k] += 0.25f * (dx[j][k]+dx_dim[k]) * (dx[j][k]+dx_dim[k]);
                }
            }
        }

      grad_mag[k]   = ::sqrt(m_MIN_NORM + grad_mag_sq[k]);
      grad_mag_d[k] = ::sqrt(m_MIN_NORM + grad_mag_sq_d[k]);
      // this grad mag should depend only on the current k
      for (i = 0; i < ImageDimension; i++)
        {
          dx_forward_Cn[i][k] = dx_forward[i][k]/grad_mag[k];
          dx_backward_Cn[i][k] = dx_backward[i][k]/grad_mag_d[k];
        }
    }

  double grad_mag_sq_tmp = 0.0f;
  double grad_mag_sq_d_tmp = 0.0f;

  for (k = 0; k < VectorDimension; k++)
    {
      grad_mag_sq_tmp += grad_mag_sq[k];
      grad_mag_sq_d_tmp += grad_mag_sq_d[k];
    }

  // this grad mag should depend on the sum over k's
  // Conductance Terms

  for (i = 0; i < ImageDimension; i++)
    {
      Cx[i]  = ::exp( grad_mag_sq_tmp   / m_k );
      Cxd[i] = ::exp( grad_mag_sq_d_tmp / m_k );
    }

  for (k = 0; k < VectorDimension; k++)
    {
      // First order normalized finite-difference conductance products
      speed = 0.0;
      for (i = 0; i < ImageDimension; i++)
        {
          dx_forward_Cn[i][k]  *= Cx[i];
          dx_backward_Cn[i][k] *= Cxd[i];
      
          // Second order conductance-modified curvature
          speed += (dx_forward_Cn[i][k] - dx_backward_Cn[i][k]);
        }
      
      // ``Upwind'' gradient magnitude term
      propagation_gradient = 0.0;
      if (speed > 0.0)
        {  
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_min(dx_backward[i][k], 0.0) )
                + vnl_math_sqr( vnl_math_max(dx_forward[i][k],  0.0) );
            }
        }
      else
        {
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_max(dx_backward[i][k], 0.0) )
                + vnl_math_sqr( vnl_math_min(dx_forward[i][k],  0.0) );
            }
        }
  
  
      ans[k] = ::sqrt(propagation_gradient) * speed;
    }
  
  return ans;    
}

template<class TImage>
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>::PixelType
VectorCurvatureNDAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void *globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j, k;
  double speed, dx_forward_Cn[ImageDimension][VectorDimension], dx_backward_Cn[ImageDimension][VectorDimension], 
    propagation_gradient;
  double grad_mag_sq[VectorDimension], grad_mag_sq_d[VectorDimension], 
    grad_mag[ImageDimension], grad_mag_d[ImageDimension];
  double Cx[ImageDimension], Cxd[ImageDimension];
  
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;
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
    }

  for (k = 0; k < VectorDimension; k++)
    {
      grad_mag_sq[k]   = 0.0;
      grad_mag_sq_d[k] = 0.0;
      for (i = 0; i < ImageDimension; i++)
        {
          // Gradient magnitude approximations
          grad_mag_sq[k]   += dx_forward[i][k]  * dx_forward[i][k];
          grad_mag_sq_d[k] += dx_backward[i][k] * dx_backward[i][k];
          for (j = 0; j < ImageDimension; j++)
            {
              if (j != i)
                {
                  dx_aug     = m_SmartInnerProduct(xa_slice[j][i],it, dx_op);
                  dx_dim     = m_SmartInnerProduct(xd_slice[j][i],it, dx_op);
                  grad_mag_sq[k]   += 0.25f * (dx[j][k]+dx_aug[k]) * (dx[j][k]+dx_aug[k]);
                  grad_mag_sq_d[k] += 0.25f * (dx[j][k]+dx_dim[k]) * (dx[j][k]+dx_dim[k]);
                }
            }
        }

      grad_mag[k]   = ::sqrt(m_MIN_NORM + grad_mag_sq[k]);
      grad_mag_d[k] = ::sqrt(m_MIN_NORM + grad_mag_sq_d[k]);
      // this grad mag should depend only on the current k
      for (i = 0; i < ImageDimension; i++)
        {
          dx_forward_Cn[i][k] = dx_forward[i][k]/grad_mag[k];
          dx_backward_Cn[i][k] = dx_backward[i][k]/grad_mag_d[k];
        }
    }

  double grad_mag_sq_tmp = 0.0f;
  double grad_mag_sq_d_tmp = 0.0f;

  for (k = 0; k < VectorDimension; k++)
    {
      grad_mag_sq_tmp += grad_mag_sq[k];
      grad_mag_sq_d_tmp += grad_mag_sq_d[k];
    }

  // this grad mag should depend on the sum over k's
  // Conductance Terms

  for (i = 0; i < ImageDimension; i++)
    {
      Cx[i]  = ::exp( grad_mag_sq_tmp   / m_k );
      Cxd[i] = ::exp( grad_mag_sq_d_tmp / m_k );
    }

  for (k = 0; k < VectorDimension; k++)
    {
      // First order normalized finite-difference conductance products
      speed = 0.0;
      for (i = 0; i < ImageDimension; i++)
        {
          dx_forward_Cn[i][k]  *= Cx[i];
          dx_backward_Cn[i][k] *= Cxd[i];
      
          // Second order conductance-modified curvature
          speed += (dx_forward_Cn[i][k] - dx_backward_Cn[i][k]);
        }
      
      // ``Upwind'' gradient magnitude term
      propagation_gradient = 0.0;
      if (speed > 0.0)
        {  
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_min(dx_backward[i][k], 0.0) )
                + vnl_math_sqr( vnl_math_max(dx_forward[i][k],  0.0) );
            }
        }
      else
        {
          for (i = 0; i < ImageDimension; i++)
            {
              propagation_gradient +=
                vnl_math_sqr( vnl_math_max(dx_backward[i][k], 0.0) )
                + vnl_math_sqr( vnl_math_min(dx_forward[i][k],  0.0) );
            }
        }
  
  
      ans[k] = ::sqrt(propagation_gradient) * speed;
    }
  
  return ans;    
}

} // end namespace itk

#endif
