/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorCurvature2DAnisotropicDiffusionEquation.txx
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
#ifndef __itkVectorCurvature2DAnisotropicDiffusionEquation_txx_
#define __itkVectorCurvature2DAnisotropicDiffusionEquation_txx_

namespace itk {

template<class TImage>
VectorCurvature2DAnisotropicDiffusionEquation<TImage>
::VectorCurvature2DAnisotropicDiffusionEquation()
{
  RadiusType r;
  r[0] = 2;
  r[1] = 1;
  this->SetRadius(r);

  x_slice  = std::slice(6,3,1);
  y_slice  = std::slice(2,3,5);
  xa_slice = std::slice(7,3,1);
  ya_slice = std::slice(3,3,5);
  xd_slice = std::slice(5,3,1);
  yd_slice = std::slice(1,3,5);
  
  dx_op.SetDirection(0);
  dx_op.SetOrder(1);
  dx_op.CreateDirectional();
  
  dy_op.SetDirection(1);
  dy_op.SetOrder(1);
  dy_op.CreateDirectional();
}

template<class TImage>
VectorCurvature2DAnisotropicDiffusionEquation<TImage>::PixelType
VectorCurvature2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  unsigned int j;

  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
  PixelType ans;

  double Cx, Cy, Cxd, Cyd;
  double grad_mag_x, grad_mag_y, grad_mag_xd, grad_mag_yd;
  double grad_mag_x_sq, grad_mag_y_sq, grad_mag_xd_sq, grad_mag_yd_sq;
  double dx_forward_Cn, dx_backward_Cn, dy_forward_Cn, dy_backward_Cn;
  double speed, propagation_gradient;
  
  // Centralized differences, vectors
  dx_forward = it.GetPixel(8)  - it.GetPixel(7);
  dx_backward= it.GetPixel(7)  - it.GetPixel(6);
  dy_forward = it.GetPixel(12) - it.GetPixel(7);
  dy_backward= it.GetPixel(7)  - it.GetPixel(2);

  dx         = m_InnerProduct(x_slice, it, dx_op);
  dy         = m_InnerProduct(y_slice, it, dy_op);
  dx_aug     = m_InnerProduct(xa_slice,it, dx_op);
  dy_aug     = m_InnerProduct(ya_slice,it, dy_op);
  dx_dim     = m_InnerProduct(xd_slice,it, dx_op);
  dy_dim     = m_InnerProduct(yd_slice,it, dy_op);

  // Gradient magnitude approximations, scalars
  // Sum of gradient magnitude across the vector components
  grad_mag_x_sq  = 0.0;
  grad_mag_y_sq  = 0.0;
  grad_mag_xd_sq = 0.0;
  grad_mag_yd_sq = 0.0;

  for (j = 0; j < VectorDimension; ++j)
    {
      grad_mag_x_sq
        += dx_forward[j]*dx_forward[j]
                       + 0.25f*(dy[j]+dy_aug[j])*(dy[j]+dy_aug[j]);
      grad_mag_y_sq
        += dy_forward[j]*dy_forward[j]
                       + 0.25f*(dx[j]+dx_aug[j])*(dx[j]+dx_aug[j]);
      grad_mag_xd_sq
        += dx_backward[j]*dx_backward[j]
                       + 0.25f*(dy[j]+dy_dim[j])*(dy[j]+dy_dim[j]);
      grad_mag_yd_sq
        += dy_backward[j]*dy_backward[j]
                       + 0.25f*(dx[j]+dx_dim[j])*(dx[j]+dx_dim[j]);
    }
  grad_mag_x  = ::sqrt(grad_mag_x_sq);
  grad_mag_y  = ::sqrt(grad_mag_y_sq);
  grad_mag_xd = ::sqrt(grad_mag_xd_sq);
  grad_mag_yd = ::sqrt(grad_mag_yd_sq);
    
  // Conductance terms, scalars
  Cx = ::exp( grad_mag_x_sq / m_k );
  Cy = ::exp( grad_mag_y_sq / m_k );
  Cxd= ::exp( grad_mag_xd_sq / m_k );
  Cyd= ::exp( grad_mag_yd_sq / m_k );

  // Normalized finite-difference, conductance products (1st order), vectors
  for (j = 0; j < VectorDimension; ++j)
    {
      if ( grad_mag_x  != 0.0 )
        {  dx_forward_Cn  = (dx_forward[j]  / grad_mag_x) * Cx;  }
      else dx_forward_Cn = 0.0;
      
      if ( grad_mag_y  != 0.0 )
        {  dy_forward_Cn  = (dy_forward[j]  / grad_mag_y) * Cy;  }
      else dy_forward_Cn = 0.0;
      
      if ( grad_mag_xd != 0.0 )
        {  dx_backward_Cn  = (dx_backward[j] / grad_mag_xd)* Cxd; }
      else dx_backward_Cn = 0.0;
      
      if ( grad_mag_yd != 0.0 )
        {  dy_backward_Cn  = (dy_backward[j] / grad_mag_yd)* Cyd; }
      else dy_backward_Cn = 0.0;

      // Conductance-modified curvature (2nd order, speed_x + speed_y)
      speed =(dx_forward_Cn - dx_backward_Cn)+(dy_forward_Cn - dy_backward_Cn);

      // ``Upwind'' gradient magnitude term
      if (speed > 0.0)
        {
          propagation_gradient = 
              vnl_math_sqr( vnl_math_min(dx_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_max(dx_forward[j],  0.0) )
            + vnl_math_sqr( vnl_math_min(dy_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_max(dy_forward[j],  0.0) );
        }
      else
        {
          propagation_gradient =
              vnl_math_sqr( vnl_math_max(dx_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_min(dx_forward[j],  0.0) )
            + vnl_math_sqr( vnl_math_max(dy_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_min(dy_forward[j],  0.0) );
        }
      ans[j] = vnl_math_sqrt(propagation_gradient) * speed;
    }
    
  // Final product
  return ans;
}

template<class TImage>
VectorCurvature2DAnisotropicDiffusionEquation<TImage>::PixelType
VectorCurvature2DAnisotropicDiffusionEquation<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  unsigned int j;

  PixelType dx_forward, dx_backward, dy_forward, dy_backward;
  PixelType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;
  PixelType ans;

  double Cx, Cy, Cxd, Cyd;
  double grad_mag_x, grad_mag_y, grad_mag_xd, grad_mag_yd;
  double grad_mag_x_sq, grad_mag_y_sq, grad_mag_xd_sq, grad_mag_yd_sq;
  double dx_forward_Cn, dx_backward_Cn, dy_forward_Cn, dy_backward_Cn;
  double speed, propagation_gradient;
  
  // Centralized differences, vectors
  dx_forward = it.GetPixel(8)  - it.GetPixel(7);
  dx_backward= it.GetPixel(7)  - it.GetPixel(6);
  dy_forward = it.GetPixel(12) - it.GetPixel(7);
  dy_backward= it.GetPixel(7)  - it.GetPixel(2);

  dx         = m_SmartInnerProduct(x_slice, it, dx_op);
  dy         = m_SmartInnerProduct(y_slice, it, dy_op);
  dx_aug     = m_SmartInnerProduct(xa_slice,it, dx_op);
  dy_aug     = m_SmartInnerProduct(ya_slice,it, dy_op);
  dx_dim     = m_SmartInnerProduct(xd_slice,it, dx_op);
  dy_dim     = m_SmartInnerProduct(yd_slice,it, dy_op);

  // Gradient magnitude approximations, scalars
  // Sum of gradient magnitude across the vector components
  grad_mag_x_sq  = 0.0;
  grad_mag_y_sq  = 0.0;
  grad_mag_xd_sq = 0.0;
  grad_mag_yd_sq = 0.0;

  for (j = 0; j < VectorDimension; ++j)
    {
      grad_mag_x_sq
        += dx_forward[j]*dx_forward[j]
                       + 0.25f*(dy[j]+dy_aug[j])*(dy[j]+dy_aug[j]);
      grad_mag_y_sq
        += dy_forward[j]*dy_forward[j]
                       + 0.25f*(dx[j]+dx_aug[j])*(dx[j]+dx_aug[j]);
      grad_mag_xd_sq
        += dx_backward[j]*dx_backward[j]
                       + 0.25f*(dy[j]+dy_dim[j])*(dy[j]+dy_dim[j]);
      grad_mag_yd_sq
        += dy_backward[j]*dy_backward[j]
                       + 0.25f*(dx[j]+dx_dim[j])*(dx[j]+dx_dim[j]);
    }
  grad_mag_x  = ::sqrt(grad_mag_x_sq);
  grad_mag_y  = ::sqrt(grad_mag_y_sq);
  grad_mag_xd = ::sqrt(grad_mag_xd_sq);
  grad_mag_yd = ::sqrt(grad_mag_yd_sq);
    
  // Conductance terms, scalars
  Cx = ::exp( grad_mag_x_sq / m_k );
  Cy = ::exp( grad_mag_y_sq / m_k );
  Cxd= ::exp( grad_mag_xd_sq / m_k );
  Cyd= ::exp( grad_mag_yd_sq / m_k );

  // Normalized finite-difference, conductance products (1st order), vectors
  for (j = 0; j < VectorDimension; ++j)
    {
      if ( grad_mag_x  != 0.0 )
        {  dx_forward_Cn  = (dx_forward[j]  / grad_mag_x) * Cx;  }
      else dx_forward_Cn = 0.0;
      
      if ( grad_mag_y  != 0.0 )
        {  dy_forward_Cn  = (dy_forward[j]  / grad_mag_y) * Cy;  }
      else dy_forward_Cn = 0.0;
      
      if ( grad_mag_xd != 0.0 )
        {  dx_backward_Cn  = (dx_backward[j] / grad_mag_xd)* Cxd; }
      else dx_backward_Cn = 0.0;
      
      if ( grad_mag_yd != 0.0 )
        {  dy_backward_Cn  = (dy_backward[j] / grad_mag_yd)* Cyd; }
      else dy_backward_Cn = 0.0;

      // Conductance-modified curvature (2nd order, speed_x + speed_y)
      speed =(dx_forward_Cn - dx_backward_Cn)+(dy_forward_Cn - dy_backward_Cn);

      // ``Upwind'' gradient magnitude term
      if (speed > 0.0)
        {
          propagation_gradient = 
              vnl_math_sqr( vnl_math_min(dx_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_max(dx_forward[j],  0.0) )
            + vnl_math_sqr( vnl_math_min(dy_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_max(dy_forward[j],  0.0) );
        }
      else
        {
          propagation_gradient =
              vnl_math_sqr( vnl_math_max(dx_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_min(dx_forward[j],  0.0) )
            + vnl_math_sqr( vnl_math_max(dy_backward[j], 0.0) )
            + vnl_math_sqr( vnl_math_min(dy_forward[j],  0.0) );
        }
      ans[j] = vnl_math_sqrt(propagation_gradient) * speed;
    }
    
  // Final product
  return ans;
}

} // end namespace itk

#endif
