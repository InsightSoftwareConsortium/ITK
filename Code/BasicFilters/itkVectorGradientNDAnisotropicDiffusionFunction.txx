/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientNDAnisotropicDiffusionFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorGradientNDAnisotropicDiffusionFunction_txx_
#define __itkVectorGradientNDAnisotropicDiffusionFunction_txx_

namespace itk {

template<class TImage>
double VectorGradientNDAnisotropicDiffusionFunction<TImage>
::m_MIN_NORM = 1.0e-10;
  
template<class TImage>
VectorGradientNDAnisotropicDiffusionFunction<TImage>
::VectorGradientNDAnisotropicDiffusionFunction()
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
typename VectorGradientNDAnisotropicDiffusionFunction<TImage>::PixelType
VectorGradientNDAnisotropicDiffusionFunction<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j, k;
  PixelType delta;

  double GradMag;
  double GradMag_d;
  double Cx[ImageDimension];
  double Cxd[ImageDimension];

  // Remember: PixelType is a Vector of length VectorDimension.
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;

  // Calculate the directional and centralized derivatives.
  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]=  it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);
      dx[i]      = m_InnerProduct(x_slice[i], it, dx_op);
    }

  // Calculate the conductance term for each dimension.
  for (i = 0; i < ImageDimension; i++)
    {
      // Calculate gradient magnitude approximation in this
      // dimension linked (summed) across the vector components.
      GradMag   = 0.0;
      GradMag_d = 0.0;
      for (k =0; k < VectorDimension; k++)
        {
          GradMag   +=  vnl_math_sqr( dx_forward[i][k] );
          GradMag_d +=  vnl_math_sqr( dx_backward[i][k] );

          for (j = 0; j < ImageDimension; j++)
            {
              if ( j != i)
                {
                  dx_aug  = m_InnerProduct(xa_slice[j][i], it, dx_op);
                  dx_dim  = m_InnerProduct(xd_slice[j][i], it, dx_op);
                  GradMag   += 0.25f * vnl_math_sqr( dx[j][k]+dx_aug[k] );
                  GradMag_d += 0.25f * vnl_math_sqr( dx[j][k]+dx_dim[k] );
                }
            }
        }
      
      Cx[i]  = ::exp( GradMag   / m_K );
      Cxd[i] = ::exp( GradMag_d / m_K ); 
    }

  // Compute update value  
  for (k = 0; k < VectorDimension; k++)
    {
      delta[k] = NumericTraits<ScalarValueType>::Zero;
      
      for (i = 0; i < ImageDimension; ++i)
        {
          dx_forward[i][k]  *= Cx[i];
          dx_backward[i][k] *= Cxd[i];
          delta[k] += dx_forward[i][k] - dx_backward[i][k];
        }
    }
      
  return delta;
}

template<class TImage>
typename VectorGradientNDAnisotropicDiffusionFunction<TImage>::PixelType
VectorGradientNDAnisotropicDiffusionFunction<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  unsigned int i, j, k;
  PixelType delta;
  
  double GradMag;
  double GradMag_d;
  double Cx[ImageDimension];
  double Cxd[ImageDimension];

  // Remember: PixelType is a Vector of length VectorDimension.
  PixelType dx_forward[ImageDimension];
  PixelType dx_backward[ImageDimension];
  PixelType dx[ImageDimension];
  PixelType dx_aug;
  PixelType dx_dim;

  // Calculate the directional and centralized derivatives.
  for (i = 0; i < ImageDimension; i++)
    {
      dx_forward[i] = it.GetPixel(m_Center + m_Stride[i])
        - it.GetPixel(m_Center);
      dx_backward[i]=  it.GetPixel(m_Center)
        - it.GetPixel(m_Center - m_Stride[i]);
      dx[i]      = m_SmartInnerProduct(x_slice[i], it, dx_op);
    }

  // Calculate the conductance term for each dimension.
  for (i = 0; i < ImageDimension; i++)
    {
      // Calculate gradient magnitude approximation in this
      // dimension linked (summed) across the vector components.
      GradMag   = 0.0;
      GradMag_d = 0.0;
      for (k =0; k < VectorDimension; k++)
        {
          GradMag   +=  vnl_math_sqr( dx_forward[i][k] );
          GradMag_d +=  vnl_math_sqr( dx_backward[i][k] );

          for (j = 0; j < ImageDimension; j++)
            {
              if ( j != i)
                {
                  dx_aug  = m_SmartInnerProduct(xa_slice[j][i], it, dx_op);
                  dx_dim  = m_SmartInnerProduct(xd_slice[j][i], it, dx_op);
                  GradMag   += 0.25f * vnl_math_sqr( dx[j][k]+dx_aug[k] );
                  GradMag_d += 0.25f * vnl_math_sqr( dx[j][k]+dx_dim[k] );
                }
            }
        }
      
      Cx[i]  = ::exp( GradMag   / m_K );
      Cxd[i] = ::exp( GradMag_d / m_K ); 
    }

  // Compute update value  
  for (k = 0; k < VectorDimension; k++)
    {
      delta[k] = NumericTraits<ScalarValueType>::Zero;
      
      for (i = 0; i < ImageDimension; ++i)
        {
          dx_forward[i][k]  *= Cx[i];
          dx_backward[i][k] *= Cxd[i];
          delta[k] += dx_forward[i][k] - dx_backward[i][k];
        }
    }
      
  return delta;
}

} // end namespace itk

#endif
