/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientAnisotropicDiffusionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkDerivativeOperator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"

namespace itk
{
  
template<class TInnerProduct, class TIterator >
void AnisoDiffuseGrad2D<TInnerProduct, TIterator>
::operator()(void *d1, void *d2)
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::PixelType ScalarValueType;
  enum { ImageDimension = ImageType::ImageDimension };
  enum { X=0, Y=1 };

  typename ImageType::Pointer input = static_cast<ImageType*>(d1);
  typename ImageType::Pointer output= static_cast<ImageType*>(d2);

  TInnerProduct IP;
  
  AvgGradMagSquared<ImageType> GradMag;
  const float k = GradMag(input, input->GetRequestedRegion())
    * this->m_ConductanceTerm * -1.0f;
  
  // set up the iterator
  Size<ImageType::ImageDimension> hR;
  hR[0] = 2;
  hR[1] = 1;
  TIterator it(hR, input, input->GetRequestedRegion());
  it.SetOutputBuffer(output->GetBufferPointer()
                     + output->ComputeOffset(it.GetStartIndex()));

  // set up the operators
  ScalarValueType Cx, Cy, Cxd, Cyd;
  ScalarValueType dx_forward, dx_backward, dy_forward, dy_backward;
  ScalarValueType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;

  DerivativeOperator<ScalarValueType, 2> dx_op;
   dx_op.SetDirection(X);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();
   
  DerivativeOperator<ScalarValueType, 2> dy_op;
   dy_op.SetDirection(Y);
   dy_op.SetOrder(1);
   dy_op.CreateDirectional();

  // Slice the neighborhood
  // 0  1  2  3  4
  // 5  6 *7* 8  9
  // 10 11 12 13 14
  std::slice  x_slice(6, 3, 1);
  std::slice  y_slice(2, 3, 5);
  std::slice xa_slice(7, 3, 1);
  std::slice ya_slice(3, 3, 5);
  std::slice xd_slice(5, 3, 1);
  std::slice yd_slice(1, 3, 5);

  // Process the image
  const TIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      dx_forward = it.GetPixel(6) - it.GetPixel(7);
      dx_backward= it.GetPixel(8) - it.GetPixel(7);
      dy_forward = it.GetPixel(12) - it.GetPixel(7);
      dy_backward= it.GetPixel(2) - it.GetPixel(7);
      dx         = IP(x_slice, it, dx_op);
      dy         = IP(y_slice, it, dy_op);
      dx_aug     = IP(xa_slice,it, dx_op);
      dy_aug     = IP(ya_slice,it, dy_op);
      dx_dim     = IP(xd_slice,it, dx_op);
      dy_dim     = IP(yd_slice,it, dy_op);

      Cx = exp( (dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug))
                     / k );
      
      Cy = exp( (dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug))
                     / k );
      
      Cxd= exp( (dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim))
                     / k );
      
      Cyd= exp( (dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim))
                     / k );
      
      dx_forward  *= Cx;
      dy_forward  *= Cy;
      dx_backward *= Cxd;
      dy_backward *= Cyd;
      
      *(it.GetOutputBuffer())= dx_forward  + dy_forward
        + dx_backward + dy_backward;
    }  
}


template<class TInnerProduct, class TIterator >
void AnisoDiffuseGradND<TInnerProduct, TIterator>
::operator()(void *d1, void *d2)
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  enum { ImageDimension = ImageType::ImageDimension };

  typename ImageType::Pointer input = static_cast<ImageType*>(d1);
  typename ImageType::Pointer output= static_cast<ImageType*>(d2);

  TInnerProduct IP;
  
  AvgGradMagSquared<ImageType> GradMag;
  const float k = GradMag(input, input->GetRequestedRegion())
    * this->m_ConductanceTerm * -1.0f;
  
  // set up the iterator
  Size<ImageDimension> hR;
  hR[0] = 2;
  for (int i = 1; i < ImageDimension; ++i) hR[i] = 1;

  TIterator it(hR, input, input->GetRequestedRegion());
  it.SetOutputBuffer(output->GetBufferPointer()
                     + output->ComputeOffset(it.GetStartIndex()));

  // Set up the operators
  unsigned int i, j;
  ScalarValueType accum, accum_d, delta;
  ScalarValueType Cx[ImageDimension];
  ScalarValueType Cxd[ImageDimension];
  ScalarValueType dx_forward[ImageDimension];
  ScalarValueType dx_backward[ImageDimension];
  ScalarValueType dx[ImageDimension];
  ScalarValueType dx_aug[ImageDimension];
  ScalarValueType dx_dim[ImageDimension];
  
  DerivativeOperator<ScalarValueType, ImageDimension> dx_op;
   dx_op.SetDirection(0);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();

  // Slice the neighborhood
  unsigned long stride[ImageDimension];
  const unsigned long center =  it.Size() / 2;
  std::slice x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension];
  std::slice xd_slice[ImageDimension];
  for (i = 0; i< ImageDimension; ++i)
    {
      stride[i]   = it.GetStride(i);
      x_slice[i]  = std::slice( center - stride[i],  3, stride[i]);
      xa_slice[i] = std::slice((center+1)-stride[i], 3, stride[i]);
      xd_slice[i] = std::slice((center-1)-stride[i], 3, stride[i]);
    }

  // Process the image
  const TIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      delta = NumericTraits<ScalarValueType>::Zero;
      for (i = 0; i < ImageDimension; ++i)  // Calculate all derivatives
        {
          dx_forward[i] =
            it.GetPixel(center-stride[i]) - it.GetPixel(center); 

          dx_backward[i]=
            it.GetPixel(center+stride[i]) - it.GetPixel(center); 

          dx[i]     = IP( x_slice[i], it, dx_op);
          dx_aug[i] = IP(xa_slice[i], it, dx_op);
          dx_dim[i] = IP(xd_slice[i], it, dx_op);
        }
      for (i = 0; i< ImageDimension; ++i) // Calculate conductance terms
        { 
          accum   = NumericTraits<ScalarValueType>::Zero;
          accum_d = NumericTraits<ScalarValueType>::Zero;;
          for (j = 0; j < ImageDimension; ++j)
            {
              if (j != i)
                {
                  accum   += 0.25f * (dx[j]+dx_aug[j]) * (dx[j]+dx_aug[j]);
                  accum_d += 0.25f * (dx[j]+dx_dim[j]) * (dx[j]+dx_dim[j]);
                }
            }
          Cx[i] = exp(( dx_forward[i] * dx_forward[i]  + accum)  / k);
          Cxd[i]= exp((dx_backward[i] * dx_backward[i] + accum_d)/ k);
          dx_forward[i]  *= Cx[i];
          dx_backward[i] *= Cxd[i];
          delta += dx_forward[i] + dx_backward[i];
        }
      *(it.GetOutputBuffer()) = delta;
    }  
}


} // end namespace itk
