/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAnisotropicDiffusion.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkDerivativeHalfForwardOperator.h"
#include "itkDerivativeHalfBackwardOperator.h"
#include "itkDerivativeOperator.h"
#include "itkImageRegionIterator.h"
namespace itk
{

template<class TPixel, unsigned int VDimension>
void
FilterImageAnisotropicDiffusion<TPixel, VDimension>
::GenerateData()
{
  Superclass::GenerateData();  // Allocates output, copies input to output

  TPixelScalarValueType k_adj;
  TPixelScalarValueType grad_mag_avg;
  
  ImageType::Pointer output = this->GetOutput();
  ImageType::Pointer input  = this->GetInput();
  
  // Create a temporary update image.
  ImageType::Pointer delta = ImageType::New();
  delta->SetLargestPossibleRegion(output->GetLargestPossibleRegion());
  delta->SetRequestedRegion(output->GetRequestedRegion());
  delta->SetBufferedRegion(output->GetBufferedRegion());
  delta->Allocate();

  // Define the neighborhood radius we'll need to use.
  unsigned long siz[VDimension];
  Size<VDimension> hoodRadius;
  siz[0] = 2;
  for (int i = 1; i < VDimension; ++i)
    {
      siz[i] = 1;
    }
  hoodRadius.SetSize(siz);
  
  // Do two passes: Non-boundary pixels, then boundary pixels.
  ImageRegion<VDimension> cropped;
  Size<VDimension> szc;
  Index<VDimension> idxc;
  for (int i = 0; i< VDimension; ++i)
    {
      szc[i] = delta->GetRequestedRegion().GetSize()[i] - hoodRadius[i]*2;
      idxc[i]= delta->GetRequestedRegion().GetIndex()[i]+ hoodRadius[i];
    }
  cropped.SetSize(szc);
  cropped.SetIndex(idxc);

  RegionNeighborhoodIterator<TPixel, VDimension>
    rni(hoodRadius, output, cropped);
 
  RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
    bni(hoodRadius, output, delta->GetRequestedRegion());
  
  for (int i=0; i< this->GetIterations(); ++i)
    {
      rni.SetOutputBuffer(delta->GetBufferPointer()
                          +delta->ComputeOffset(idxc));
      bni.SetOutputBuffer(delta->GetBufferPointer());
      grad_mag_avg = this->AverageGradientMagnitudeScalar(this->GetOutput(),
                     this->GetOutput()->GetRequestedRegion());

      //      cout << grad_mag_avg << endl;
      
      k_adj = grad_mag_avg * this->GetConductanceParameter() * -1.0f;

      if (VDimension==2)
        {
          this->AnisotropicDiffuse2D(rni, k_adj);
          this->AnisotropicDiffuse2D(bni, k_adj);
        }
      else if(VDimension==3)
        {
          this->AnisotropicDiffuse3D(rni, k_adj);
          this->AnisotropicDiffuse3D(bni, k_adj);
        }
      else
        {
          this->AnisotropicDiffuseND(rni, k_adj);
          this->AnisotropicDiffuseND(bni, k_adj);
        }
      this->UpdateOutputScalar(delta, this->GetTimeStep());
    }
  delta->Delete();
}

template< class TPixel, unsigned int VDimension >
template< class TNeighborhoodIterator >
void
FilterImageAnisotropicDiffusion<TPixel, VDimension>
::AnisotropicDiffuse2D(TNeighborhoodIterator it, const float k)
{
  enum { X=0, Y=1 };

  TPixelScalarValueType Cx, Cy, Cxd, Cyd;
  TPixelScalarValueType dx_forward, dx_backward, dy_forward, dy_backward;
  TPixelScalarValueType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim;

  DerivativeOperator<TPixel, 2> dx_op;
   dx_op.SetDirection(X);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();
   
  DerivativeOperator<TPixel, 2> dy_op;
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
  const TNeighborhoodIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      //      dx_forward = ScalarTraits<TPixel>::GetScalar(it.GetPixel(6))
      //        - ScalarTraits<TPixel>::GetScalar(it.GetPixel(7));
      //      dx_backward= ScalarTraits<TPixel>::GetScalar(it.GetPixel(8))
      //        - ScalarTraits<TPixel>::GetScalar(it.GetPixel(7));
      //      dy_forward = ScalarTraits<TPixel>::GetScalar(it.GetPixel(12))
      //        - ScalarTraits<TPixel>::GetScalar(it.GetPixel(7));
      //      dy_backward= ScalarTraits<TPixel>::GetScalar(it.GetPixel(2))
      //        - ScalarTraits<TPixel>::GetScalar(it.GetPixel(7));

      dx_forward = it.GetPixel(6) - it.GetPixel(7);
      dx_backward= it.GetPixel(8) - it.GetPixel(7);
      dy_forward = it.GetPixel(12) - it.GetPixel(7);
      dy_backward= it.GetPixel(2) - it.GetPixel(7);
      dx         = it.SlicedInnerProduct(x_slice, dx_op);
      dy         = it.SlicedInnerProduct(y_slice, dy_op);
      dx_aug     = it.SlicedInnerProduct(xa_slice, dx_op);
      dy_aug     = it.SlicedInnerProduct(ya_slice, dy_op);
      dx_dim     = it.SlicedInnerProduct(xd_slice, dx_op);
      dy_dim     = it.SlicedInnerProduct(yd_slice, dy_op);

      Cx = std::exp( (dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug))
                     / k );
      
      Cy = std::exp( (dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug))
                     / k );
      
      Cxd= std::exp( (dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim))
                     / k );
      
      Cyd= std::exp( (dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim))
                     / k );
      
      dx_forward  *= Cx;
      dy_forward  *= Cy;
      dx_backward *= Cxd;
      dy_backward *= Cyd;
      
      *(it.GetOutputBuffer())= dx_forward  + dy_forward + dx_backward + dy_backward;
      //      ScalarTraits<TPixel>::SetScalar(*(it.GetOutputBuffer()), dx_forward +
      //                                      dy_forward+ dx_backward + dy_backward);

    }  
}

template< class TPixel, unsigned int VDimension >
template< class TNeighborhoodIterator >
void
FilterImageAnisotropicDiffusion<TPixel, VDimension>
::AnisotropicDiffuse3D(TNeighborhoodIterator it, const float k)
{
  enum { X=0, Y=1, Z=2 };

  TPixelScalarValueType Cx, Cy, Cxd, Cyd, Cz, Czd;
  TPixelScalarValueType dx_forward, dx_backward, dy_forward, dy_backward,
    dz_forward, dz_backward;
  TPixelScalarValueType dy, dx, dy_aug, dy_dim, dx_aug, dx_dim, dz, dz_aug,
    dz_dim; 

  DerivativeOperator<TPixel, 3> dx_op;
   dx_op.SetDirection(X);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();
   
  DerivativeOperator<TPixel, 3> dy_op;
   dy_op.SetDirection(Y);
   dy_op.SetOrder(1);
   dy_op.CreateDirectional();

  DerivativeOperator<TPixel, 3> dz_op;
   dz_op.SetDirection(Z);
   dz_op.SetOrder(1);
   dz_op.CreateDirectional();

  // Slice the neighborhood 
  // 0   1   2   3   4
  // 5   6   7   8   9
  // 10  11  12  13  14

  // 15  16  17  18  19
  // 20  21 *22* 23  24
  // 25  26  27  28  29

  // 30  31  32  33  34
  // 35  36  37  38  39
  // 40  41  42  43  44
   
  std::slice  x_slice(21, 3, 1);
  std::slice  y_slice(17, 3, 5);
  std::slice  z_slice(7, 3, 15);
  std::slice xa_slice(22, 3, 1);
  std::slice ya_slice(18, 3, 5);
  std::slice za_slice(8, 3, 15);
  std::slice xd_slice(20, 3, 1);
  std::slice yd_slice(16, 3, 5);
  std::slice zd_slice(6, 3, 15);

  // Process the image
  const TNeighborhoodIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      dx_forward = it.GetPixel(21) - it.GetPixel(22);
      dx_backward= it.GetPixel(23) - it.GetPixel(22);
      dy_forward = it.GetPixel(27) - it.GetPixel(22);
      dy_backward= it.GetPixel(17) - it.GetPixel(22);
      dz_forward = it.GetPixel(37) - it.GetPixel(22);
      dz_backward= it.GetPixel(7)  - it.GetPixel(22);
      dx         = it.SlicedInnerProduct(x_slice, dx_op);
      dy         = it.SlicedInnerProduct(y_slice, dy_op);
      dz         = it.SlicedInnerProduct(z_slice, dz_op);
      dx_aug     = it.SlicedInnerProduct(xa_slice, dx_op);
      dy_aug     = it.SlicedInnerProduct(ya_slice, dy_op);
      dz_aug     = it.SlicedInnerProduct(za_slice, dz_op);
      dx_dim     = it.SlicedInnerProduct(xd_slice, dx_op);
      dy_dim     = it.SlicedInnerProduct(yd_slice, dy_op);
      dz_dim     = it.SlicedInnerProduct(zd_slice, dz_op);

      Cx = std::exp( (dx_forward*dx_forward + 0.25f*(dy+dy_aug)*(dy+dy_aug)
                      +0.25f*(dz+dz_aug)*(dz+dz_aug))
                     / k );
      
      Cy = std::exp( (dy_forward*dy_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug)
                      +0.25f*(dz+dz_aug)*(dz+dz_aug))
                     / k );
      
      Cz = std::exp( (dz_forward*dz_forward + 0.25f*(dx+dx_aug)*(dx+dx_aug)
                      +0.25f*(dy+dy_aug)*(dy+dy_aug))
                     / k );
      
      Cxd= std::exp( (dx_backward*dx_backward + 0.25f*(dy+dy_dim)*(dy+dy_dim)
                      +0.25f*(dz+dz_dim)*(dz+dz_dim))
                     / k );
      
      Cyd= std::exp( (dy_backward*dy_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim)
                      +0.25f*(dz+dz_dim)*(dz+dz_dim))
                     / k );

      Czd= std::exp( (dz_backward*dz_backward + 0.25f*(dx+dx_dim)*(dx+dx_dim)
                      +0.25f*(dy+dy_dim)*(dy+dy_dim))
                     / k );
      
      
      dx_forward  *= Cx;
      dy_forward  *= Cy;
      dz_forward  *= Cz;
      dx_backward *= Cxd;
      dy_backward *= Cyd;
      dz_backward *= Czd;
      
      *(it.GetOutputBuffer())= dx_forward  + dy_forward +
              dx_backward + dy_backward + dz_forward + dz_backward;
      //ScalarTraits<TPixel>::SetScalar(*(it.GetOutputBuffer()), dx_forward +
      //                              dy_forward +  dz_forward + dx_backward+
      //                              dy_backward + dz_backward); 
    }  
}


template< class TPixel, unsigned int VDimension >
template< class TNeighborhoodIterator >
void
FilterImageAnisotropicDiffusion<TPixel, VDimension>
::AnisotropicDiffuseND(TNeighborhoodIterator it, const float k)
{
  unsigned int i, j;
  TPixelScalarValueType accum, accum_d, delta;
  TPixelScalarValueType Cx[VDimension];
  TPixelScalarValueType Cxd[VDimension];
  TPixelScalarValueType dx_forward[VDimension];
  TPixelScalarValueType dx_backward[VDimension];
  TPixelScalarValueType dx[VDimension];
  TPixelScalarValueType dx_aug[VDimension];
  TPixelScalarValueType dx_dim[VDimension];
  
  DerivativeOperator<TPixel, VDimension> dx_op;
   dx_op.SetDirection(0);
   dx_op.SetOrder(1);
   dx_op.CreateDirectional();

  // Slice the neighborhood
  unsigned long stride[VDimension];
  const unsigned long center =  it.size() / 2;
  std::slice x_slice[VDimension];
  std::slice xa_slice[VDimension];
  std::slice xd_slice[VDimension];
  for (i = 0; i< VDimension; ++i)
    {
      stride[i]   = it.GetStride(i);
      x_slice[i]  = std::slice( center - stride[i],  3, stride[i]);
      xa_slice[i] = std::slice((center+1)-stride[i], 3, stride[i]);
      xd_slice[i] = std::slice((center-1)-stride[i], 3, stride[i]);
    }

  // Process the image
  const TNeighborhoodIterator it_end = it.End();
  for (it = it.Begin(); it < it_end; ++it)
    {
      delta = NumericTraits<TPixelScalarValueType>::Zero;
      for (i = 0; i < VDimension; ++i)  // Calculate all derivatives
        {
          dx_forward[i] =
            it.GetPixel(center-stride[i]) - it.GetPixel(center); 

          dx_backward[i]=
            it.GetPixel(center+stride[i]) - it.GetPixel(center); 

          dx[i]     = it.SlicedInnerProduct( x_slice[i], dx_op);
          dx_aug[i] = it.SlicedInnerProduct(xa_slice[i], dx_op);
          dx_dim[i] = it.SlicedInnerProduct(xd_slice[i], dx_op);
        }
      for (i = 0; i< VDimension; ++i) // Calculate conductance terms
        { 
          accum   = NumericTraits<TPixelScalarValueType>::Zero;
          accum_d = NumericTraits<TPixelScalarValueType>::Zero;;
          for (j = 0; j < VDimension; ++j)
            {
              if (j != i)
                {
                  accum   += 0.25f * (dx[j]+dx_aug[j]) * (dx[j]+dx_aug[j]);
                  accum_d += 0.25f * (dx[j]+dx_dim[j]) * (dx[j]+dx_dim[j]);
                }
            }
          Cx[i] = std::exp(( dx_forward[i] * dx_forward[i]  + accum)  / k);
          Cxd[i]= std::exp((dx_backward[i] * dx_backward[i] + accum_d)/ k);
          dx_forward[i]  *= Cx[i];
          dx_backward[i] *= Cxd[i];
          delta += dx_forward[i] + dx_backward[i];
        }
      *(it.GetOutputBuffer()) = delta;
    }  
}






} // end namespace itk
