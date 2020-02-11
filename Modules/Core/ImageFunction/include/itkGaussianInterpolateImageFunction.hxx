/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#ifndef itkGaussianInterpolateImageFunction_hxx
#define itkGaussianInterpolateImageFunction_hxx

#include "itkGaussianInterpolateImageFunction.h"

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

template <typename TImageType, typename TCoordRep>
GaussianInterpolateImageFunction<TImageType, TCoordRep>::GaussianInterpolateImageFunction()
  : m_Alpha(1.0)
{
  this->m_Sigma.Fill(1.0);

  this->m_BoundingBoxStart.Fill(-0.5);
  this->m_BoundingBoxEnd.Fill(1.0);
  this->m_ScalingFactor.Fill(1.0);
  this->m_CutOffDistance.Fill(1.0);
}

template <typename TImageType, typename TCoordRep>
void
GaussianInterpolateImageFunction<TImageType, TCoordRep>::ComputeBoundingBox()
{
  if (!this->GetInputImage())
  {
    return;
  }

  typename InputImageType::ConstPointer input = this->GetInputImage();
  typename InputImageType::SpacingType  spacing = input->GetSpacing();
  typename InputImageType::IndexType    index = input->GetLargestPossibleRegion().GetIndex();
  typename InputImageType::SizeType     size = input->GetLargestPossibleRegion().GetSize();

  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    this->m_BoundingBoxStart[d] = index[d] - 0.5;
    this->m_BoundingBoxEnd[d] = static_cast<RealType>(index[d] + size[d]) - 0.5;
    this->m_ScalingFactor[d] = 1.0 / (itk::Math::sqrt2 * this->m_Sigma[d] / spacing[d]);
    this->m_CutOffDistance[d] = this->m_Sigma[d] * this->m_Alpha / spacing[d];
  }
}


template <typename TInputImage, typename TCoordRep>
typename GaussianInterpolateImageFunction<TInputImage, TCoordRep>::RegionType
GaussianInterpolateImageFunction<TInputImage, TCoordRep>::ComputeInterpolationRegion(
  const ContinuousIndexType & cindex) const
{
  RegionType region = this->GetInputImage()->GetBufferedRegion();
  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    TCoordRep      cBegin = cindex[d] + 0.5 - this->m_CutOffDistance[d];
    IndexValueType begin = std::max(region.GetIndex()[d], static_cast<IndexValueType>(std::floor(cBegin)));
    TCoordRep      cEnd = cindex[d] + 0.5 + this->m_CutOffDistance[d];
    SizeValueType  end =
      std::min(region.GetIndex()[d] + region.GetSize()[d], static_cast<SizeValueType>(std::ceil(cEnd)));
    region.SetIndex(d, begin);
    region.SetSize(d, end - begin);
  }
  return region;
}

template <typename TImageType, typename TCoordRep>
typename GaussianInterpolateImageFunction<TImageType, TCoordRep>::OutputType
GaussianInterpolateImageFunction<TImageType, TCoordRep>::EvaluateAtContinuousIndex(const ContinuousIndexType & cindex,
                                                                                   OutputType * grad) const
{
  vnl_vector<RealType> erfArray[ImageDimension];
  vnl_vector<RealType> gerfArray[ImageDimension];

  RegionType region = this->ComputeInterpolationRegion(cindex);

  // Compute the ERF difference arrays
  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    this->ComputeErrorFunctionArray(region, d, cindex[d], erfArray[d], gerfArray[d], grad != nullptr);
  }

  RealType  sum_me = 0.0;
  RealType  sum_m = 0.0;
  ArrayType dsum_me;
  ArrayType dsum_m;
  ArrayType dw;

  dsum_m.Fill(0.0);
  dsum_me.Fill(0.0);
  dw.Fill(0.0);


  ImageRegionConstIteratorWithIndex<InputImageType> It(this->GetInputImage(), region);
  for (It.GoToBegin(); !It.IsAtEnd(); ++It)
  {
    unsigned int j = It.GetIndex()[0] - region.GetIndex()[0];
    RealType     w = erfArray[0][j];
    if (grad)
    {
      dw[0] = gerfArray[0][j];
      for (unsigned int d = 1; d < ImageDimension; d++)
      {
        dw[d] = erfArray[0][j];
      }
    }
    for (unsigned int d = 1; d < ImageDimension; d++)
    {
      j = It.GetIndex()[d] - region.GetIndex()[d];
      w *= erfArray[d][j];
      if (grad)
      {
        for (unsigned int q = 0; q < ImageDimension; q++)
        {
          if (d == q)
          {
            dw[q] *= gerfArray[d][j];
          }
          else
          {
            dw[q] *= erfArray[d][j];
          }
        }
      }
    }
    RealType V = It.Get();
    sum_me += V * w;
    sum_m += w;
    if (grad)
    {
      for (unsigned int q = 0; q < ImageDimension; q++)
      {
        dsum_me[q] += V * dw[q];
        dsum_m[q] += dw[q];
      }
    }
  }
  RealType rc = sum_me / sum_m;

  if (grad)
  {
    for (unsigned int q = 0; q < ImageDimension; q++)
    {
      grad[q] = (dsum_me[q] - rc * dsum_m[q]) / sum_m;
      grad[q] /= -itk::Math::sqrt2 * this->m_Sigma[q];
    }
  }

  return rc;
}

template <typename TImageType, typename TCoordRep>
void
GaussianInterpolateImageFunction<TImageType, TCoordRep>::ComputeErrorFunctionArray(const RegionType &     region,
                                                                                   unsigned int           dimension,
                                                                                   RealType               cindex,
                                                                                   vnl_vector<RealType> & erfArray,
                                                                                   vnl_vector<RealType> & gerfArray,
                                                                                   bool evaluateGradient) const
{
  erfArray.set_size(region.GetSize()[dimension]);
  gerfArray.set_size(region.GetSize()[dimension]);

  // Start at the first voxel
  RealType t = (this->m_BoundingBoxStart[dimension] - cindex + static_cast<RealType>(region.GetIndex()[dimension])) *
               this->m_ScalingFactor[dimension];
  RealType e_last = vnl_erf(t);
  RealType g_last = 0.0;
  if (evaluateGradient)
  {
    g_last = itk::Math::two_over_sqrtpi * std::exp(-itk::Math::sqr(t));
  }

  for (unsigned i = 0; i < region.GetSize()[dimension]; i++)
  {
    t += this->m_ScalingFactor[dimension];
    RealType e_now = vnl_erf(t);
    erfArray[i] = e_now - e_last;
    if (evaluateGradient)
    {
      RealType g_now = itk::Math::two_over_sqrtpi * std::exp(-itk::Math::sqr(t));
      gerfArray[i] = g_now - g_last;
      g_last = g_now;
    }
    e_last = e_now;
  }
}


template <typename TImageType, typename TCoordRep>
typename GaussianInterpolateImageFunction<TImageType, TCoordRep>::SizeType
GaussianInterpolateImageFunction<TImageType, TCoordRep>::GetRadius() const
{
  SizeType radius;

  if (!this->GetInputImage())
  {
    itkExceptionMacro("Input image required!");
  }

  const InputImageType *                     input = this->GetInputImage();
  const typename InputImageType::SpacingType spacing = input->GetSpacing();

  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    radius[dim] = static_cast<SizeValueType>(Math::ceil(m_CutOffDistance[dim] / spacing[dim]));
  }
  return radius;
}


template <typename TImageType, typename TCoordRep>
void
GaussianInterpolateImageFunction<TImageType, TCoordRep>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Alpha: " << this->m_Alpha << std::endl;
  os << indent << "Sigma: " << this->m_Sigma << std::endl;

  os << indent << "Bounding box start: " << this->m_BoundingBoxStart << std::endl;
  os << indent << "Bounding box end: " << this->m_BoundingBoxEnd << std::endl;
  os << indent << "Scaling factor: " << this->m_ScalingFactor << std::endl;
  os << indent << "Cut-off distance: " << this->m_CutOffDistance << std::endl;
}
} // namespace itk

#endif
