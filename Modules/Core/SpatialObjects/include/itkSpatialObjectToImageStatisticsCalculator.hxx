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
#ifndef itkSpatialObjectToImageStatisticsCalculator_hxx
#define itkSpatialObjectToImageStatisticsCalculator_hxx

#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include "itkMeanSampleFilter.h"
#include "itkCovarianceSampleFilter.h"
#include "itkImageMaskSpatialObject.h"

namespace itk
{
/** Constructor */
template <typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension>
SpatialObjectToImageStatisticsCalculator<TInputImage, TInputSpatialObject, TSampleDimension>::
  SpatialObjectToImageStatisticsCalculator()
{
  m_Image = nullptr;
  m_SpatialObject = nullptr;
  m_Mean.Fill(0);
  m_CovarianceMatrix.SetIdentity();
  m_SampleDirection = TSampleDimension - 1;
  m_InternalImageTime = 0;
  m_InternalSpatialObjectTime = 0;
  m_Sum = 0;
  m_NumberOfPixels = 0;
  m_Sample = SampleType::New();
}

/** Compute Statistics from the Sample vector */
template <typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension>
bool
SpatialObjectToImageStatisticsCalculator<TInputImage, TInputSpatialObject, TSampleDimension>::ComputeStatistics()
{
  using MeanAlgorithmType = itk::Statistics::MeanSampleFilter<SampleType>;
  typename MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New();
  meanAlgorithm->SetInput(m_Sample);
  meanAlgorithm->Update();

  typename MeanAlgorithmType::MeasurementVectorType mean = meanAlgorithm->GetMean();

  for (unsigned int i = 0; i < SampleDimension; i++)
  {
    m_Mean[i] = mean[i];
  }

  using CovarianceAlgorithmType = itk::Statistics::CovarianceSampleFilter<SampleType>;
  typename CovarianceAlgorithmType::Pointer covarianceAlgorithm = CovarianceAlgorithmType::New();

  covarianceAlgorithm->SetInput(m_Sample);
  covarianceAlgorithm->Update();

  typename CovarianceAlgorithmType::MatrixType covarianceMatrix = covarianceAlgorithm->GetCovarianceMatrix();
  for (unsigned int i = 0; i < covarianceMatrix.Rows(); i++)
  {
    for (unsigned int j = 0; j < covarianceMatrix.Rows(); j++)
    {
      m_CovarianceMatrix(i, j) = covarianceMatrix(i, j);
    }
  }

  return true;
}

/** */
template <typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension>
void
SpatialObjectToImageStatisticsCalculator<TInputImage, TInputSpatialObject, TSampleDimension>::Update()
{
  if (!m_Image || !m_SpatialObject)
  {
    itkExceptionMacro("SpatialObjectToImageStatisticsCalculator: set image AND spatialObject.");
  }

  // Update only if the image or the spatial object has been modified
  if ((m_Image->GetMTime() == m_InternalImageTime) && (m_SpatialObject->GetMTime() == m_InternalSpatialObjectTime))
  {
    return; // No need to update
  }

  m_InternalImageTime = m_Image->GetMTime();
  m_InternalSpatialObjectTime = m_SpatialObject->GetMTime();

  m_Sample = SampleType::New();
  m_Sample->SetMeasurementVectorSize(SampleDimension);

  m_NumberOfPixels = 0;
  m_Sum = 0;

  // If this is an ImageMaskSpatialObject we cannot use the flood filled
  // iterator
  if (!strcmp(m_SpatialObject->GetTypeName().c_str(), "ImageMaskSpatialObject"))
  {
    using MaskImageType = Image<unsigned char, Self::ObjectDimension>;
    using MaskSOType = ImageMaskSpatialObject<Self::ObjectDimension>;

    typename MaskSOType::Pointer maskSpatialObject = dynamic_cast<MaskSOType *>(m_SpatialObject.GetPointer());
    if (maskSpatialObject.IsNull())
    {
      itkExceptionMacro("Invalid dynamic cast.");
    }

    typename MaskImageType::ConstPointer maskImage = maskSpatialObject->GetImage();

    using MaskIteratorType = ImageRegionConstIteratorWithIndex<MaskImageType>;
    MaskIteratorType it(maskImage, maskImage->GetLargestPossibleRegion());
    it.GoToBegin();
    IndexType  ind;
    PointType  pnt;
    PointType  tPnt;
    VectorType mv;
    while (!it.IsAtEnd())
    {
      if (it.Get() > 0) // if inside the mask
      {
        ind = it.GetIndex();
        maskImage->TransformIndexToPhysicalPoint(ind, pnt);
        tPnt = maskSpatialObject->GetObjectToWorldTransform()->TransformPoint(pnt);
        ind = m_Image->TransformPhysicalPointToIndex(tPnt);
        mv[0] = m_Image->GetPixel(ind);
        m_Sum += static_cast<AccumulateType>(mv[0]);
        for (unsigned int i = 1; i < Self::SampleDimension; i++)
        {
          ind[m_SampleDirection] += 1;
          mv[i] = m_Image->GetPixel(ind);
          m_Sum += static_cast<AccumulateType>(mv[i]);
        }
        m_Sample->PushBack(mv);
        m_NumberOfPixels++;
      }
      ++it;
    }
  }
  else
  {
    // Get the bounding box
    m_SpatialObject->ComputeFamilyBoundingBox(SpatialObjectType::MaximumDepth);
    m_SpatialObject->Update();
    const typename SpatialObjectType::BoundingBoxType::BoundsArrayType bounds =
      m_SpatialObject->GetFamilyBoundingBoxInWorldSpace()->GetBounds();

    Point<double, Self::ObjectDimension> ptMin;
    Point<double, Self::ObjectDimension> ptMax;
    SizeType                             size;
    for (unsigned int i = 0; i < Self::ObjectDimension; i++)
    {
      ptMin[i] = bounds[i * 2];
      ptMax[i] = bounds[i * 2 + 1];
    }
    auto      indMin = m_Image->TransformPhysicalPointToIndex(ptMin);
    auto      indMax = m_Image->TransformPhysicalPointToIndex(ptMax);
    IndexType imageIndex = m_Image->GetLargestPossibleRegion().GetIndex();
    SizeType  imageSize = m_Image->GetLargestPossibleRegion().GetSize();
    for (unsigned int i = 0; i < Self::ObjectDimension; i++)
    {
      if (indMin[i] > indMax[i])
      {
        int tmpI = indMin[i];
        indMin[i] = indMax[i];
        indMax[i] = tmpI;
      }
      if (indMin[i] < imageIndex[i])
      {
        indMin[i] = imageIndex[i];
      }
      size[i] = indMax[i] - indMin[i] + 1;
      if (indMin[i] + size[i] < imageIndex[i] + imageSize[i])
      {
        size[i] = imageIndex[i] + imageSize[i] - indMin[i];
      }
    }

    RegionType region;
    region.SetIndex(indMin);
    region.SetSize(size);

    using IteratorType = ImageRegionConstIteratorWithIndex<ImageType>;
    IteratorType it = IteratorType(m_Image, region);
    it.GoToBegin();
    IndexType  ind;
    PointType  pnt;
    VectorType mv;
    while (!it.IsAtEnd())
    {
      ind = it.GetIndex();
      m_Image->TransformIndexToPhysicalPoint(ind, pnt);
      if (m_SpatialObject->IsInsideInWorldSpace(pnt))
      {
        mv[0] = it.Get();
        m_Sum += static_cast<AccumulateType>(mv[0]);
        for (unsigned int i = 1; i < Self::SampleDimension; i++)
        {
          ind[m_SampleDirection] += 1;
          mv[i] = m_Image->GetPixel(ind);
          m_Sum += static_cast<AccumulateType>(mv[i]);
        }
        m_Sample->PushBack(mv);
        m_NumberOfPixels++;
      }
      ++it;
    }
  }

  this->ComputeStatistics();
}

template <typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension>
void
SpatialObjectToImageStatisticsCalculator<TInputImage, TInputSpatialObject, TSampleDimension>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Image: " << m_Image << std::endl;
  os << indent << "SpatialObject: " << m_SpatialObject << std::endl;
  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance Matrix: " << m_CovarianceMatrix << std::endl;
  os << indent << "Sum: " << m_Sum << std::endl;
  os << indent << "m_NumberOfPixels: " << m_NumberOfPixels << std::endl;
  os << indent << "Internal Image Time: " << m_InternalImageTime << std::endl;
  os << indent << "Internal Spatial Object Time: " << m_InternalSpatialObjectTime << std::endl;
  os << indent << "SampleDirection: " << m_SampleDirection << std::endl;
  os << indent << "Sample: " << m_Sample << std::endl;
}

} // end namespace itk

#endif
