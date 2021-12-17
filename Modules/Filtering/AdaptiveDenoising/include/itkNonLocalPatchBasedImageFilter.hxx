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
#ifndef itkNonLocalPatchBasedImageFilter_hxx
#define itkNonLocalPatchBasedImageFilter_hxx


#include "itkNeighborhood.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::NonLocalPatchBasedImageFilter()
{
  this->m_SimilarityMetric = SimilarityMetricEnum::MEAN_SQUARES;

  this->m_NeighborhoodPatchRadius.Fill(1);
  this->m_NeighborhoodPatchOffsetList.clear();

  this->m_NeighborhoodSearchRadius.Fill(3);
  this->m_NeighborhoodSearchOffsetList.clear();
}

template <typename TInputImage, typename TOutputImage>
void
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  // Set up the search neighborhood parameters

  this->m_NeighborhoodSearchOffsetList.clear();

  NeighborhoodType searchNeighborhood;
  searchNeighborhood.SetRadius(this->m_NeighborhoodSearchRadius);

  this->m_NeighborhoodSearchSize = searchNeighborhood.Size();
  for (unsigned int n = 0; n < this->m_NeighborhoodSearchSize; n++)
  {
    this->m_NeighborhoodSearchOffsetList.push_back(searchNeighborhood.GetOffset(n));
  }

  // Set up the patch neighborhood parameters

  this->m_NeighborhoodPatchOffsetList.clear();

  NeighborhoodType patchNeighborhood;
  patchNeighborhood.SetRadius(this->m_NeighborhoodPatchRadius);

  this->m_NeighborhoodPatchSize = patchNeighborhood.Size();
  for (unsigned int n = 0; n < this->m_NeighborhoodPatchSize; n++)
  {
    this->m_NeighborhoodPatchOffsetList.push_back(patchNeighborhood.GetOffset(n));
  }

  this->m_TargetImageRegion = this->GetInput()->GetRequestedRegion();
}

template <typename TInputImage, typename TOutputImage>
typename NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::InputImagePixelVectorType
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::VectorizeImageListPatch(const InputImageList & imageList,
                                                                                  const IndexType        index,
                                                                                  const bool             normalize)
{
  InputImagePixelVectorType patchVector(this->m_NeighborhoodPatchSize * imageList.size());
  for (unsigned int i = 0; i < imageList.size(); i++)
  {
    InputImagePixelVectorType patchVectorPerModality = this->VectorizeImagePatch(imageList[i], index, normalize);
    for (unsigned int j = 0; j < this->m_NeighborhoodPatchSize; j++)
    {
      patchVector[i * this->m_NeighborhoodPatchSize + j] = patchVectorPerModality[j];
    }
  }
  return patchVector;
}

template <typename TInputImage, typename TOutputImage>
typename NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::InputImagePixelVectorType
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::VectorizeImagePatch(const InputImagePointer image,
                                                                              const IndexType         index,
                                                                              const bool              normalize)
{
  InputImagePixelVectorType patchVector(this->m_NeighborhoodPatchSize);
  for (SizeValueType i = 0; i < this->m_NeighborhoodPatchSize; i++)
  {
    IndexType neighborhoodIndex = index + this->m_NeighborhoodPatchOffsetList[i];

    bool isInBounds = this->m_TargetImageRegion.IsInside(neighborhoodIndex);
    if (isInBounds)
    {
      InputPixelType pixel = image->GetPixel(neighborhoodIndex);
      patchVector[i] = pixel;
    }
    else
    {
      patchVector[i] = std::numeric_limits<RealType>::quiet_NaN();
    }
  }

  if (normalize)
  {
    RealType mean = 0.0;
    RealType standardDeviation = 0.0;
    this->GetMeanAndStandardDeviationOfVectorizedImagePatch(patchVector, mean, standardDeviation);

    standardDeviation = std::max(standardDeviation, NumericTraits<RealType>::OneValue());

    typename InputImagePixelVectorType::iterator it;
    for (it = patchVector.begin(); it != patchVector.end(); ++it)
    {
      *it = (*it - mean) / standardDeviation;
    }
  }
  return patchVector;
}

template <typename TInputImage, typename TOutputImage>
void
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::GetMeanAndStandardDeviationOfVectorizedImagePatch(
  const InputImagePixelVectorType & patchVector,
  RealType &                        mean,
  RealType &                        standardDeviation)
{
  RealType sum = 0.0;
  RealType sumOfSquares = 0.0;
  RealType count = 0.0;

  typename InputImagePixelVectorType::const_iterator it;
  for (it = patchVector.begin(); it != patchVector.end(); ++it)
  {
    if (std::isfinite(*it))
    {
      sum += *it;
      sumOfSquares += itk::Math::sqr(*it);
      count += itk::NumericTraits<RealType>::OneValue();
    }
  }
  mean = sum / count;
  standardDeviation =
    std::sqrt((sumOfSquares - count * itk::Math::sqr(mean)) / (count - itk::NumericTraits<RealType>::OneValue()));
}

template <typename TInputImage, typename TOutputImage>
typename NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::RealType
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::ComputeNeighborhoodPatchSimilarity(
  const InputImageList &            imageList,
  const IndexType                   index,
  const InputImagePixelVectorType & patchVectorY,
  const bool                        useOnlyFirstImage)
{
  unsigned int numberOfImagesToUse = imageList.size();
  if (useOnlyFirstImage)
  {
    numberOfImagesToUse = 1;
  }

  RealType sumX = 0.0;
  RealType sumOfSquaresX = 0.0;
  RealType sumOfSquaredDifferencesXY = 0.0;
  RealType sumXY = 0.0;
  RealType N = 0.0;

  SizeValueType count = 0;
  for (SizeValueType i = 0; i < numberOfImagesToUse; i++)
  {
    for (SizeValueType j = 0; j < this->m_NeighborhoodPatchSize; j++)
    {
      IndexType neighborhoodIndex = index + this->m_NeighborhoodPatchOffsetList[j];

      bool isInBounds = this->m_TargetImageRegion.IsInside(neighborhoodIndex);
      if (isInBounds && std::isfinite(patchVectorY[count]))
      {
        auto x = static_cast<RealType>(imageList[i]->GetPixel(neighborhoodIndex));
        auto y = static_cast<RealType>(patchVectorY[count]);

        sumX += x;
        sumOfSquaresX += itk::Math::sqr(x);
        sumXY += (x * y);

        sumOfSquaredDifferencesXY += itk::Math::sqr(y - x);
        N += itk::NumericTraits<RealType>::OneValue();
      }
      ++count;
    }
  }

  // If we are on the boundary, a neighborhood patch might not overlap
  // with the image.  If we have 2 voxels or less for a neighborhood patch
  // we don't consider it to be a suitable match.
  if (N < static_cast<RealType>(3.0))
  {
    return NumericTraits<RealType>::max();
  }

  if (this->m_SimilarityMetric == SimilarityMetricEnum::PEARSON_CORRELATION)
  {
    RealType varianceX = sumOfSquaresX - itk::Math::sqr(sumX) / N;
    varianceX = std::max(varianceX, static_cast<RealType>(1.0e-6));

    RealType measure = itk::Math::sqr(sumXY) / varianceX;
    if (sumXY > 0)
    {
      return -measure;
    }
    else
    {
      return measure;
    }
  }
  else if (this->m_SimilarityMetric == SimilarityMetricEnum::MEAN_SQUARES)
  {
    return (sumOfSquaredDifferencesXY / N);
  }
  else
  {
    itkExceptionMacro("Unrecognized similarity metric.");
  }
}

template <typename TInputImage, typename TOutputImage>
void
NonLocalPatchBasedImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if (this->m_SimilarityMetric == SimilarityMetricEnum::PEARSON_CORRELATION)
  {
    os << "Using Pearson correlation to measure the patch similarity." << std::endl;
  }
  else if (this->m_SimilarityMetric == SimilarityMetricEnum::MEAN_SQUARES)
  {
    os << "Using mean squares to measure the patch similarity." << std::endl;
  }

  os << indent << "Neighborhood search radius = " << this->m_NeighborhoodSearchRadius << std::endl;
  os << indent << "Neighborhood patch radius = " << this->m_NeighborhoodPatchRadius << std::endl;
}

} // end namespace itk

#endif
