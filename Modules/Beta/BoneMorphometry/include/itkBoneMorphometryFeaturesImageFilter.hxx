/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkBoneMorphometryFeaturesImageFilter_hxx
#define itkBoneMorphometryFeaturesImageFilter_hxx


#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TMaskImage>

BoneMorphometryFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::BoneMorphometryFeaturesImageFilter()
  : m_Threshold(1)
{
  this->SetNumberOfRequiredInputs(1);

  using NeighborhoodType = Neighborhood<typename TInputImage::PixelType, TInputImage::ImageDimension>;
  NeighborhoodType nhood;
  nhood.SetRadius(2);
  this->m_NeighborhoodRadius = nhood.GetRadius();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
BoneMorphometryFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateOutputInformation()
{
  // Call superclass's version
  Superclass::GenerateOutputInformation();

  TOutputImage * output = this->GetOutput();
  // If the output image type is a VectorImage the number of
  // components will be properly sized if before allocation, if the
  // output is a fixed width vector and the wrong number of
  // components, then an exception will be thrown.
  if (output->GetNumberOfComponentsPerPixel() != 5)
  {
    output->SetNumberOfComponentsPerPixel(5);
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
BoneMorphometryFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::DynamicThreadedGenerateData(
  const RegionType & outputRegionForThread)
{
  NeighborhoodOffsetType            offsetX = { { 0, 0, 1 } };
  NeighborhoodOffsetType            offsetXO = { { 0, 0, -1 } };
  NeighborhoodOffsetType            offsetY = { { 0, 1, 0 } };
  NeighborhoodOffsetType            offsetYO = { { 0, -1, 0 } };
  NeighborhoodOffsetType            offsetZ = { { 1, 0, 0 } };
  NeighborhoodOffsetType            offsetZO = { { -1, 0, 0 } };
  NeighborhoodOffsetType            tempOffset;
  typename TInputImage::SpacingType inSpacing = this->GetInput()->GetSpacing();

  MaskImagePointer maskPointer = TMaskImage::New();
  maskPointer = const_cast<TMaskImage *>(this->GetMaskImage());

  IndexType firstIndex;
  firstIndex[0] = 0;
  firstIndex[1] = 0;
  firstIndex[2] = 0;
  TOutputImage *                   outputPtr = this->GetOutput();
  typename TOutputImage::PixelType outputPixel = outputPtr->GetPixel(firstIndex);

  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>                        boundaryFacesCalculator;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>::FaceListType faceList =
    boundaryFacesCalculator(this->GetInput(), outputRegionForThread, m_NeighborhoodRadius);
  auto fit = faceList.begin();

  for (; fit != faceList.end(); ++fit)
  {
    NeighborhoodIteratorType inputNIt(m_NeighborhoodRadius, this->GetInput(), *fit);
    BoundaryConditionType    BoundaryCondition;
    inputNIt.SetBoundaryCondition(BoundaryCondition);
    inputNIt.GoToBegin();

    using IteratorType = itk::ImageRegionIterator<TOutputImage>;
    IteratorType outputIt(outputPtr, *fit);

    while (!inputNIt.IsAtEnd())
    {
      if (maskPointer && maskPointer->GetPixel(inputNIt.GetIndex()) == 0)
      {
        outputPixel.Fill(0);
        outputIt.Set(outputPixel);
        ++inputNIt;
        ++outputIt;
        continue;
      }

      SizeValueType numVoxels = 0;
      SizeValueType numBoneVoxels = 0;
      SizeValueType numX = 0;
      SizeValueType numY = 0;
      SizeValueType numZ = 0;
      SizeValueType numXO = 0;
      SizeValueType numYO = 0;
      SizeValueType numZO = 0;

      // Iteration over the all neighborhood region
      for (NeighborIndexType nb = 0; nb < inputNIt.Size(); ++nb)
      {
        IndexType ind = inputNIt.GetIndex(nb);

        if (maskPointer && !(this->IsInsideMaskRegion(ind, maskPointer->GetBufferedRegion().GetSize())))
        {
          continue;
        }

        if (maskPointer && maskPointer->GetPixel(ind) == 0)
        {
          continue;
        }

        ++numVoxels;
        inputNIt.GetPixel(nb);
        tempOffset = inputNIt.GetOffset(nb);
        if (inputNIt.GetPixel(tempOffset) >= m_Threshold)
        {
          ++numBoneVoxels;
          if (this->IsInsideNeighborhood(tempOffset + offsetX) && inputNIt.GetPixel(tempOffset + offsetX) < m_Threshold)
          {
            ++numXO;
          }
          if (this->IsInsideNeighborhood(tempOffset + offsetXO) &&
              inputNIt.GetPixel(tempOffset + offsetXO) < m_Threshold)
          {
            ++numX;
          }
          if (this->IsInsideNeighborhood(tempOffset + offsetY) && inputNIt.GetPixel(tempOffset + offsetY) < m_Threshold)
          {
            ++numYO;
          }
          if (this->IsInsideNeighborhood(tempOffset + offsetYO) &&
              inputNIt.GetPixel(tempOffset + offsetYO) < m_Threshold)
          {
            ++numY;
          }
          if (this->IsInsideNeighborhood(tempOffset + offsetZ) && inputNIt.GetPixel(tempOffset + offsetZ) < m_Threshold)
          {
            ++numZO;
          }
          if (this->IsInsideNeighborhood(tempOffset + offsetZO) &&
              inputNIt.GetPixel(tempOffset + offsetZO) < m_Threshold)
          {
            ++numZ;
          }
        }
      }

      RealType PlX = (RealType)((numX + numXO) / 2.0) / (RealType)(numVoxels * inSpacing[0]) * 2;
      RealType PlY = (RealType)((numY + numYO) / 2.0) / (RealType)(numVoxels * inSpacing[1]) * 2;
      RealType PlZ = (RealType)((numZ + numZO) / 2.0) / (RealType)(numVoxels * inSpacing[2]) * 2;
      outputPixel[0] = (RealType)numBoneVoxels / (RealType)numVoxels;
      outputPixel[1] = (PlX + PlY + PlZ) / 3.0;
      outputPixel[2] = outputPixel[0] / outputPixel[1];
      outputPixel[3] = (1.0 - outputPixel[0]) / outputPixel[1];
      outputPixel[4] = 2.0 * (outputPixel[1] / outputPixel[0]);

      outputIt.Set(outputPixel);

      ++inputNIt;
      ++outputIt;
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
bool
BoneMorphometryFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::IsInsideMaskRegion(
  const IndexType &                     imageIndex,
  const typename TMaskImage::SizeType & maskSize)
{
  bool insideMask = true;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
  {
    if (imageIndex[i] < 0 || imageIndex[i] >= static_cast<long>(maskSize[i]))
    {
      insideMask = false;
      break;
    }
  }
  return insideMask;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
bool
BoneMorphometryFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::IsInsideNeighborhood(
  const NeighborhoodOffsetType & iteratedOffset)
{
  bool insideNeighborhood = true;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
  {
    int boundDistance = m_NeighborhoodRadius[i] - itk::Math::abs(iteratedOffset[i]);
    if (boundDistance < 0)
    {
      insideNeighborhood = false;
      break;
    }
  }
  return insideNeighborhood;
}


template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
BoneMorphometryFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os,
                                                                                     Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "m_Threshold: " << m_Threshold << std::endl;
}
} // end namespace itk

#endif // itkBoneMorphometryFeaturesImageFilter_hxx
