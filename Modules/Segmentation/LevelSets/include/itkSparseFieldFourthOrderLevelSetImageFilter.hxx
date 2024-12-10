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
#ifndef itkSparseFieldFourthOrderLevelSetImageFilter_hxx
#define itkSparseFieldFourthOrderLevelSetImageFilter_hxx

#include "itkNeighborhoodIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
const SizeValueType SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::m_NumVertex =
  1 << ImageDimension;

template <typename TInputImage, typename TOutputImage>
const typename SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::ValueType
  SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::m_DimConst =
    static_cast<ValueType>(2.0 / m_NumVertex);

template <typename TInputImage, typename TOutputImage>
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::SparseFieldFourthOrderLevelSetImageFilter()
{
  m_RefitIteration = 0;
  m_LevelSetFunction = nullptr;
  m_ConvergenceFlag = false;

  this->SetIsoSurfaceValue(0);
  m_MaxRefitIteration = 100;
  m_MaxNormalIteration = 25;
  m_RMSChangeNormalProcessTrigger = ValueType{};
  m_CurvatureBandWidth = static_cast<ValueType>(ImageDimension) + 0.5;
  m_NormalProcessType = 0;
  m_NormalProcessConductance = ValueType{};
  m_NormalProcessUnsharpFlag = false;
  m_NormalProcessUnsharpWeight = ValueType{};
}

template <typename TInputImage, typename TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaxRefitIteration: " << m_MaxRefitIteration << std::endl;
  os << indent << "MaxNormalIteration: " << m_MaxNormalIteration << std::endl;
  os << indent << "CurvatureBandWidth: " << m_CurvatureBandWidth << std::endl;

  os << indent << "RMSChangeNormalProcessTrigger: " << m_RMSChangeNormalProcessTrigger << std::endl;

  os << indent << "NormalProcessType: " << m_NormalProcessType << std::endl;

  os << indent << "NormalProcessConductance: " << m_NormalProcessConductance << std::endl;

  os << indent << "NormalProcessUnsharpFlag: " << m_NormalProcessUnsharpFlag << std::endl;

  os << indent << "NormalProcessUnsharpWeight: " << m_NormalProcessUnsharpWeight << std::endl;
}

template <typename TInputImage, typename TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::SetLevelSetFunction(LevelSetFunctionType * lsf)
{
  m_LevelSetFunction = lsf;
  Superclass::SetDifferenceFunction(lsf);
}

template <typename TInputImage, typename TOutputImage>
auto
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::ComputeCurvatureFromSparseImageNeighborhood(
  SparseImageIteratorType & it) const -> ValueType
{
  constexpr SizeValueType      one = 1;
  const SizeValueType          center = it.Size() / 2;
  bool                         flag = false;
  const NeighborhoodScalesType neighborhoodScales = this->GetDifferenceFunction()->ComputeNeighborhoodScales();

  SizeValueType stride[ImageDimension];
  SizeValueType indicator[ImageDimension];
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    stride[j] = it.GetStride(j);
    indicator[j] = one << j;
  }

  auto curvature = ValueType{};

  for (unsigned int counter = 0; counter < m_NumVertex; ++counter)
  {
    SizeValueType position = center;
    for (unsigned int k = 0; k < ImageDimension; ++k)
    {
      if (counter & indicator[k])
      {
        position -= stride[k];
      }
    }
    if (it.GetPixel(position) == nullptr)
    {
      flag = true;
    }
    else
    {
      NormalVectorType normalvector = it.GetPixel(position)->m_Data;
      for (unsigned int j = 0; j < ImageDimension; ++j) // derivative axis
      {
        if (counter & indicator[j])
        {
          curvature -= normalvector[j] * neighborhoodScales[j];
        }
        else
        {
          curvature += normalvector[j] * neighborhoodScales[j];
        }
      } // end derivative axis
    }
  } // end counter

  if (flag)
  {
    curvature = ValueType{};
  }
  curvature *= m_DimConst;
  return curvature;
}

template <typename TInputImage, typename TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::ComputeCurvatureTarget(
  const OutputImageType * distanceImage,
  SparseImageType *       sparseImage) const
{
  using DistanceImageIteratorType = ImageRegionConstIterator<OutputImageType>;

  DistanceImageIteratorType distanceImageIterator(distanceImage, distanceImage->GetRequestedRegion());
  typename SparseImageIteratorType::RadiusType radius;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    radius[j] = 1;
  }
  SparseImageIteratorType sparseImageIterator(radius, sparseImage, sparseImage->GetRequestedRegion());

  sparseImageIterator.GoToBegin();
  distanceImageIterator.GoToBegin();
  while (!distanceImageIterator.IsAtEnd())
  {
    const ValueType distance = distanceImageIterator.Value();
    NodeType *      node = sparseImageIterator.GetCenterPixel();
    if ((distance >= -m_CurvatureBandWidth) && (distance <= m_CurvatureBandWidth))
    {
      node->m_Curvature = ComputeCurvatureFromSparseImageNeighborhood(sparseImageIterator);
      node->m_CurvatureFlag = true;
    }
    else
    {
      if (node != nullptr)
      {
        node->m_CurvatureFlag = false;
      }
    }
    ++sparseImageIterator;
    ++distanceImageIterator;
  }
}

template <typename TInputImage, typename TOutputImage>
bool
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::ActiveLayerCheckBand() const
{
  const typename SparseImageType::Pointer im = m_LevelSetFunction->GetSparseTargetImage();
  bool                                    flag = false;

  typename LayerType::Iterator layerIt = this->m_Layers[0]->Begin();
  while (layerIt != this->m_Layers[0]->End())
  {
    NodeType * node = im->GetPixel(layerIt->m_Value);
    if ((node == nullptr) || (node->m_CurvatureFlag == false))
    {
      // level set touching edge of normal band
      flag = true;
      break;
    }
    ++layerIt;
  }
  return flag;
}

template <typename TInputImage, typename TOutputImage>
void
SparseFieldFourthOrderLevelSetImageFilter<TInputImage, TOutputImage>::ProcessNormals()
{
  auto temp = static_cast<ValueType>(ImageDimension);

  const typename NormalVectorFilterType::Pointer   NormalVectorFilter = NormalVectorFilterType::New();
  const typename NormalVectorFunctionType::Pointer NormalVectorFunction = NormalVectorFunctionType::New();
  NormalVectorFunction->SetNormalProcessType(m_NormalProcessType);
  NormalVectorFunction->SetConductanceParameter(m_NormalProcessConductance);
  NormalVectorFilter->SetNormalFunction(NormalVectorFunction);
  NormalVectorFilter->SetIsoLevelLow(-m_CurvatureBandWidth - temp);
  NormalVectorFilter->SetIsoLevelHigh(m_CurvatureBandWidth + temp);
  NormalVectorFilter->SetMaxIteration(m_MaxNormalIteration);
  NormalVectorFilter->SetUnsharpMaskingFlag(m_NormalProcessUnsharpFlag);
  NormalVectorFilter->SetUnsharpMaskingWeight(m_NormalProcessUnsharpWeight);

  // Move the pixel container and image information of the image we are working
  // on into a temporary image to  use as the input to the mini-pipeline.  This
  // avoids a complete copy of the image.
  const typename OutputImageType::Pointer phi = this->GetOutput();
  auto                                    tmp = OutputImageType::New();
  tmp->SetRequestedRegion(phi->GetRequestedRegion());
  tmp->SetBufferedRegion(phi->GetBufferedRegion());
  tmp->SetLargestPossibleRegion(phi->GetLargestPossibleRegion());
  tmp->SetPixelContainer(phi->GetPixelContainer());
  tmp->CopyInformation(phi);

  NormalVectorFilter->SetInput(tmp);
  NormalVectorFilter->Update();

  const typename SparseImageType::Pointer SparseNormalImage = NormalVectorFilter->GetOutput();

  this->ComputeCurvatureTarget(tmp, SparseNormalImage);
  m_LevelSetFunction->SetSparseTargetImage(SparseNormalImage);
}
} // end namespace itk

#endif
