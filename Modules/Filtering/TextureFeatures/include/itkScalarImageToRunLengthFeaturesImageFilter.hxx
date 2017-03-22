/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkScalarImageToRunLengthFeaturesImageFilter_hxx
#define itkScalarImageToRunLengthFeaturesImageFilter_hxx

#include "itkScalarImageToRunLengthFeaturesImageFilter.h"
#include "itkNeighborhood.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkRegionOfInterestImageFilter.h"

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  ScalarImageToRunLengthFeaturesImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  for (int i = 0; i < 2; ++i)
  {
    this->ProcessObject::SetNthOutput(i, this->MakeOutput(i));
  }

  this->m_RunLengthMatrixGenerator = RunLengthMatrixFilterType::New();
  this->m_FeatureMeans = FeatureValueVector::New();
  this->m_FeatureStandardDeviations = FeatureValueVector::New();

  // Set the requested features to the default value:
  // {Energy, Entropy, InverseDifferenceMoment, Inertia, ClusterShade,
  // ClusterProminence}
  FeatureNameVectorPointer requestedFeatures = FeatureNameVector::New();
  // can't directly set this->m_RequestedFeatures since it is const!

  requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LongRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::GreyLevelNonuniformity);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::RunLengthNonuniformity);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LowGreyLevelRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::HighGreyLevelRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunLowGreyLevelEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunHighGreyLevelEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LongRunLowGreyLevelEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LongRunHighGreyLevelEmphasis);

  this->SetRequestedFeatures(requestedFeatures);

  // Set the offset directions to their defaults: half of all the possible
  // directions 1 pixel away. (The other half is included by symmetry.)
  // We use a neighborhood iterator to calculate the appropriate offsets.
  typedef Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;
  NeighborhoodType                                                                         hood;
  hood.SetRadius(1);

  // select all "previous" neighbors that are face+edge+vertex
  // connected to the current pixel. do not include the center pixel.
  unsigned int        centerIndex = hood.GetCenterNeighborhoodIndex();
  OffsetVectorPointer offsets = OffsetVector::New();
  for (unsigned int d = 0; d < centerIndex; d++)
  {
    OffsetType offset = hood.GetOffset(d);
    offsets->push_back(offset);
  }
  this->SetOffsets(offsets);
  this->m_FastCalculations = false;
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::GenerateData(void)
{
  ////////////////////////////////////////////////////////////////
  typename TOutputImage::Pointer output = this->GetOutput();

  InputRegionType InputRegion;
  RegionIndexType InputRegionIndex;
  RegionSizeType  InputRegionSize;

  InputRegionType                              OutputRegion;
  RegionIndexType                              OutputRegionIndex;
  const typename InputImageType::SpacingType & Spacing = this->ProcessObject::GetOutput(0)->GetOutput()->GetSpacing();
  const typename InputImageType::PointType & InputOrigin = this->ProcessObject::GetOutput(0)->GetOutput()->GetOrigin();
  double                                     OutputOrigin[m_NeighborhoodRadius.Dimension];

  for (unsigned int i = 0; i < m_NeighborhoodRadius.Dimension; i++)
  {
    InputRegionIndex[i] = m_NeighborhoodRadius[i];
    InputRegionSize[i] = this->ProcessObject::GetOutput(0).Size[i] - 2 * m_NeighborhoodRadius[i];

    OutputRegionIndex[i] = 0;
    OutputOrigin[i] = InputOrigin[i] + Spacing[i] * InputRegionIndex[i];
  }

  InputRegion.SetIndex(InputRegionIndex);
  InputRegion.SetSize(InputRegionSize);

  OutputRegion.SetIndex(OutputRegionIndex);
  OutputRegion.SetSize(InputRegionSize);

  output->SetRegions(OutputRegion);
  output->SetSpacing(Spacing);
  output->SetOrigin(OutputOrigin);
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  typedef itk::ImageRegionConstIteratorWithIndex<InputImageType> ConstIteratorType;
  ConstIteratorType                                         InputIt(this->ProcessObject::GetOutput(0), InputRegion);
  typedef itk::ImageRegionIteratorWithIndex<InputImageType> IteratorType;
  IteratorType                                              OutputIt(output, OutputRegion);

  while (!InputIt.IsAtEnd())
  {
    typename InputImageType::IndexType InputIndex = InputIt.GetIndex();
    InputIndex.Dimension;

    typename InputImageType::IndexType start;
    typename InputImageType::IndexType end;

    for (unsigned int i = 0; i < InputIndex.Dimension; i++)
    {
      start[i] = InputIndex[i] - m_NeighborhoodRadius[i];
      end[i] = InputIndex[i] + m_NeighborhoodRadius[i];
    }

    typename InputImageType::RegionType ExtractedRegion;
    ExtractedRegion.SetIndex(start);
    ExtractedRegion.SetUpperIndex(end);

    typedef typename itk::RegionOfInterestImageFilter<InputImageType, InputImageType> ExtractionFilterType;
    typename ExtractionFilterType::Pointer ExtractionFilter = ExtractionFilterType::New();
    ExtractionFilter->SetInput(this->ProcessObject::GetOutput(0));
    ExtractionFilter->SetRegionOfInterest(ExtractedRegion);

    // Compute the feature for the first offset
    typename OffsetVector::ConstIterator offsetIt = this->m_Offsets->Begin();
    this->m_RunLengthMatrixGenerator->SetOffset(offsetIt.Value());
    this->m_RunLengthMatrixGenerator->SetInput(ExtractionFilter->GetOutput());
    this->m_RunLengthMatrixGenerator->Update();
    typename RunLengthFeaturesFilterType::Pointer runLengthMatrixCalculator = RunLengthFeaturesFilterType::New();
    runLengthMatrixCalculator->SetInput(this->m_RunLengthMatrixGenerator->GetOutput());
    runLengthMatrixCalculator->Update();

    OutputIt.Set(runLengthMatrixCalculator.GetGreyLevelNonuniformityOutput());
    ++InputIt;
    ++OutputIt;
  }

  ////////////////////////////////////////////////////////////////
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetInput(
  const InputImageType * image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, const_cast<InputImageType *>(image));

  this->m_RunLengthMatrixGenerator->SetInput(image);
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  SetNumberOfBinsPerAxis(unsigned int numberOfBins)
{
  itkDebugMacro("setting NumberOfBinsPerAxis to " << numberOfBins);
  this->m_RunLengthMatrixGenerator->SetNumberOfBinsPerAxis(numberOfBins);
  this->Modified();
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetPixelValueMinMax(
  PixelType min,
  PixelType max)
{
  itkDebugMacro("setting Min to " << min << "and Max to " << max);
  this->m_RunLengthMatrixGenerator->SetPixelValueMinMax(min, max);
  this->Modified();
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  SetDistanceValueMinMax(double min, double max)
{
  itkDebugMacro("setting Min to " << min << "and Max to " << max);
  this->m_RunLengthMatrixGenerator->SetDistanceValueMinMax(min, max);
  this->Modified();
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetMaskImage(
  const InputImageType * image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, const_cast<InputImageType *>(image));

  this->m_RunLengthMatrixGenerator->SetMaskImage(image);
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
const TInputImage *
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::GetInput() const
{
  if (this->GetNumberOfInputs() < 1)
  {
    return ITK_NULLPTR;
  }
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(0));
}


template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
const TInputImage *
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::GetMaskImage() const
{
  if (this->GetNumberOfInputs() < 2)
  {
    return ITK_NULLPTR;
  }
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetInsidePixelValue(
  PixelType insidePixelValue)
{
  itkDebugMacro("setting InsidePixelValue to " << insidePixelValue);
  this->m_RunLengthMatrixGenerator->SetInsidePixelValue(insidePixelValue);
  this->Modified();
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "RequestedFeatures: " << this->GetRequestedFeatures() << std::endl;
  os << indent << "FeatureStandardDeviations: " << this->GetFeatureStandardDeviations() << std::endl;
  os << indent << "FastCalculations: " << this->GetFastCalculations() << std::endl;
  os << indent << "Offsets: " << this->GetOffsets() << std::endl;
  os << indent << "FeatureMeans: " << this->GetFeatureMeans() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
