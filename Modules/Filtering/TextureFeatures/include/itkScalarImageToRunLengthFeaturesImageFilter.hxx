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
#include "itkImageFileWriter.h"
#include <stdio.h>

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  ScalarImageToRunLengthFeaturesImageFilter()
  : m_NumberOfBinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis))
  , m_Min(NumericTraits<PixelType>::NonpositiveMin())
  , m_Max(NumericTraits<PixelType>::max())
  , m_MinDistance(NumericTraits<RealType>::ZeroValue())
  , m_MaxDistance(NumericTraits<RealType>::max())
  , m_InsidePixelValue(NumericTraits<PixelType>::OneValue())
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  for (int i = 1; i < 2; ++i)
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
  NeighborhoodType Nhood;
  Nhood.SetRadius(2);

  const unsigned int measurementVectorSize = 2;

  this->m_NeighborhoodRadius = Nhood.GetRadius();

  this->m_LowerBound.SetSize(measurementVectorSize);
  this->m_UpperBound.SetSize(measurementVectorSize);

  this->m_LowerBound[0] = this->m_Min;
  this->m_LowerBound[1] = this->m_MinDistance;
  this->m_UpperBound[0] = this->m_Max;
  this->m_UpperBound[1] = this->m_MaxDistance;
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  GenerateOutputInformation()
{
  typename Superclass::OutputImagePointer     outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();

  if (!outputPtr || !inputPtr)
  {
    return;
  }

  // Copy Information without modification.
  outputPtr->CopyInformation(inputPtr);

  InputRegionType      region;
  InputRegionSizeType  size;
  InputRegionIndexType start;
  start.Fill(0);

  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; i++)
  {
    size[i] =
      (this->ImageToImageFilter<TInputImage, TOutputImage>::GetInput(0)->GetLargestPossibleRegion().GetSize(i)) -
      (2 * this->m_NeighborhoodRadius[i]);
  }

  region.SetSize(size);
  region.SetIndex(start);

  // Adjust output region
  outputPtr->SetLargestPossibleRegion(region);
}


template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  BeforeThreadedGenerateData()
{
  typename TOutputImage::Pointer Output = this->GetOutput();

  OutputRegionType                              OutputRegion;
  OutputRegionIndexType                         OutputRegionIndex;
  OutputRegionSizeType                          OutputRegionSize;
  const typename OutputImageType::SpacingType & Spacing = Output->GetSpacing();
  const typename OutputImageType::PointType &   InputOrigin = Output->GetOrigin();
  double                                        OutputOrigin[this->m_NeighborhoodRadius.Dimension];

  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; i++)
  {
    OutputRegionSize[i] =
      (this->ImageToImageFilter<TInputImage, TOutputImage>::GetInput(0)->GetLargestPossibleRegion().GetSize(i)) -
      (2 * this->m_NeighborhoodRadius[i]);
    OutputOrigin[i] = InputOrigin[i] + Spacing[i] * this->m_NeighborhoodRadius[i];
  }
  OutputRegionIndex.Fill(0);

  OutputRegion.SetIndex(OutputRegionIndex);
  OutputRegion.SetSize(OutputRegionSize);

  Output->SetSpacing(Spacing);
  Output->SetOrigin(OutputOrigin);
  Output->SetRegions(OutputRegion);
  Output->Allocate();
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  ThreadedGenerateData(const OutputRegionType & outputRegionForThread, ThreadIdType threadId)
{

  TInputImage *  inputPtr = const_cast<TInputImage *>(this->GetInput());
  TOutputImage * outputPtr = this->GetOutput();

  inputPtr->DisconnectPipeline();

  InputRegionType inputRegionForThread;
  inputRegionForThread.SetSize(outputRegionForThread.GetSize());

  InputRegionIndexType start;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; i++)
  {
    start[i] = this->m_NeighborhoodRadius[i] + outputRegionForThread.GetIndex()[i];
  }


  inputRegionForThread.SetIndex(start);

  typedef typename itk::RegionOfInterestImageFilter<InputImageType, InputImageType> ExtractionFilterType;
  typename ExtractionFilterType::Pointer ExtractionFilter = ExtractionFilterType::New();
  ExtractionFilter->SetInput(inputPtr);

  typedef itk::ImageRegionConstIteratorWithIndex<InputImageType> ConstIteratorType;
  ConstIteratorType                                              inputIt(inputPtr, inputRegionForThread);
  typedef itk::ImageRegionIteratorWithIndex<OutputImageType>     IteratorType;
  IteratorType                                                   outputIt(outputPtr, outputRegionForThread);

  typename RunLengthMatrixFilterType::Pointer runLengthMatrixGenerator = RunLengthMatrixFilterType::New();
  typename OffsetVector::ConstIterator        offsetIt = this->m_Offsets->Begin();
  runLengthMatrixGenerator->SetOffset(offsetIt.Value());
  runLengthMatrixGenerator->SetDistanceValueMinMax(m_MinDistance, m_MaxDistance);
  runLengthMatrixGenerator->SetNumberOfBinsPerAxis(m_NumberOfBinsPerAxis);
  runLengthMatrixGenerator->SetPixelValueMinMax(m_Min, m_Max);
  runLengthMatrixGenerator->SetInsidePixelValue(m_InsidePixelValue);


  typename RunLengthFeaturesFilterType::Pointer runLengthMatrixCalculator = RunLengthFeaturesFilterType::New();

  typedef typename RunLengthFeaturesFilterType::RunLengthFeatureName InternalRunLengthFeatureName;
  typename FeatureNameVector::ConstIterator                          fnameIt;
  fnameIt = this->m_RequestedFeatures->Begin();
  fnameIt++;
  fnameIt++;
  fnameIt++;
  while (!inputIt.IsAtEnd())
  {
    typename InputImageType::IndexType inputIndex = inputIt.GetIndex();


    if ((this->GetMaskImage() && this->GetMaskImage()->GetPixel(inputIndex) != this->m_InsidePixelValue))
    {
      outputIt.Set(0);
    }
    else
    {
      typename InputImageType::IndexType start;
      typename InputImageType::IndexType end;

      for (unsigned int i = 0; i < inputIndex.Dimension; i++)
      {
        start[i] = inputIndex[i] - m_NeighborhoodRadius[i];
        end[i] = inputIndex[i] + m_NeighborhoodRadius[i];
      }

      typename InputImageType::RegionType ExtractedRegion;
      ExtractedRegion.SetIndex(start);
      ExtractedRegion.SetUpperIndex(end);

      inputPtr->SetRequestedRegion(ExtractedRegion);
      ExtractionFilter->SetRegionOfInterest(ExtractedRegion);


      runLengthMatrixGenerator->SetInput(ExtractionFilter->GetOutput());

      runLengthMatrixCalculator->SetInput(runLengthMatrixGenerator->GetOutput());
      runLengthMatrixCalculator->UpdateLargestPossibleRegion();


      outputIt.Set(runLengthMatrixCalculator->GetFeature((InternalRunLengthFeatureName)fnameIt.Value()));

      outputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
    ++inputIt;
    ++outputIt;
  }
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  AfterThreadedGenerateData()
{}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetMaskImage(
  const InputImageType * image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, const_cast<InputImageType *>(image));
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
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetPixelValueMinMax(
  PixelType min,
  PixelType max)
{
  if (this->m_Min != min || this->m_Max != max)
  {
    itkDebugMacro("setting Min to " << min << "and Max to " << max);
    this->m_Min = min;
    this->m_Max = max;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  SetDistanceValueMinMax(RealType min, RealType max)
{
  if (Math::NotExactlyEquals(this->m_MinDistance, min) || Math::NotExactlyEquals(this->m_MaxDistance, max))
  {
    itkDebugMacro("setting MinDistance to " << min << "and MaxDistance to " << max);
    this->m_MinDistance = min;
    this->m_MaxDistance = max;
    this->Modified();
  }
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
