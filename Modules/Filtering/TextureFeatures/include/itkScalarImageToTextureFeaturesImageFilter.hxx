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
#ifndef itkScalarImageToTextureFeaturesImageFilter_hxx
#define itkScalarImageToTextureFeaturesImageFilter_hxx

#include "itkScalarImageToTextureFeaturesImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage>
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::ScalarImageToTextureFeaturesImageFilter()
  : m_NumberOfBinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis))
  , m_Min(NumericTraits<PixelType>::NonpositiveMin())
  , m_Max(NumericTraits<PixelType>::max())
  , m_MinDistance(NumericTraits<RealType>::ZeroValue())
  , m_MaxDistance(NumericTraits<RealType>::max())
  , m_InsidePixelValue(NumericTraits<PixelType>::OneValue())
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  // Set the offset directions to their defaults: half of all the possible
  // directions 1 pixel away. (The other half is included by symmetry.)
  // We use a neighborhood iterator to calculate the appropriate offsets.
  typedef Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;
  NeighborhoodType                                                                         hood;
  hood.SetRadius(1);

  // select all "previous" neighbors that are face+edge+vertex
  // connected to the iterated pixel. do not include the curentInNeighborhood pixel.
  unsigned int        centerIndex = hood.GetCenterNeighborhoodIndex();
  OffsetVectorPointer offsets = OffsetVector::New();
  for (unsigned int d = 0; d < centerIndex; d++)
  {
    OffsetType offset = hood.GetOffset(d);
    offsets->push_back(offset);
  }
  this->SetOffsets(offsets);
  NeighborhoodType nhood;
  nhood.SetRadius(2);
  this->m_NeighborhoodRadius = nhood.GetRadius();

  TOutputImage *                   outputPtr = this->GetOutput();
  typename TOutputImage::PixelType pixelNull;
  pixelNull.Fill(0);
  outputPtr->FillBuffer(pixelNull);
}


template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  typename TInputImage::Pointer maskPointer = TInputImage::New();
  maskPointer = const_cast<TInputImage *>(this->GetMaskImage());
  this->m_DigitalisedInputImageg = InputImageType::New();
  this->m_DigitalisedInputImageg->SetRegions(this->GetInput()->GetRequestedRegion());
  this->m_DigitalisedInputImageg->CopyInformation(this->GetInput());
  this->m_DigitalisedInputImageg->Allocate();
  typedef itk::ImageRegionIterator<InputImageType> IteratorType;
  IteratorType digitIt(this->m_DigitalisedInputImageg, this->m_DigitalisedInputImageg->GetLargestPossibleRegion());
  typedef itk::ImageRegionConstIterator<InputImageType> ConstIteratorType;
  ConstIteratorType inputIt(this->GetInput(), this->GetInput()->GetLargestPossibleRegion());
  unsigned int      binNumber;
  while (!inputIt.IsAtEnd())
  {
    if (maskPointer && maskPointer->GetPixel(inputIt.GetIndex()) != this->m_InsidePixelValue)
    {
      digitIt.Set(this->m_Min - 10);
    }
    else if (inputIt.Get() < this->m_Min || inputIt.Get() > this->m_Max)
    {
      digitIt.Set(this->m_Min - 1);
    }
    else
    {
      binNumber = (inputIt.Get() - m_Min) / ((m_Max - m_Min) / m_NumberOfBinsPerAxis);
      digitIt.Set(binNumber);
    }
    ++inputIt;
    ++digitIt;
  }
  m_Spacing = this->GetInput()->GetSpacing();

  // Support VectorImages by setting number of components on output.
  typename TOutputImage::Pointer outputPtr = TOutputImage::New();
  outputPtr = this->GetOutput();
  if (strcmp(outputPtr->GetNameOfClass(), "VectorImage") == 0)
  {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength(outputPtr, 10);
  }
  outputPtr->Allocate();
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputRegionType & outputRegionForThread,
  ThreadIdType             threadId)
{}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::UpdateOutputInformation()
{
  // Call superclass's version
  Superclass::UpdateOutputInformation();

  if (strcmp(this->GetOutput()->GetNameOfClass(), "VectorImage") == 0)
  {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength(this->GetOutput(), 10);
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::SetMaskImage(const InputImageType * image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputImage>
const TInputImage *
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::GetMaskImage() const
{
  if (this->GetNumberOfInputs() < 2)
  {
    return ITK_NULLPTR;
  }
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::SetPixelValueMinMax(PixelType min, PixelType max)
{
  if (this->m_Min != min || this->m_Max != max)
  {
    this->m_Min = min;
    this->m_Max = max;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::SetDistanceValueMinMax(RealType min, RealType max)
{
  if (Math::NotExactlyEquals(this->m_MinDistance, min) || Math::NotExactlyEquals(this->m_MaxDistance, max))
  {
    this->m_MinDistance = min;
    this->m_MaxDistance = max;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
bool
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::IsInsideNeighborhood(
  const OffsetType & iteratedOffset)
{
  bool insideNeighborhood = true;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
  {
    int boundDistance = m_NeighborhoodRadius[i] - std::abs(iteratedOffset[i]);
    if (boundDistance < 0)
    {
      insideNeighborhood = false;
      break;
    }
  }
  return insideNeighborhood;
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::IncreaseHistograme(
  unsigned int **      hist,
  unsigned int &       totalNumberOfRuns,
  const PixelType &    curentInNeighborhoodPixelIntensity,
  const OffsetType &   offset,
  const unsigned int & pixelDistance)
{}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::ComputeFeatures(
  unsigned int **                    hist,
  const unsigned int &               totalNumberOfRuns,
  typename TOutputImage::PixelType & outputPixel)
{}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{}
} // end of namespace Statistics
} // end of namespace itk

#endif
