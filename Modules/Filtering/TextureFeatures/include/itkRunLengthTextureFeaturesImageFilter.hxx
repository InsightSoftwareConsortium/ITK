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
#ifndef itkRunLengthTextureFeaturesImageFilter_hxx
#define itkRunLengthTextureFeaturesImageFilter_hxx

#include "itkRunLengthTextureFeaturesImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkBinaryFunctorImageFilter.h"
#include "itkDigitizerFunctor.h"

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage, typename TMaskImage>
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::RunLengthTextureFeaturesImageFilter()
  : m_NumberOfBinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis))
  , m_HistogramValueMinimum(NumericTraits<PixelType>::NonpositiveMin())
  , m_HistogramValueMaximum(NumericTraits<PixelType>::max())
  , m_HistogramDistanceMinimum(NumericTraits<RealType>::ZeroValue())
  , m_HistogramDistanceMaximum(NumericTraits<RealType>::max())
  , m_InsidePixelValue(NumericTraits<MaskPixelType>::OneValue())
  , m_Spacing(1.0)
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  // Mark the "MaskImage" as an optional named input. First it has to
  // be added to the list of named inputs then removed from the
  // required list.
  Self::AddRequiredInputName("MaskImage");
  Self::RemoveRequiredInputName("MaskImage");

  // Set the offset directions to their defaults: half of all the possible
  // directions 1 pixel away. (The other half is included by symmetry.)
  // We use a neighborhood iterator to calculate the appropriate offsets.
  using NeighborhoodType = Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension>;
  NeighborhoodType hood;
  hood.SetRadius(1);

  // Select all "previous" neighbors that are face+edge+vertex
  // connected to the iterated pixel. Do not include the currentInNeighborhood pixel.
  unsigned int        centerIndex = hood.GetCenterNeighborhoodIndex();
  OffsetVectorPointer offsets = OffsetVector::New();
  for (unsigned int d = 0; d < centerIndex; ++d)
  {
    OffsetType offset = hood.GetOffset(d);
    offsets->push_back(offset);
  }
  this->SetOffsets(offsets);
  NeighborhoodType nhood;
  nhood.SetRadius(2);
  this->m_NeighborhoodRadius = nhood.GetRadius();
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::SetOffset(const OffsetType offset)
{
  OffsetVectorPointer offsetVector = OffsetVector::New();
  offsetVector->push_back(offset);
  this->SetOffsets(offsetVector);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::BeforeThreadedGenerateData()
{

  typename TInputImage::Pointer input = InputImageType::New();
  input->Graft(const_cast<TInputImage *>(this->GetInput()));

  using DigitizerFunctorType = Digitizer<PixelType, PixelType, typename DigitizedImageType::PixelType>;

  DigitizerFunctorType digitalizer(
    m_NumberOfBinsPerAxis, m_InsidePixelValue, m_HistogramValueMinimum, m_HistogramValueMaximum);

  using FilterType = BinaryFunctorImageFilter<MaskImageType, InputImageType, DigitizedImageType, DigitizerFunctorType>;
  typename FilterType::Pointer filter = FilterType::New();
  if (this->GetMaskImage() != nullptr)
  {
    typename TMaskImage::Pointer mask = MaskImageType::New();
    mask->Graft(const_cast<TMaskImage *>(this->GetMaskImage()));
    filter->SetInput1(mask);
  }
  else
  {
    filter->SetConstant1(m_InsidePixelValue);
  }
  filter->SetInput2(input);
  filter->SetFunctor(digitalizer);
  filter->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());

  filter->Update();
  m_DigitizedInputImage = filter->GetOutput();

  m_Spacing = this->GetInput()->GetSpacing();
}


template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::AfterThreadedGenerateData()
{
  // free internal image
  this->m_DigitizedInputImage = nullptr;
}


template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::DynamicThreadedGenerateData(
  const OutputRegionType & outputRegionForThread)
{
  // Get the inputs/outputs
  TOutputImage * outputPtr = this->GetOutput();

  // Creation of the output pixel type
  typename TOutputImage::PixelType outputPixel;
  NumericTraits<typename TOutputImage::PixelType>::SetLength(outputPixel, outputPtr->GetNumberOfComponentsPerPixel());

  // Creation of a region with the same size as the neighborhood. This region
  // will be used to check if each voxel has already been visited.
  InputRegionType                     boolRegion;
  typename InputRegionType::IndexType boolStart;
  typename InputRegionType::SizeType  boolSize;
  IndexType                           boolCurentInNeighborhoodIndex;
  using BoolImageType = Image<bool, TInputImage::ImageDimension>;
  typename BoolImageType::Pointer alreadyVisitedImage = BoolImageType::New();
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
  {
    boolSize[i] = this->m_NeighborhoodRadius[i] * 2 + 1;
    boolStart[i] = 0;
    boolCurentInNeighborhoodIndex[i] = m_NeighborhoodRadius[i];
  }
  boolRegion.SetIndex(boolStart);
  boolRegion.SetSize(boolSize);
  alreadyVisitedImage->CopyInformation(this->m_DigitizedInputImage);
  alreadyVisitedImage->SetRegions(boolRegion);
  alreadyVisitedImage->Allocate();

  // Separation of the non-boundary region that will be processed in a different way
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<DigitizedImageType> boundaryFacesCalculator;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<DigitizedImageType>::FaceListType faceList =
    boundaryFacesCalculator(this->m_DigitizedInputImage, outputRegionForThread, m_NeighborhoodRadius);
  auto fit = faceList.begin();

  // Declaration of the variables useful to iterate over the all image region
  bool                                 isInImage;
  typename OffsetVector::ConstIterator offsets;

  // Declaration of the variables useful to iterate over the all the offsets
  OffsetType   offset;
  unsigned int totalNumberOfRuns;

  vnl_matrix<unsigned int> histogram(m_NumberOfBinsPerAxis, m_NumberOfBinsPerAxis);


  // Declaration of the variables useful to iterate over the all neighborhood region
  HistogramIndexType currentInNeighborhoodPixelIntensity;

  // Declaration of the variables useful to iterate over the run
  HistogramIndexType pixelIntensity(NumericTraits<HistogramIndexType>::ZeroValue());
  OffsetType         iteratedOffset;
  OffsetType         tempOffset;
  unsigned int       pixelDistance;
  bool               insideNeighborhood;

  /// ***** Non-boundary Region *****
  for (; fit != faceList.end(); ++fit)
  {
    NeighborhoodIteratorType inputNIt(m_NeighborhoodRadius, this->m_DigitizedInputImage, *fit);
    using IteratorType = itk::ImageRegionIterator<OutputImageType>;
    IteratorType outputIt(outputPtr, *fit);

    // Iteration over the all image region
    while (!inputNIt.IsAtEnd())
    {
      // If the voxel is outside of the mask, don't treat it
      if (inputNIt.GetCenterPixel() < (-5)) // the pixel is outside of the mask
      {
        outputPixel.Fill(0);
        outputIt.Set(outputPixel);
        ++inputNIt;
        ++outputIt;
        continue;
      }
      // Initialisation of the histogram
      for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; ++a)
      {
        for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; ++b)
        {
          histogram[a][b] = 0;
        }
      }
      totalNumberOfRuns = 0;
      // Iteration over all the offsets
      for (offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); ++offsets)
      {
        alreadyVisitedImage->FillBuffer(false);
        offset = offsets.Value();
        this->NormalizeOffsetDirection(offset);
        // Iteration over the all neighborhood region
        for (NeighborIndexType nb = 0; nb < inputNIt.Size(); ++nb)
        {
          currentInNeighborhoodPixelIntensity = inputNIt.GetPixel(nb);
          tempOffset = inputNIt.GetOffset(nb);
          // Checking if the value is out-of-bounds or is outside the mask.
          if (currentInNeighborhoodPixelIntensity < 0 || // The pixel is outside of the mask or outside of bounds
              alreadyVisitedImage->GetPixel(boolCurentInNeighborhoodIndex + tempOffset))
          {
            continue;
          }
          // Initialisation of the variables useful to iterate over the run
          iteratedOffset = tempOffset + offset;
          pixelDistance = 0;
          insideNeighborhood = this->IsInsideNeighborhood(iteratedOffset);
          // Scan from the iterated pixel at index, following the direction of
          // offset. Run length is computed as the length of continuous pixel
          // whose pixel values are in the same bin.
          while (insideNeighborhood)
          {
            // If the voxel reached is outside of the image, stop the iterations
            if (fit == faceList.begin())
            {
              inputNIt.GetPixel(iteratedOffset, isInImage);
              if (!isInImage)
              {
                break;
              }
            }
            pixelIntensity = inputNIt.GetPixel(iteratedOffset);
            // Special attention paid to boundaries of bins.
            // For the last bin, it is left close and right close (following the previous
            // gerrit patch). For all other bins, the bin is left close and right open.
            if (pixelIntensity == currentInNeighborhoodPixelIntensity)
            {
              alreadyVisitedImage->SetPixel(boolCurentInNeighborhoodIndex + iteratedOffset, true);
              ++pixelDistance;
              iteratedOffset += offset;
              insideNeighborhood = this->IsInsideNeighborhood(iteratedOffset);
            }
            else
            {
              break;
            }
          }
          // Increase the corresponding bin in the histogram

          this->IncreaseHistogram(
            histogram, totalNumberOfRuns, currentInNeighborhoodPixelIntensity, offset, pixelDistance);
        }
      }
      // Compute the run length features
      this->ComputeFeatures(histogram, totalNumberOfRuns, outputPixel);
      outputIt.Set(outputPixel);

      ++inputNIt;
      ++outputIt;
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  OutputImageType * output = this->GetOutput();
  // If the output image type is a VectorImage the number of
  // components will be properly sized if before allocation, if the
  // output is a fixed width vector and the wrong number of
  // components, then an exception will be thrown.
  if (output->GetNumberOfComponentsPerPixel() != 10)
  {
    output->SetNumberOfComponentsPerPixel(10);
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::NormalizeOffsetDirection(
  OffsetType & offset)
{
  itkDebugMacro("old offset = " << offset << std::endl);
  int  sign = 1;
  bool metLastNonZero = false;
  for (int i = offset.GetOffsetDimension() - 1; i >= 0; i--)
  {
    if (metLastNonZero)
    {
      offset[i] *= sign;
    }
    else if (offset[i] != 0)
    {
      sign = (offset[i] > 0) ? 1 : -1;
      metLastNonZero = true;
      offset[i] *= sign;
    }
  }
  itkDebugMacro("new  offset = " << offset << std::endl);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
bool
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::IsInsideNeighborhood(
  const OffsetType & iteratedOffset)
{
  bool insideNeighborhood = true;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
  {
    int boundDistance = m_NeighborhoodRadius[i] - Math::abs(iteratedOffset[i]);
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
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::IncreaseHistogram(
  vnl_matrix<unsigned int> & histogram,
  unsigned int &             totalNumberOfRuns,
  const HistogramIndexType & currentInNeighborhoodPixelIntensity,
  const OffsetType &         offset,
  const unsigned int &       pixelDistance)
{
  float offsetDistance = 0;
  for (unsigned int i = 0; i < offset.GetOffsetDimension(); ++i)
  {
    offsetDistance += (offset[i] * m_Spacing[i]) * (offset[i] * m_Spacing[i]);
  }
  offsetDistance = std::sqrt(offsetDistance);
  auto offsetDistanceBin =
    static_cast<int>((offsetDistance * pixelDistance - m_HistogramDistanceMinimum) /
                     ((m_HistogramDistanceMaximum - m_HistogramDistanceMinimum) / (float)m_NumberOfBinsPerAxis));
  if (offsetDistanceBin < static_cast<int>(m_NumberOfBinsPerAxis) && offsetDistanceBin >= 0)
  {
    ++totalNumberOfRuns;
    ++histogram[currentInNeighborhoodPixelIntensity][offsetDistanceBin];
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::ComputeFeatures(
  vnl_matrix<unsigned int> &         histogram,
  const unsigned int &               totalNumberOfRuns,
  typename TOutputImage::PixelType & outputPixel)
{
  OutputRealType shortRunEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType longRunEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType greyLevelNonuniformity = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType runLengthNonuniformity = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType lowGreyLevelRunEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType highGreyLevelRunEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType shortRunLowGreyLevelEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType shortRunHighGreyLevelEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType longRunLowGreyLevelEmphasis = NumericTraits<OutputRealType>::ZeroValue();
  OutputRealType longRunHighGreyLevelEmphasis = NumericTraits<OutputRealType>::ZeroValue();

  vnl_vector<double> greyLevelNonuniformityVector(m_NumberOfBinsPerAxis, 0.0);
  vnl_vector<double> runLengthNonuniformityVector(m_NumberOfBinsPerAxis, 0.0);

  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; ++a)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; ++b)
    {
      OutputRealType frequency = histogram[a][b];
      if (Math::ExactlyEquals(frequency, NumericTraits<OutputRealType>::ZeroValue()))
      {
        continue;
      }

      auto i2 = static_cast<double>((a + 1) * (a + 1));
      auto j2 = static_cast<double>((b + 1) * (b + 1));

      // Traditional measures
      shortRunEmphasis += (frequency / j2);
      longRunEmphasis += (frequency * j2);

      greyLevelNonuniformityVector[a] += frequency;
      runLengthNonuniformityVector[b] += frequency;

      // Measures from Chu et al.
      lowGreyLevelRunEmphasis += (frequency / i2);
      highGreyLevelRunEmphasis += (frequency * i2);

      // Measures from Dasarathy and Holder
      shortRunLowGreyLevelEmphasis += (frequency / (i2 * j2));
      shortRunHighGreyLevelEmphasis += (frequency * i2 / j2);
      longRunLowGreyLevelEmphasis += (frequency * j2 / i2);
      longRunHighGreyLevelEmphasis += (frequency * i2 * j2);
    }
  }
  greyLevelNonuniformity = greyLevelNonuniformityVector.squared_magnitude();
  runLengthNonuniformity = runLengthNonuniformityVector.squared_magnitude();

  // Normalize all measures by the total number of runs

  shortRunEmphasis /= static_cast<double>(totalNumberOfRuns);
  longRunEmphasis /= static_cast<double>(totalNumberOfRuns);
  greyLevelNonuniformity /= static_cast<double>(totalNumberOfRuns);
  runLengthNonuniformity /= static_cast<double>(totalNumberOfRuns);

  lowGreyLevelRunEmphasis /= static_cast<double>(totalNumberOfRuns);
  highGreyLevelRunEmphasis /= static_cast<double>(totalNumberOfRuns);

  shortRunLowGreyLevelEmphasis /= static_cast<double>(totalNumberOfRuns);
  shortRunHighGreyLevelEmphasis /= static_cast<double>(totalNumberOfRuns);
  longRunLowGreyLevelEmphasis /= static_cast<double>(totalNumberOfRuns);
  longRunHighGreyLevelEmphasis /= static_cast<double>(totalNumberOfRuns);

  outputPixel[0] = shortRunEmphasis;
  outputPixel[1] = longRunEmphasis;
  outputPixel[2] = greyLevelNonuniformity;
  outputPixel[3] = runLengthNonuniformity;
  outputPixel[4] = lowGreyLevelRunEmphasis;
  outputPixel[5] = highGreyLevelRunEmphasis;
  outputPixel[6] = shortRunLowGreyLevelEmphasis;
  outputPixel[7] = shortRunHighGreyLevelEmphasis;
  outputPixel[8] = longRunLowGreyLevelEmphasis;
  outputPixel[9] = longRunHighGreyLevelEmphasis;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
RunLengthTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os,
                                                                                      Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(DigitizedInputImage);

  os << indent << "NeighborhoodRadius: "
     << static_cast<typename NumericTraits<NeighborhoodRadiusType>::PrintType>(m_NeighborhoodRadius) << std::endl;

  itkPrintSelfObjectMacro(Offsets);

  os << indent << "NumberOfBinsPerAxis: " << m_NumberOfBinsPerAxis << std::endl;
  os << indent << "Min: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_HistogramValueMinimum)
     << std::endl;
  os << indent << "Max: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_HistogramValueMaximum)
     << std::endl;
  os << indent
     << "MinDistance: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_HistogramDistanceMinimum)
     << std::endl;
  os << indent
     << "MaxDistance: " << static_cast<typename NumericTraits<RealType>::PrintType>(m_HistogramDistanceMaximum)
     << std::endl;
  os << indent << "InsidePixelValue: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_InsidePixelValue)
     << std::endl;
  os << indent
     << "Spacing: " << static_cast<typename NumericTraits<typename TInputImage::SpacingType>::PrintType>(m_Spacing)
     << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
