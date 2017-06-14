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
#include "itkRegionOfInterestImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage>
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::ScalarImageToRunLengthFeaturesImageFilter()
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
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::SetOffset(const OffsetType offset)
{
  OffsetVectorPointer offsetVector = OffsetVector::New();
  offsetVector->push_back(offset);
  this->SetOffsets(offsetVector);
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
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
    else if (inputIt.Get() < this->m_Min || inputIt.Get() >= this->m_Max)
    {
      digitIt.Set(this->m_Min - 1);
    }
    else
    {
      binNumber = (inputIt.Get() - m_Min) / ((m_Max - m_Min) / (float)m_NumberOfBinsPerAxis);
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
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputRegionType & outputRegionForThread,
  ThreadIdType             threadId)
{
  // Recuperation of the different inputs/outputs
  typename TOutputImage::Pointer outputPtr = TOutputImage::New();
  outputPtr = this->GetOutput();

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // Creation of the output pixel type
  typename TOutputImage::PixelType outputPixel;

  // Creation of a region with the same size than the neighborhood, this region
  // will be used to check if each voxel has already been visited
  InputRegionType                                  boolRegion;
  typename InputRegionType::IndexType              boolStart;
  typename InputRegionType::SizeType               boolSize;
  IndexType                                        boolCurentInNeighborhoodIndex;
  typedef Image<bool, TInputImage::ImageDimension> BoolImageType;
  typename BoolImageType::Pointer                  alreadyVisitedImage = BoolImageType::New();
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; i++)
  {
    boolSize[i] = this->m_NeighborhoodRadius[i] * 2 + 1;
    boolStart[i] = 0;
    boolCurentInNeighborhoodIndex[i] = m_NeighborhoodRadius[i];
  }
  boolRegion.SetIndex(boolStart);
  boolRegion.SetSize(boolSize);
  alreadyVisitedImage->CopyInformation(this->m_DigitalisedInputImageg);
  alreadyVisitedImage->SetRegions(boolRegion);
  alreadyVisitedImage->Allocate();

  // Separation of the non-boundery region that will be processed in a different way
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>                           boundaryFacesCalculator;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList =
    boundaryFacesCalculator(this->m_DigitalisedInputImageg, outputRegionForThread, m_NeighborhoodRadius);
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit =
    faceList.begin();

  // Declaration of the variables usefull to iterate over the all image region
  bool isInImage;
  outputPixel = outputPtr->GetPixel(boolCurentInNeighborhoodIndex);
  typename OffsetVector::ConstIterator offsets;

  // Declaration of the variables usefull to iterate over the all the offsets
  OffsetType      offset;
  unsigned int    totalNumberOfRuns;
  unsigned int ** hist = new unsigned int *[m_NumberOfBinsPerAxis];
  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    hist[a] = new unsigned int[m_NumberOfBinsPerAxis];
  }

  // Declaration of the variables usefull to iterate over the all neighborhood region
  PixelType curentInNeighborhoodPixelIntensity;

  // Declaration of the variables usefull to iterate over the run
  PixelType    pixelIntensity(NumericTraits<PixelType>::ZeroValue());
  OffsetType   iteratedOffset;
  OffsetType   tempOffset;
  unsigned int pixelDistance;
  bool         insideNeighborhood;

  /// ***** Non-boundary Region *****
  for (fit; fit != faceList.end(); ++fit)
  {
    NeighborhoodIteratorType inputNIt(m_NeighborhoodRadius, this->m_DigitalisedInputImageg, *fit);
    typedef itk::ImageRegionIterator<OutputImageType> IteratorType;
    IteratorType                                      outputIt(outputPtr, *fit);

    // Iteration over the all image region
    while (!inputNIt.IsAtEnd())
    {
      // If the voxel is outside of the mask, don't treat it
      if (inputNIt.GetCenterPixel() < (this->m_Min - 5)) // the pixel is outside of the mask
      {
        progress.CompletedPixel();
        ++inputNIt;
        ++outputIt;
        continue;
      }
      // Initialisation of the histogram
      for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
      {
        for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
        {
          hist[a][b] = 0;
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
          curentInNeighborhoodPixelIntensity = inputNIt.GetPixel(nb);
          tempOffset = inputNIt.GetOffset(nb);
          // Cecking if the value is out-of-bounds or is outside the mask.
          if (curentInNeighborhoodPixelIntensity <
                this->m_Min || // the pixel is outside of the mask or outside of bounds
              alreadyVisitedImage->GetPixel(boolCurentInNeighborhoodIndex + tempOffset))
          {
            continue;
          }
          // Initialisation of the variables usefull to iterate over the run
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
            if (pixelIntensity == curentInNeighborhoodPixelIntensity)
            {
              alreadyVisitedImage->SetPixel(boolCurentInNeighborhoodIndex + iteratedOffset, true);
              pixelDistance++;
              iteratedOffset += offset;
              insideNeighborhood = this->IsInsideNeighborhood(iteratedOffset);
            }
            else
            {
              break;
            }
          }
          // Increase the coresponding bin in the histogram
          this->IncreaseHistograme(hist, totalNumberOfRuns, curentInNeighborhoodPixelIntensity, offset, pixelDistance);
        }
      }
      // Compute the run lenght features
      this->ComputeFeatures(hist, totalNumberOfRuns, outputPixel);
      outputIt.Set(outputPixel);

      progress.CompletedPixel();
      ++inputNIt;
      ++outputIt;
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::UpdateOutputInformation()
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
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::SetMaskImage(const InputImageType * image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputImage>
const TInputImage *
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::GetMaskImage() const
{
  if (this->GetNumberOfInputs() < 2)
  {
    return ITK_NULLPTR;
  }
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::SetPixelValueMinMax(PixelType min, PixelType max)
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
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::SetDistanceValueMinMax(RealType min, RealType max)
{
  if (Math::NotExactlyEquals(this->m_MinDistance, min) || Math::NotExactlyEquals(this->m_MaxDistance, max))
  {
    this->m_MinDistance = min;
    this->m_MaxDistance = max;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::NormalizeOffsetDirection(OffsetType & offset)
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

template <typename TInputImage, typename TOutputImage>
bool
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::IsInsideNeighborhood(
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

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::IncreaseHistograme(
  unsigned int **      hist,
  unsigned int &       totalNumberOfRuns,
  const PixelType &    curentInNeighborhoodPixelIntensity,
  const OffsetType &   offset,
  const unsigned int & pixelDistance)
{
  float offsetDistance = 0;
  for (unsigned int i = 0; i < offset.GetOffsetDimension(); ++i)
  {
    offsetDistance += (offset[i] * m_Spacing[i]) * (offset[i] * m_Spacing[i]);
  }
  offsetDistance = std::sqrt(offsetDistance);

  int offsetDistanceBin = (int)((offsetDistance * pixelDistance - m_MinDistance) /
                                ((m_MaxDistance - m_MinDistance) / (float)m_NumberOfBinsPerAxis));

  if (offsetDistanceBin < m_NumberOfBinsPerAxis)
  {
    totalNumberOfRuns++;
    hist[curentInNeighborhoodPixelIntensity][offsetDistanceBin]++;
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::ComputeFeatures(
  unsigned int **                    hist,
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

  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
    {
      OutputRealType frequency = hist[a][b];
      if (Math::ExactlyEquals(frequency, NumericTraits<OutputRealType>::ZeroValue()))
      {
        continue;
      }

      double i2 = static_cast<double>((a + 1) * (a + 1));
      double j2 = static_cast<double>((b + 1) * (b + 1));

      // Traditional measures
      shortRunEmphasis += (frequency / j2);
      longRunEmphasis += (frequency * j2);

      greyLevelNonuniformityVector[a] += frequency;
      runLengthNonuniformityVector[b] += frequency;

      // measures from Chu et al.
      lowGreyLevelRunEmphasis += (frequency / i2);
      highGreyLevelRunEmphasis += (frequency * i2);

      // measures from Dasarathy and Holder
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

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(DigitalisedInputImageg);

  os << indent << "NeighborhoodRadius"
     << static_cast<typename NumericTraits<NeighborhoodRadiusType>::PrintType>(m_NeighborhoodRadius) << std::endl;

  itkPrintSelfObjectMacro(Offsets);

  os << indent << "NumberOfBinsPerAxis" << m_NumberOfBinsPerAxis << std::endl;
  os << indent << "Min" << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Min) << std::endl;
  os << indent << "Max" << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Max) << std::endl;
  os << indent << "MinDistance" << static_cast<typename NumericTraits<RealType>::PrintType>(m_MinDistance) << std::endl;
  os << indent << "MaxDistance" << static_cast<typename NumericTraits<RealType>::PrintType>(m_MaxDistance) << std::endl;
  os << indent << "InsidePixelValue" << static_cast<typename NumericTraits<PixelType>::PrintType>(m_InsidePixelValue)
     << std::endl;
  os << indent << "Spacing"
     << static_cast<typename NumericTraits<typename TInputImage::SpacingType>::PrintType>(m_Spacing) << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
