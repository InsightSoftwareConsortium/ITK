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

  this->m_Normalize = false;
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::SetOffset(const OffsetType offset)
{
  OffsetVectorPointer offsetVector = OffsetVector::New();
  offsetVector->push_back(offset);
  this->SetOffsets(offsetVector);
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  InputImageType * maskPointer = const_cast<TInputImage *>(this->GetMaskImage());
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
  OutputImageType * outputPtr = this->GetOutput();
  if (strcmp(outputPtr->GetNameOfClass(), "VectorImage") == 0)
  {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength(outputPtr, 8);
  }
  outputPtr->Allocate();
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputRegionType & outputRegionForThread,
  ThreadIdType             threadId)
{
  // Recuperation of the different inputs/outputs
  OutputImageType * outputPtr = this->GetOutput();

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // Creation of the output pixel type
  typename TOutputImage::PixelType outputPixel;

  // Separation of the non-boundery region that will be processed in a different way
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>                           boundaryFacesCalculator;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList =
    boundaryFacesCalculator(this->m_DigitalisedInputImageg, outputRegionForThread, m_NeighborhoodRadius);
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit =
    faceList.begin();

  // Declaration of the variables usefull to iterate over the all image region
  bool      isInImage;
  IndexType firstIndex;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; i++)
  {
    firstIndex[i] = 0;
  }
  outputPixel = outputPtr->GetPixel(firstIndex);
  typename OffsetVector::ConstIterator offsets;

  // Declaration of the variables usefull to iterate over the all the offsets
  OffsetType   offset;
  unsigned int totalNumberOfFreq;


  vnl_matrix<unsigned int> hist(m_NumberOfBinsPerAxis, m_NumberOfBinsPerAxis);

  // Declaration of the variables usefull to iterate over the all neighborhood region
  PixelType curentInNeighborhoodPixelIntensity;

  // Declaration of the variables usefull to iterate over the run
  PixelType  pixelIntensity(NumericTraits<PixelType>::ZeroValue());
  OffsetType tempOffset;

  /// ***** Non-boundary Region *****
  for (; fit != faceList.end(); ++fit)
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
      totalNumberOfFreq = 0;
      // Iteration over all the offsets
      for (offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); ++offsets)
      {
        offset = offsets.Value();
        // Iteration over the all neighborhood region
        for (NeighborIndexType nb = 0; nb < inputNIt.Size(); ++nb)
        {
          // Test if the curent voxel is in the mask and is the range of the image intensity sepcified
          curentInNeighborhoodPixelIntensity = inputNIt.GetPixel(nb);
          if (curentInNeighborhoodPixelIntensity < this->m_Min)
          {
            continue;
          }

          // Test if the curent offset is still pointing to a voxel inside th neighborhood
          tempOffset = inputNIt.GetOffset(nb) + offset;
          if (!(this->IsInsideNeighborhood(tempOffset)))
          {
            continue;
          }

          // Test if the part of the neighborhood pointed by the offset is still part of the image
          if (fit == faceList.begin())
          {
            inputNIt.GetPixel(tempOffset, isInImage);
            if (!isInImage)
            {
              break;
            }
          }

          // Test if the pointed voxel is in the mask and is the range of the image intensity sepcified
          pixelIntensity = inputNIt.GetPixel(tempOffset);
          if (pixelIntensity < this->m_Min)
          {
            continue;
          }

          // Increase the coresponding bin in the histogram
          totalNumberOfFreq++;
          hist[curentInNeighborhoodPixelIntensity][pixelIntensity]++;
        }
      }
      // Compute the run lenght features
      this->ComputeFeatures(hist, totalNumberOfFreq, outputPixel);
      outputIt.Set(outputPixel);

      progress.CompletedPixel();
      ++inputNIt;
      ++outputIt;
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::UpdateOutputInformation()
{
  // Call superclass's version
  Superclass::UpdateOutputInformation();

  if (strcmp(this->GetOutput()->GetNameOfClass(), "VectorImage") == 0)
  {
    typedef typename TOutputImage::AccessorFunctorType AccessorFunctorType;
    AccessorFunctorType::SetVectorLength(this->GetOutput(), 8);
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
bool
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::IsInsideNeighborhood(
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
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::ComputeFeatures(
  vnl_matrix<unsigned int> &         hist,
  const unsigned int &               totalNumberOfFreq,
  typename TOutputImage::PixelType & outputPixel)
{
  // Now get the various means and variances. This is takes two passes
  // through the histogram.
  double pixelMean;
  double marginalMean;
  double marginalDevSquared;
  double pixelVariance;

  this->ComputeMeansAndVariances(hist, totalNumberOfFreq, pixelMean, marginalMean, marginalDevSquared, pixelVariance);

  // Finally compute the texture features. Another one pass.
  MeasurementType energy = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType entropy = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType correlation = NumericTraits<MeasurementType>::ZeroValue();

  MeasurementType inverseDifferenceMoment = NumericTraits<MeasurementType>::ZeroValue();

  MeasurementType inertia = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType clusterShade = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType clusterProminence = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType haralickCorrelation = NumericTraits<MeasurementType>::ZeroValue();

  double pixelVarianceSquared = pixelVariance * pixelVariance;
  // Variance is only used in correlation. If variance is 0, then
  //   (index[0] - pixelMean) * (index[1] - pixelMean)
  // should be zero as well. In this case, set the variance to 1. in
  // order to avoid NaN correlation.
  if (Math::FloatAlmostEqual(pixelVarianceSquared, 0.0, 4, 2 * NumericTraits<double>::epsilon()))
  {
    pixelVarianceSquared = 1.;
  }
  const double log2 = std::log(2.0);

  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
    {
      float frequency = hist[a][b] / (float)totalNumberOfFreq;
      if (Math::AlmostEquals(frequency, NumericTraits<float>::ZeroValue()))
      {
        continue; // no use doing these calculations if we're just multiplying by
                  // zero.
      }

      energy += frequency * frequency;
      entropy -= (frequency > 0.0001) ? frequency * std::log(frequency) / log2 : 0;
      correlation += ((a - pixelMean) * (b - pixelMean) * frequency) / pixelVarianceSquared;
      inverseDifferenceMoment += frequency / (1.0 + (a - b) * (a - b));
      inertia += (a - b) * (a - b) * frequency;
      clusterShade += std::pow((a - pixelMean) + (b - pixelMean), 3) * frequency;
      clusterProminence += std::pow((a - pixelMean) + (b - pixelMean), 4) * frequency;
      haralickCorrelation += a * b * frequency;
    }
  }

  haralickCorrelation = (haralickCorrelation - marginalMean * marginalMean) / marginalDevSquared;

  outputPixel[0] = energy;
  outputPixel[1] = entropy;
  outputPixel[2] = correlation;
  outputPixel[3] = inverseDifferenceMoment;
  outputPixel[4] = inertia;
  outputPixel[5] = clusterShade;
  outputPixel[6] = clusterProminence;
  outputPixel[7] = haralickCorrelation;
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::ComputeMeansAndVariances(
  vnl_matrix<unsigned int> & hist,
  const unsigned int &       totalNumberOfFreq,
  double &                   pixelMean,
  double &                   marginalMean,
  double &                   marginalDevSquared,
  double &                   pixelVariance)
{
  // This function takes two passes through the histogram and two passes through
  // an array of the same length as a histogram axis. This could probably be
  // cleverly compressed to one pass, but it's not clear that that's necessary.

  // Initialize everything
  double * marginalSums = new double[m_NumberOfBinsPerAxis];

  for (double * ms_It = marginalSums; ms_It < marginalSums + m_NumberOfBinsPerAxis; ms_It++)
  {
    *ms_It = 0;
  }
  pixelMean = 0;

  // Ok, now do the first pass through the histogram to get the marginal sums
  // and compute the pixel mean
  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
    {
      int   k = hist[a][b];
      float frequency = hist[a][b] / (float)totalNumberOfFreq;
      pixelMean += a * frequency;
      marginalSums[a] += frequency;
    }
  }

  /*  Now get the mean and deviaton of the marginal sums.
      Compute incremental mean and SD, a la Knuth, "The  Art of Computer
      Programming, Volume 2: Seminumerical Algorithms",  section 4.2.2.
      Compute mean and standard deviation using the recurrence relation:
      M(1) = x(1), M(k) = M(k-1) + (x(k) - M(k-1) ) / k
      S(1) = 0, S(k) = S(k-1) + (x(k) - M(k-1)) * (x(k) - M(k))
      for 2 <= k <= n, then
      sigma = std::sqrt(S(n) / n) (or divide by n-1 for sample SD instead of
      population SD).
  */
  marginalMean = marginalSums[0];
  marginalDevSquared = 0;
  for (unsigned int arrayIndex = 1; arrayIndex < m_NumberOfBinsPerAxis; arrayIndex++)
  {
    int    k = arrayIndex + 1;
    double M_k_minus_1 = marginalMean;
    double S_k_minus_1 = marginalDevSquared;
    double x_k = marginalSums[arrayIndex];

    double M_k = M_k_minus_1 + (x_k - M_k_minus_1) / k;
    double S_k = S_k_minus_1 + (x_k - M_k_minus_1) * (x_k - M_k);

    marginalMean = M_k;
    marginalDevSquared = S_k;
  }
  marginalDevSquared = marginalDevSquared / m_NumberOfBinsPerAxis;

  // OK, now compute the pixel variances.
  pixelVariance = 0;
  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
    {
      float frequency = hist[a][b] / (float)totalNumberOfFreq;
      pixelVariance += (a - pixelMean) * (a - pixelMean) * (frequency);
    }
  }

  delete[] marginalSums;
}

template <typename TInputImage, typename TOutputImage>
void
ScalarImageToTextureFeaturesImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{

  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(DigitalisedInputImageg);

  os << indent << "NeighborhoodRadius"
     << static_cast<typename NumericTraits<NeighborhoodRadiusType>::PrintType>(m_NeighborhoodRadius) << std::endl;

  itkPrintSelfObjectMacro(Offsets);

  os << indent << "NumberOfBinsPerAxis" << m_NumberOfBinsPerAxis << std::endl;
  os << indent << "Min" << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Min) << std::endl;
  os << indent << "Max" << static_cast<typename NumericTraits<PixelType>::PrintType>(m_Max) << std::endl;
  os << indent << "InsidePixelValue" << static_cast<typename NumericTraits<PixelType>::PrintType>(m_InsidePixelValue)
     << std::endl;
  os << indent << "Spacing"
     << static_cast<typename NumericTraits<typename TInputImage::SpacingType>::PrintType>(m_Spacing) << std::endl;
  os << indent << "Normalize" << m_Normalize << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
