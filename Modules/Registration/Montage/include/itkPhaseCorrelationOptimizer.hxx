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
#ifndef itkPhaseCorrelationOptimizer_hxx
#define itkPhaseCorrelationOptimizer_hxx


#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCompensatedSummation.h"

#include <cmath>
#include <type_traits>

//#ifndef NDEBUG
#include "itkImageFileWriter.h"

namespace
{
template <typename TImage>
void
WriteDebug(const TImage * out, const char * filename)
{
  using WriterType = itk::ImageFileWriter<TImage>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(out);
  w->SetFileName(filename);
  try
  {
    w->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << error << std::endl;
  }
}
} // namespace
//#else
// namespace
//{
// template <typename TImage>
// void
// WriteDebug(TImage *, const char *)
//{}
//} // namespace
//#endif

namespace itk
{

template <typename TRealPixelType, unsigned int VImageDimension>
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::PhaseCorrelationOptimizer()
{
  this->SetNumberOfRequiredInputs(3);
  this->SetOffsetCount(4);

  this->m_AdjustedInput = ImageType::New();

  this->m_PadFilter->SetSizeGreatestPrimeFactor(this->m_FFTFilter->GetSizeGreatestPrimeFactor());
  this->m_CyclicShiftFilter->SetInput(this->m_PadFilter->GetOutput());
  this->m_FFTFilter->SetInput(this->m_CyclicShiftFilter->GetOutput());
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::SetOffsetCount(unsigned count)
{
  if (m_Offsets.size() != count)
  {
    this->SetNumberOfRequiredOutputs(count);
    for (unsigned i = m_Offsets.size(); i < count; i++)
    {
      OffsetOutputPointer offsetDecorator = static_cast<OffsetOutputType *>(this->MakeOutput(i).GetPointer());
      this->ProcessObject::SetNthOutput(i, offsetDecorator.GetPointer());
    }
    m_Offsets.resize(count);

    this->Modified();
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Offsets:";
  for (unsigned i = 0; i < m_Offsets.size(); i++)
  {
    os << " " << m_Offsets[i];
  }
  os << indent << "PeakInterpolationMethod: " << m_PeakInterpolationMethod << std::endl;
  os << indent << "MaxCalculator: " << m_MaxCalculator << std::endl;
  os << indent << "MergePeaks: " << m_MergePeaks << std::endl;
  os << indent << "ZeroSuppression: " << m_ZeroSuppression << std::endl;
  os << indent << "PixelDistanceTolerance: " << m_PixelDistanceTolerance << std::endl;
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::SetPeakInterpolationMethod(
  const PeakInterpolationMethodEnum peakInterpolationMethod)
{
  if (this->m_PeakInterpolationMethod != peakInterpolationMethod)
  {
    this->m_PeakInterpolationMethod = peakInterpolationMethod;
    this->Modified();
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::GenerateData()
{
  if (!m_Updating)
  {
    this->Update();
  }
  else
  {
    OffsetType empty;
    empty.Fill(0);
    try
    {
      this->ComputeOffset();
    }
    catch (ExceptionObject & err)
    {
      itkDebugMacro("exception called while computing offset - passing");

      this->SetOffsetCount(1);
      m_Offsets[0] = empty;

      // pass exception to caller
      throw err;
    }
  }

  for (unsigned i = 0; i < m_Offsets.size(); i++)
  {
    // write the result to the output
    auto * output = static_cast<OffsetOutputType *>(this->ProcessObject::GetOutput(0));
    output->Set(m_Offsets[i]);
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::SetFixedImage(
  const ImageBase<ImageType::ImageDimension> * image)
{
  itkDebugMacro("setting fixed image to " << image);
  if (this->GetInput(0) != image)
  {
    this->ProcessObject::SetNthInput(0, const_cast<ImageBase<ImageType::ImageDimension> *>(image));
    this->Modified();
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::SetMovingImage(
  const ImageBase<ImageType::ImageDimension> * image)
{
  itkDebugMacro("setting moving image to " << image);
  if (this->GetInput(1) != image)
  {
    this->ProcessObject::SetNthInput(1, const_cast<ImageBase<ImageType::ImageDimension> *>(image));
    this->Modified();
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::SetRealInput(const ImageType * image)
{
  itkDebugMacro("setting real input image to " << image);
  if (this->GetInput(2) != image)
  {
    this->ProcessObject::SetNthInput(2, const_cast<ImageType *>(image));
    this->Modified();
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::SetComplexInput(const ComplexImageType * image)
{
  itkDebugMacro("setting real input image to " << image);
  if (this->GetInput(3) != image)
  {
    this->ProcessObject::SetNthInput(3, const_cast<ComplexImageType *>(image));
    this->Modified();
  }
}


template <typename TRealPixelType, unsigned int VImageDimension>
void
PhaseCorrelationOptimizer<TRealPixelType, VImageDimension>::ComputeOffset()
{
  const ImageType * fixed = static_cast<ImageType *>(this->GetInput(0));
  const ImageType * moving = static_cast<ImageType *>(this->GetInput(1));
  const ImageType * input = static_cast<ImageType *>(this->GetInput(2));

  const typename ImageType::SpacingType spacing = fixed->GetSpacing();
  const typename ImageType::PointType   fixedOrigin = fixed->GetOrigin();
  const typename ImageType::PointType   movingOrigin = moving->GetOrigin();

  const typename ImageType::RegionType wholeImage = input->GetLargestPossibleRegion();
  const typename ImageType::SizeType   size = wholeImage.GetSize();
  const typename ImageType::IndexType  oIndex = wholeImage.GetIndex();

  // ----- Start sample peak correlation optimization ----- //
  OffsetType offset;
  offset.Fill(0);

  // create the image which will be biased towards the expected solution
  // other pixels get their value reduced by multiplication with
  // e^(-f*(d/s)^2), where f is distancePenaltyFactor,
  // d is pixel's distance, and s is approximate image size
  m_AdjustedInput->CopyInformation(input);
  m_AdjustedInput->SetRegions(input->GetBufferedRegion());
  m_AdjustedInput->Allocate(false);

  typename ImageType::IndexType adjustedSize;
  typename ImageType::IndexType directExpectedIndex;
  typename ImageType::IndexType mirrorExpectedIndex;
  double                        imageSize2 = 0.0; // image size, squared
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    adjustedSize[d] = size[d] + oIndex[d];
    imageSize2 += adjustedSize[d] * adjustedSize[d];
    directExpectedIndex[d] = (movingOrigin[d] - fixedOrigin[d]) / spacing[d] + oIndex[d];
    mirrorExpectedIndex[d] = (movingOrigin[d] - fixedOrigin[d]) / spacing[d] + adjustedSize[d];
  }

  double distancePenaltyFactor = 0.0;
  if (m_PixelDistanceTolerance == 0) // up to about half image size
  {
    distancePenaltyFactor = -10.0 / imageSize2;
  }
  else // up to about five times the provided tolerance
  {
    distancePenaltyFactor = std::log(0.9) / (m_PixelDistanceTolerance * m_PixelDistanceTolerance);
  }

  MultiThreaderBase * mt = this->GetMultiThreader();
  mt->ParallelizeImageRegion<ImageDimension>(
    wholeImage,
    [&](const typename ImageType::RegionType & region) {
      ImageRegionConstIterator<ImageType>     iIt(input, region);
      ImageRegionIteratorWithIndex<ImageType> oIt(m_AdjustedInput, region);
      IndexValueType                          zeroDist2 =
        100 * m_PixelDistanceTolerance * m_PixelDistanceTolerance; // round down to zero further from this
      for (; !oIt.IsAtEnd(); ++iIt, ++oIt)
      {
        typename ImageType::IndexType ind = oIt.GetIndex();
        IndexValueType                dist = 0;
        for (unsigned d = 0; d < ImageDimension; d++)
        {
          IndexValueType distDirect = (directExpectedIndex[d] - ind[d]) * (directExpectedIndex[d] - ind[d]);
          IndexValueType distMirror = (mirrorExpectedIndex[d] - ind[d]) * (mirrorExpectedIndex[d] - ind[d]);
          if (distDirect <= distMirror)
          {
            dist += distDirect;
          }
          else
          {
            dist += distMirror;
          }
        }

        typename ImageType::PixelType pixel;
        if (m_PixelDistanceTolerance > 0 && dist > zeroDist2)
        {
          pixel = 0;
        }
        else // evaluate the expensive exponential function
        {
          pixel = iIt.Get() * std::exp(distancePenaltyFactor * dist);
#ifndef NDEBUG
          pixel *= 1000; // make the intensities in this image more humane (close to 1.0)
                         // it is really hard to count zeroes after decimal point when comparing pixel intensities
                         // since this images is used to find maxima, absolute values are irrelevant
#endif
        }
        oIt.Set(pixel);
      }
    },
    nullptr);

  // WriteDebug(m_AdjustedInput.GetPointer(), "m_AdjustedInput.nrrd");

  if (m_ZeroSuppression > 0.0) // suppress trivial zero solution
  {
    constexpr IndexValueType znSize = 4; // zero neighborhood size, in city-block distance
    mt->ParallelizeImageRegion<ImageDimension>(
      wholeImage,
      [&](const typename ImageType::RegionType & region) {
        ImageRegionIteratorWithIndex<ImageType> oIt(m_AdjustedInput, region);
        for (; !oIt.IsAtEnd(); ++oIt)
        {
          bool                          pixelValid = false;
          typename ImageType::PixelType pixel;
          typename ImageType::IndexType ind = oIt.GetIndex();
          IndexValueType                dist = 0;
          for (unsigned d = 0; d < ImageDimension; d++)
          {
            IndexValueType distD = ind[d] - oIndex[d];
            if (distD > IndexValueType(size[d] / 2)) // wrap around
            {
              distD = size[d] - distD;
            }
            dist += distD;
          }

          if (dist < znSize) // neighborhood of [0,0,...,0] - in case zero peak is blurred
          {
            pixelValid = true;
          }
          else
          {
            for (unsigned d = 0; d < ImageDimension; d++) // lines/sheets of zero m_MaxIndices
            {
              if (ind[d] == oIndex[d]) // one of the m_MaxIndices is "zero"
              {
                pixelValid = true;
              }
            }
          }

          if (pixelValid) // either neighborhood or lines/sheets says update the pixel
          {
            pixel = oIt.Get();
            // avoid the initial steep rise of function x/(1+x) by shifting it by 10
            pixel *= (dist + 10) / (m_ZeroSuppression + dist + 10);
            oIt.Set(pixel);
          }
        }
      },
      nullptr);

    // WriteDebug(m_AdjustedInput.GetPointer(), "m_AdjustedInputZS.nrrd");
  }

  m_MaxCalculator->SetImage(m_AdjustedInput);
  if (m_MergePeaks)
  {
    m_MaxCalculator->SetN(std::ceil(this->m_Offsets.size() / 2) *
                          (static_cast<unsigned>(std::pow(3, ImageDimension)) - 1));
  }
  else
  {
    m_MaxCalculator->SetN(this->m_Offsets.size());
  }

  try
  {
    m_MaxCalculator->ComputeMaxima();
  }
  catch (ExceptionObject & err)
  {
    itkDebugMacro("exception caught during execution of max calculator - passing ");
    throw err;
  }

  this->m_Confidences = m_MaxCalculator->GetMaxima();
  this->m_MaxIndices = m_MaxCalculator->GetIndicesOfMaxima();
  itkAssertOrThrowMacro(this->m_Confidences.size() == m_MaxIndices.size(),
                        "Maxima and their m_MaxIndices must have the same number of elements");
  std::greater<RealPixelType> compGreater;
  const auto zeroBound = std::upper_bound(this->m_Confidences.begin(), this->m_Confidences.end(), 0.0, compGreater);
  if (zeroBound != this->m_Confidences.end()) // there are some non-positive values in here
  {
    unsigned i = zeroBound - this->m_Confidences.begin();
    this->m_Confidences.resize(i);
    m_MaxIndices.resize(i);
  }

  if (m_MergePeaks > 0) // eliminate m_MaxIndices belonging to the same blurry peak
  {
    unsigned i = 1;
    while (i < m_MaxIndices.size())
    {
      unsigned k = 0;
      while (k < i)
      {
        // calculate maximum distance along any dimension
        SizeValueType dist = 0;
        for (unsigned d = 0; d < ImageDimension; d++)
        {
          SizeValueType d1 = itk::Math::abs(m_MaxIndices[i][d] - m_MaxIndices[k][d]);
          if (d1 > size[d] / 2) // wrap around
          {
            d1 = size[d] - d1;
          }
          dist = std::max(dist, d1);
        }
        if (dist <= m_MergePeaks)
        {
          break;
        }
        ++k;
      }

      if (k < i) // k is nearby
      {
        this->m_Confidences[k] += this->m_Confidences[i]; // join amplitudes
        this->m_Confidences.erase(this->m_Confidences.begin() + i);
        m_MaxIndices.erase(m_MaxIndices.begin() + i);
      }
      else // examine next index
      {
        ++i;
      }
    }

    // now we need to re-sort the values
    std::vector<unsigned> sIndices;
    sIndices.reserve(this->m_Confidences.size());
    for (i = 0; i < this->m_Confidences.size(); i++)
    {
      sIndices.push_back(i);
    }
    std::sort(sIndices.begin(), sIndices.end(), [this](unsigned a, unsigned b) {
      return this->m_Confidences[a] > this->m_Confidences[b];
    });

    // now apply sorted order
    typename MaxCalculatorType::ValueVector tMaxs(this->m_Confidences.size());
    typename MaxCalculatorType::IndexVector tIndices(this->m_Confidences.size());
    for (i = 0; i < this->m_Confidences.size(); i++)
    {
      tMaxs[i] = this->m_Confidences[sIndices[i]];
      tIndices[i] = m_MaxIndices[sIndices[i]];
    }
    this->m_Confidences.swap(tMaxs);
    m_MaxIndices.swap(tIndices);
  }

  if (this->m_Offsets.size() > this->m_Confidences.size())
  {
    this->SetOffsetCount(this->m_Confidences.size());
  }
  else
  {
    this->m_Confidences.resize(this->m_Offsets.size());
    m_MaxIndices.resize(this->m_Offsets.size());
  }

  // double confidenceFactor = 1.0 / this->m_Confidences[0];

  for (unsigned m = 0; m < this->m_Confidences.size(); m++)
  {
    using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
    ContinuousIndexType maxIndex = m_MaxIndices[m];

    for (unsigned i = 0; i < ImageDimension; ++i)
    {
      const OffsetScalarType directOffset =
        (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - oIndex[i]);
      const OffsetScalarType mirrorOffset =
        (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - adjustedSize[i]);
      if (itk::Math::abs(directOffset) <= itk::Math::abs(mirrorOffset))
      {
        offset[i] = directOffset;
      }
      else
      {
        offset[i] = mirrorOffset;
      }
    }

    // this->m_Confidences[m] *= confidenceFactor; // normalize - highest confidence will be 1.0
#ifdef NDEBUG
    this->m_Confidences[m] *= 1000.0; // make the intensities more humane (close to 1.0)
#endif

    this->m_Offsets[m] = offset;
  }
  // ----- End sample peak correlation optimization ----- //


  const auto maxIndices = this->m_MaxIndices;

  if (this->m_PeakInterpolationMethod != PeakInterpolationMethodEnum::None) // interpolate the peak
  {
    for (size_t offsetIndex = 0; offsetIndex < this->m_Offsets.size(); ++offsetIndex)
    {
      using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
      ContinuousIndexType           maxIndex = maxIndices[offsetIndex];
      typename ImageType::IndexType tempIndex = maxIndices[offsetIndex];
      typename ImageType::PixelType y0;
      typename ImageType::PixelType y1 = input->GetPixel(tempIndex);
      typename ImageType::PixelType y2;

      for (unsigned i = 0; i < ImageDimension; i++)
      {
        tempIndex[i] = maxIndex[i] - 1;
        if (!wholeImage.IsInside(tempIndex))
        {
          tempIndex[i] = maxIndex[i];
          continue;
        }
        y0 = input->GetPixel(tempIndex);
        tempIndex[i] = maxIndex[i] + 1;
        if (!wholeImage.IsInside(tempIndex))
        {
          tempIndex[i] = maxIndex[i];
          continue;
        }
        y2 = input->GetPixel(tempIndex);
        tempIndex[i] = maxIndex[i];

        OffsetScalarType omega, theta, ratio;
        switch (this->m_PeakInterpolationMethod)
        {
          case PeakInterpolationMethodEnum::Parabolic:
          case PeakInterpolationMethodEnum::WeightedMeanPhase:
            maxIndex[i] += (y0 - y2) / (2 * (y0 - 2 * y1 + y2));
            break;
          case PeakInterpolationMethodEnum::Cosine:
            ratio = (y0 + y2) / (2 * y1);
            if (offsetIndex > 0) // clip to -0.999... to 0.999... range
            {
              ratio = std::min(ratio, 1.0f - std::numeric_limits<OffsetScalarType>::epsilon());
              ratio = std::max(ratio, -1.0f + std::numeric_limits<OffsetScalarType>::epsilon());
            }
            omega = std::acos(ratio);
            theta = std::atan((y0 - y2) / (2 * y1 * std::sin(omega)));
            maxIndex[i] -= ::itk::Math::one_over_pi * theta / omega;
            break;
          default:
            itkAssertInDebugAndIgnoreInReleaseMacro("Unsupported interpolation method");
            break;
        } // switch PeakInterpolationMethod

        const OffsetScalarType directOffset =
          (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - oIndex[i]);
        const OffsetScalarType mirrorOffset =
          (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - adjustedSize[i]);
        if (itk::Math::abs(directOffset) <= itk::Math::abs(mirrorOffset))
        {
          this->m_Offsets[offsetIndex][i] = directOffset;
        }
        else
        {
          this->m_Offsets[offsetIndex][i] = mirrorOffset;
        }
        // TODO: remove
        // std::cout << "MAX Phase GENERATED: " << this->m_Offsets[offsetIndex] << std::endl;
      } // for ImageDimension
    }   // for offsetIndex
    if (this->m_PeakInterpolationMethod == PeakInterpolationMethodEnum::WeightedMeanPhase)
    {
      for (unsigned int peak = 0; peak < this->m_PhaseInterpolated && peak < this->m_Offsets.size(); ++peak)
      {
        this->m_PadFilter->SetInput(this->m_AdjustedInput);
        typename CyclicShiftFilterType::OffsetType shiftFilterOffset;
        for (unsigned int dim = 0; dim < ImageDimension; ++dim)
        {
          shiftFilterOffset[dim] = -maxIndices[peak][dim];
        }
        this->m_CyclicShiftFilter->SetShift(shiftFilterOffset);
        this->m_FFTFilter->Update();
        const typename FFTFilterType::OutputImageType * correlationFFT = this->m_FFTFilter->GetOutput();

        using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
        ContinuousIndexType maxIndex = maxIndices[peak];
        if (this->m_PeakInterpolationMethod == PeakInterpolationMethodEnum::WeightedMeanPhase)
        {
          using SumType = CompensatedSummation<double>;
          SumType                                            powerSum;
          SumType                                            weightedPhase;
          typename FFTFilterType::OutputImageType::IndexType index;
          for (unsigned int dim = 0; dim < ImageDimension; ++dim)
          {
            powerSum.ResetToZero();
            weightedPhase.ResetToZero();
            index.Fill(0);
            const SizeValueType maxFreqIndex = correlationFFT->GetLargestPossibleRegion().GetSize()[dim] / 2;
            for (SizeValueType freqIndex = 1; freqIndex < maxFreqIndex; ++freqIndex)
            {
              index[dim] = freqIndex;
              const typename FFTFilterType::OutputPixelType correlation = correlationFFT->GetPixel(index);
              const double                                  phase = std::arg(correlation);
              const double power = correlation.imag() * correlation.imag() + correlation.real() * correlation.real();
              weightedPhase += phase / Math::pi * power;
              powerSum += power;
            }
            const double deltaToF = -1 * weightedPhase.GetSum() / powerSum.GetSum();
            maxIndex[dim] += deltaToF;
          }
          //} else if(this->m_PeakInterpolationMethod == PeakInterpolationMethodEnum::PhaseFrequencySlope) {
          //// todo: compute the linear regression of the phase, use
          //// slope, add to maxIndex
        }

        for (unsigned i = 0; i < ImageDimension; ++i)
        {
          const OffsetScalarType directOffset =
            (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - oIndex[i]);
          const OffsetScalarType mirrorOffset =
            (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - adjustedSize[i]);
          if (itk::Math::abs(directOffset) <= itk::Math::abs(mirrorOffset))
          {
            this->m_Offsets[peak][i] = directOffset;
          }
          else
          {
            this->m_Offsets[peak][i] = mirrorOffset;
          }
        }
      }
    } // frequency domain interpolation
  }   // interpolate the peak
}


} // end namespace itk

#endif
