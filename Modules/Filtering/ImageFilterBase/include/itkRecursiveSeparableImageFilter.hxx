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
#ifndef itkRecursiveSeparableImageFilter_hxx
#define itkRecursiveSeparableImageFilter_hxx

#include "itkRecursiveSeparableImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include <memory> // For unique_ptr

namespace itk
{
template <typename TInputImage, typename TOutputImage>
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::RecursiveSeparableImageFilter()
  : m_N0(1.0)
  , m_N1(1.0)
  , m_N2(1.0)
  , m_N3(1.0)
  , m_D1(0.0)
  , m_D2(0.0)
  , m_D3(0.0)
  , m_D4(0.0)
  , m_M1(0.0)
  , m_M2(0.0)
  , m_M3(0.0)
  , m_M4(0.0)
  , m_BN1(0.0)
  , m_BN2(0.0)
  , m_BN3(0.0)
  , m_BN4(0.0)
  , m_BM1(0.0)
  , m_BM2(0.0)
  , m_BM3(0.0)
  , m_BM4(0.0)

{
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);

  this->InPlaceOff();
}

/**
 * Set Input Image
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::SetInputImage(const TInputImage * input)
{
  // ProcessObject is not const_correct so this const_cast is required
  ProcessObject::SetNthInput(0, const_cast<TInputImage *>(input));
}

/**
 * Get Input Image
 */
template <typename TInputImage, typename TOutputImage>
const TInputImage *
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::GetInputImage()
{
  return dynamic_cast<const TInputImage *>((ProcessObject::GetInput(0)));
}

/**
 * Apply Recursive Filter
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::FilterDataArray(RealType * const       outs,
                                                                          const RealType * const data,
                                                                          RealType * const       scratch,
                                                                          const SizeValueType    ln) const
{

  RealType * const scratch1 = outs;
  RealType * const scratch2 = scratch;
  /**
   * Causal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType & outV1 = data[0];

  /**
   * Initialize borders
   */

  MathEMAMAMAM(scratch1[0], outV1, m_N0, outV1, m_N1, outV1, m_N2, outV1, m_N3);
  MathEMAMAMAM(scratch1[1], data[1], m_N0, outV1, m_N1, outV1, m_N2, outV1, m_N3);
  MathEMAMAMAM(scratch1[2], data[2], m_N0, data[1], m_N1, outV1, m_N2, outV1, m_N3);
  MathEMAMAMAM(scratch1[3], data[3], m_N0, data[2], m_N1, data[1], m_N2, outV1, m_N3);

  // note that the outV1 value is multiplied by the Boundary coefficients m_BNi
  MathSMAMAMAM(scratch1[0], outV1, m_BN1, outV1, m_BN2, outV1, m_BN3, outV1, m_BN4);
  MathSMAMAMAM(scratch1[1], scratch1[0], m_D1, outV1, m_BN2, outV1, m_BN3, outV1, m_BN4);
  MathSMAMAMAM(scratch1[2], scratch1[1], m_D1, scratch1[0], m_D2, outV1, m_BN3, outV1, m_BN4);
  MathSMAMAMAM(scratch1[3], scratch1[2], m_D1, scratch1[1], m_D2, scratch1[0], m_D3, outV1, m_BN4);

  /**
   * Recursively filter the rest
   */
  for (unsigned int i = 4; i < ln; i++)
  {
    MathEMAMAMAM(scratch1[i], data[i], m_N0, data[i - 1], m_N1, data[i - 2], m_N2, data[i - 3], m_N3);
    MathSMAMAMAM(
      scratch1[i], scratch1[i - 1], m_D1, scratch1[i - 2], m_D2, scratch1[i - 3], m_D3, scratch1[i - 4], m_D4);
  }

  /**
   * Store the causal result: outs = scratch already done via alias
   *
   */

  /**
   * AntiCausal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType & outV2 = data[ln - 1];

  /**
   * Initialize borders
   */
  MathEMAMAMAM(scratch2[ln - 1], outV2, m_M1, outV2, m_M2, outV2, m_M3, outV2, m_M4);
  MathEMAMAMAM(scratch2[ln - 2], data[ln - 1], m_M1, outV2, m_M2, outV2, m_M3, outV2, m_M4);
  MathEMAMAMAM(scratch2[ln - 3], data[ln - 2], m_M1, data[ln - 1], m_M2, outV2, m_M3, outV2, m_M4);
  MathEMAMAMAM(scratch2[ln - 4], data[ln - 3], m_M1, data[ln - 2], m_M2, data[ln - 1], m_M3, outV2, m_M4);

  // note that the outV2value is multiplied by the Boundary coefficients m_BMi
  MathSMAMAMAM(scratch2[ln - 1], outV2, m_BM1, outV2, m_BM2, outV2, m_BM3, outV2, m_BM4);
  MathSMAMAMAM(scratch2[ln - 2], scratch2[ln - 1], m_D1, outV2, m_BM2, outV2, m_BM3, outV2, m_BM4);
  MathSMAMAMAM(scratch2[ln - 3], scratch2[ln - 2], m_D1, scratch2[ln - 1], m_D2, outV2, m_BM3, outV2, m_BM4);
  MathSMAMAMAM(scratch2[ln - 4], scratch2[ln - 3], m_D1, scratch2[ln - 2], m_D2, scratch2[ln - 1], m_D3, outV2, m_BM4);

  /**
   * Recursively filter the rest
   */
  for (unsigned int i = ln - 4; i > 0; i--)
  {
    MathEMAMAMAM(scratch2[i - 1], data[i], m_M1, data[i + 1], m_M2, data[i + 2], m_M3, data[i + 3], m_M4);
    MathSMAMAMAM(
      scratch2[i - 1], scratch2[i], m_D1, scratch2[i + 1], m_D2, scratch2[i + 2], m_D3, scratch2[i + 3], m_D4);
  }

  /**
   * Roll the antiCausal part into the output
   */
  for (unsigned int i = 0; i < ln; i++)
  {
    outs[i] += scratch2[i];
  }
}

//
// we need all of the image in just the "Direction" we are separated into
//
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  auto * out = dynamic_cast<TOutputImage *>(output);

  if (out)
  {
    OutputImageRegionType         outputRegion = out->GetRequestedRegion();
    const OutputImageRegionType & largestOutputRegion = out->GetLargestPossibleRegion();

    // verify sane parameter
    if (this->m_Direction >= outputRegion.GetImageDimension())
    {
      itkExceptionMacro("Direction selected for filtering is greater than ImageDimension")
    }

    // expand output region to match largest in the "Direction" dimension
    outputRegion.SetIndex(m_Direction, largestOutputRegion.GetIndex(m_Direction));
    outputRegion.SetSize(m_Direction, largestOutputRegion.GetSize(m_Direction));

    out->SetRequestedRegion(outputRegion);
  }
}


template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  using RegionType = ImageRegion<TInputImage::ImageDimension>;

  typename TInputImage::ConstPointer inputImage(this->GetInputImage());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  const unsigned int imageDimension = inputImage->GetImageDimension();

  if (this->m_Direction >= imageDimension)
  {
    itkExceptionMacro("Direction selected for filtering is greater than ImageDimension");
  }

  const typename InputImageType::SpacingType & pixelSize = inputImage->GetSpacing();

  this->SetUp(pixelSize[m_Direction]);

  const RegionType region = outputImage->GetRequestedRegion();

  const unsigned int ln = region.GetSize()[this->m_Direction];

  if (ln < 4)
  {
    itkExceptionMacro(
      "The number of pixels along direction "
      << this->m_Direction
      << " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
  }
}

template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  // Call a method that can be overridden by a subclass to allocate
  // memory for the filter's outputs
  this->AllocateOutputs();

  // Call a method that can be overridden by a subclass to perform
  // some calculations prior to splitting the main computations into
  // separate threads
  this->BeforeThreadedGenerateData();

  using RegionType = ImageRegion<TInputImage::ImageDimension>;
  typename TOutputImage::Pointer outputImage(this->GetOutput());
  const RegionType               region = outputImage->GetRequestedRegion();

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  this->GetMultiThreader()->template ParallelizeImageRegionRestrictDirection<TOutputImage::ImageDimension>(
    this->m_Direction,
    region,
    [this](const RegionType & lambdaRegion) { this->DynamicThreadedGenerateData(lambdaRegion); },
    this);
}

/**
 * Compute Recursive filter
 * line by line in one of the dimensions
 */
template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  using OutputPixelType = typename TOutputImage::PixelType;

  using InputConstIteratorType = ImageLinearConstIteratorWithIndex<TInputImage>;
  using OutputIteratorType = ImageLinearIteratorWithIndex<TOutputImage>;

  using RegionType = ImageRegion<TInputImage::ImageDimension>;

  typename TInputImage::ConstPointer inputImage(this->GetInputImage());
  typename TOutputImage::Pointer     outputImage(this->GetOutput());

  RegionType region = outputRegionForThread;

  InputConstIteratorType inputIterator(inputImage, region);
  OutputIteratorType     outputIterator(outputImage, region);

  inputIterator.SetDirection(this->m_Direction);
  outputIterator.SetDirection(this->m_Direction);

  const SizeValueType ln = region.GetSize(this->m_Direction);

  const std::unique_ptr<RealType[]> inps(new RealType[ln]);
  const std::unique_ptr<RealType[]> outs(new RealType[ln]);
  const std::unique_ptr<RealType[]> scratch(new RealType[ln]);

  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  while (!inputIterator.IsAtEnd() && !outputIterator.IsAtEnd())
  {
    unsigned int i = 0;
    while (!inputIterator.IsAtEndOfLine())
    {
      inps[i++] = inputIterator.Get();
      ++inputIterator;
    }

    this->FilterDataArray(outs.get(), inps.get(), scratch.get(), ln);

    unsigned int j = 0;
    while (!outputIterator.IsAtEndOfLine())
    {
      outputIterator.Set(static_cast<OutputPixelType>(outs[j++]));
      ++outputIterator;
    }

    inputIterator.NextLine();
    outputIterator.NextLine();
  }
}

template <typename TInputImage, typename TOutputImage>
void
RecursiveSeparableImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Direction: " << m_Direction << std::endl;
}

} // end namespace itk

#endif
