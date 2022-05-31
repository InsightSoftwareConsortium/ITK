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
#ifndef itkFrequencyExpandImageFilter_h
#define itkFrequencyExpandImageFilter_h

#include <itkImageToImageFilter.h>

namespace itk
{
/** \class FrequencyExpandImageFilter
 * \brief Expand the size of an image in the frequency domain by an integer
 * factor in each dimension.
 *
 * FrequencyExpandImageFilter increases the size of an image in the frequency domain
 * (for example, the output of a FFTImageFilter), by an integer
 * factor in each dimension.
 * The output image is composed by the input image, but all the new high
 * frequency pixels are set to Zero, no interpolation is made.
 *
 * Note that the input image shouldn't be shifted by ShiftFFTImageFilter.
 * It must conserve the following standard structure in frequency (in all dims):
 *
 * N = even , f_Nyq is unique, but shared between pos and neg freqs.
 * [ <---------f_pos ---------------f_Nyq-->                         ]
 * [                         <------f_Nyq-----------f_neg ---------> ]
 * [0 LowFreqs  .. HighFreqs f_Nyquist(at N/2) HighFreqs .. LowFreqs ]
 *
 * N = odd , f_Nyq has pos and neg freqs components.
 * [ <----f_pos ---f_Nyq(N/2)---><---f_Nyq((N+1)/2) --f_neg --> ]
 * [0 LFs  .. HFs f_Nyq(at N/2)    f_Nyq(at (N+1)/2) HFs .. LFs ]
 *
 * The output image size in each dimension is given by:
 *
 * OutputSize[j] = InputSize[j] * ExpandFactors[j]
 *
 * This filter will produce an output with different pixel spacing
 * that its input image such that:
 *
 * OutputSpacing[j] = InputSpacing[j] / ExpandFactors[j]
 *
 * The filter is templated over the input image, and will produce the same image type for the output.
 *
 * Example (Odd):
 * inputSize     = 5
 * expandFactors = [2]
 * outputSize    = 10
 *                      f_pos              f_neg
 * inputImageIndices  = 0 1 2               3 4
 * outputImageIndices = 0 1 2 [ 3 4 5 6 7 ] 8 9
 *                            [   Zeros   ]
 * So: input(4) == output(9),etc
 *
 * Example (Even):
 * inputSize     = 8
 * expandFactors = [2]
 * outputSize    = 16
 *                        f_pos                          f_neg
 * inputImageIndices  = 0 1 2 3 4                     (4) 5  6  7
 * outputImageIndices = 0 1 2 3 4 [ 5 6 7 8 9 10 11 ] 12 13 14 15
 *                                [      Zeros      ]
 * So: input(7) == output(15), etc
 *
 * Paste input regions to the outputImage:
 * the positive frequencies: index <= floor(inputSize/2.0) at the begining of the output, including zero component, and
 * negative frequencies: index > floor(inputSize/2.0) (>= if even) at the end.
 *
 * If inputSize[dim] is even, Nyquist (highest) freq is unique, but shared between negative and positive frequencies. So
 this freq (index=4) it is copied to the output: index >= floor(inputSize/2.0).
 *
 * If the input image is generated from an FFT of a real image, then the input is hermitian;
 * 0 (DC)
 * I(1) == I(N-1)
 * I(2) == I(N-2)
 * ...
 * I(N/2) if N=even. Unique Nyquist: shared between pos and neg freqs.
 * OR
 * I(N/2) == I((N+1)/2) if N=odd. Nyquist has pos and neg components.
 *
 * Note that this filter doesn't require the input to be hermitian.

 * This code was contributed in the Insight Journal paper:
 * https://hdl.handle.net....
 *
 * \ingroup IsotropicWavelets
 */
template <typename TImageType>
class FrequencyExpandImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FrequencyExpandImageFilter);

  /** Standard class type alias. */
  using Self = FrequencyExpandImageFilter;
  using Superclass = ImageToImageFilter<TImageType, TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyExpandImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  using ImageRegionType = typename TImageType::RegionType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TImageType::ImageDimension;

  /** Inherit some types from superclass. */
  using ImageType = typename Superclass::InputImageType;
  using PixelType = typename ImageType::PixelType;
  using ImagePointer = typename ImageType::Pointer;

  /** The type of the expand factors representation */
  using ExpandFactorsType = FixedArray<unsigned int, ImageDimension>;

  /** Set the expand factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ExpandFactors, ExpandFactorsType);
  virtual void
  SetExpandFactors(const unsigned int factor);

  /** Get the expand factors. */
  itkGetConstReferenceMacro(ExpandFactors, ExpandFactorsType);

  /** FrequencyExpandImageFilter produces an image which is a different resolution and
   * with a different pixel spacing than its input image.  As such,
   * FrequencyExpandImageFilter needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformation() */
  void
  GenerateOutputInformation() override;

  /** FrequencyExpandImageFilter needs a smaller input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  // End concept checking
#endif

protected:
  FrequencyExpandImageFilter();
  ~FrequencyExpandImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;


  void
  GenerateData() override;

private:
  ExpandFactorsType m_ExpandFactors;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyExpandImageFilter.hxx"
#endif

#endif
