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
#ifndef itkWaveletFrequencyForward_h
#define itkWaveletFrequencyForward_h

#include <itkImageRegionConstIterator.h>
#include <itkImageRegionIterator.h>
#include <itkImageConstIterator.h>
#include <complex>
#include <itkFixedArray.h>
#include <itkImageToImageFilter.h>
#include <itkFrequencyShrinkImageFilter.h>
#include <itkFrequencyShrinkViaInverseFFTImageFilter.h>

namespace itk
{
/** \class WaveletFrequencyForward
 * @brief IsotropicWavelet multiscale analysis where input is an image in the frequency domain.
 * Output Layout:
 * Output m_TotalOutputs - 1 is the residual low pass filtered of the last level/scale.
 * [N - 1]: Low pass residual, also called approximation.
 * [0,..,HighPassBands): Wavelet coef of first level.
 * [HighPassBands,..,l*HighPassBands]: Wavelet coef of l level.
 *
 * @note The information/metadata of input image is ignored.
 * It can be restored after reconstruction @sa WaveletFrequencyInverse
 * with a @sa ChangeInformationFilter using the input image as a reference.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TWaveletFilterBank,
          typename TFrequencyShrinkFilterType = FrequencyShrinkImageFilter<TOutputImage>>
// FrequencyShrinkViaInverseFFTImageFilter<TOutputImage> >
class WaveletFrequencyForward : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(WaveletFrequencyForward);

  /** Standard typenames type alias. */
  using Self = WaveletFrequencyForward;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Inherit types from Superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;

  using OutputsType = typename std::vector<OutputImagePointer>;
  // using OutputsType = typename itk::VectorContainer<int, OutputImagePointer>;

  using OutputRegionIterator = typename itk::ImageRegionIterator<OutputImageType>;
  using InputRegionConstIterator = typename itk::ImageRegionConstIterator<InputImageType>;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using WaveletFilterBankType = TWaveletFilterBank;
  using WaveletFilterBankPointer = typename WaveletFilterBankType::Pointer;
  using WaveletFunctionType = typename WaveletFilterBankType::WaveletFunctionType;
  using FunctionValueType = typename WaveletFilterBankType::FunctionValueType;

  using FrequencyShrinkFilterType = TFrequencyShrinkFilterType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(WaveletFrequencyForward, ImageToImageFilter);
  virtual void
  SetLevels(unsigned int n);

  itkGetConstReferenceMacro(Levels, unsigned int);
  virtual void
  SetHighPassSubBands(unsigned int n);

  itkGetConstReferenceMacro(HighPassSubBands, unsigned int);
  itkGetConstReferenceMacro(TotalOutputs, unsigned int);

  /** ScaleFactor for each level in the pyramid.
   * Set to 2 (dyadic) at constructor and not modifiable, but provides future flexibility */
  itkGetConstReferenceMacro(ScaleFactor, unsigned int);
  // itkSetMacro(ScaleFactor, unsigned int);

  /** Return modifiable pointer of the wavelet filter bank member. */
  itkGetModifiableObjectMacro(WaveletFilterBank, WaveletFilterBankType);
  /** Return modifiable pointer to the wavelet function, which is a member of wavelet filter bank. */
  virtual WaveletFunctionType *
  GetModifiableWaveletFunction()
  {
    return this->GetModifiableWaveletFilterBank()->GetModifiableWaveletFunction();
  }

  /** Flag to store the wavelet Filter Bank Pyramid, for all levels and all bands.
   * Access to it with GetWaveletFilterBankPyramid()*/
  itkSetMacro(StoreWaveletFilterBankPyramid, bool);
  itkGetMacro(StoreWaveletFilterBankPyramid, bool);
  itkBooleanMacro(StoreWaveletFilterBankPyramid);

  itkGetMacro(WaveletFilterBankPyramid, OutputsType);

  /** Compute max number of levels depending on the size of the image.
   * Return J: $ J = \text{min_element}\{J_0,\ldots, J_d\} $;
   * where each $J_i$ is the  number of integer divisions that can be done with the $i$ size and the scale factor.
   */
  static unsigned int
  ComputeMaxNumberOfLevels(const typename InputImageType::SizeType & input_size, const unsigned int scaleFactor = 2);

  /** (Level, band) pair.
   * Level from: [0, m_Levels), and equal to m_Levels only for the low_pass image.
   * band from [0, m_HighPassSubbands) */
  using IndexPairType = std::pair<unsigned int, unsigned int>;
  /** Get the (Level,Band) from a linear index output.
   * The index corresponding to the low-pass image is the last one, corresponding to the
   * IndexPairType(this->GetLevels(), 0).
   */
  IndexPairType
  OutputIndexToLevelBand(unsigned int linear_index);

  /** Retrieve outputs */
  OutputsType
  GetOutputs();

  OutputsType
  GetOutputsHighPass();

  OutputImagePointer
  GetOutputLowPass();

  OutputsType
  GetOutputsHighPassByLevel(unsigned int level);

protected:
  WaveletFrequencyForward();
  ~WaveletFrequencyForward() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Single-threaded version of GenerateData. */
  void
  GenerateData() override;

  /************ Information *************/

  /** WaveletFrequencyForward produces images which are of
   * different resolution and different pixel spacing than its input image.
   * As such, WaveletFrequencyForward needs to provide an
   * implementation for GenerateOutputInformation() in order to inform the
   * pipeline execution model.  The original documentation of this method is
   * below.
   * \sa ProcessObject::GenerateOutputInformaton()
   */
  void
  GenerateOutputInformation() override;

  /** Given one output whose requested region has been set, this method sets
   * the requested region for the remaining output images.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputRequestedRegion()
   */
  void
  GenerateOutputRequestedRegion(DataObject * output) override;

  /** WaveletFrequencyForward requires a larger input requested
   * region than the output requested regions to accommodate the shrinkage and
   * smoothing operations. As such, WaveletFrequencyForward needs
   * to provide an implementation for GenerateInputRequestedRegion().  The
   * original documentation of this method is below.
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  void
  GenerateInputRequestedRegion() override;

private:
  unsigned int             m_Levels{ 1 };
  unsigned int             m_HighPassSubBands{ 1 };
  unsigned int             m_TotalOutputs{ 1 };
  unsigned int             m_ScaleFactor{ 2 };
  WaveletFilterBankPointer m_WaveletFilterBank;
  bool                     m_StoreWaveletFilterBankPyramid{ false };
  OutputsType              m_WaveletFilterBankPyramid;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyForward.hxx"
#endif

#endif
