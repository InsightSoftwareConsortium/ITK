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
#ifndef itkWaveletFrequencyInverseUndecimated_h
#define itkWaveletFrequencyInverseUndecimated_h

#include <itkImageRegionConstIterator.h>
#include <itkImageRegionIterator.h>
#include <itkImageConstIterator.h>
#include <complex>
#include <itkFixedArray.h>
#include <itkImageToImageFilter.h>

namespace itk
{
/** \class WaveletFrequencyInverseUndecimated
 * @brief Wavelet analysis where input is an FFT image.
 * Aim to be Isotropic.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
class WaveletFrequencyInverseUndecimated : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(WaveletFrequencyInverseUndecimated);

  /** Standard class type alias. */
  using Self = WaveletFrequencyInverseUndecimated;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Inherit types from Superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;

  using InputsType = typename std::vector<InputImagePointer>;
  // using InputsType = typename itk::VectorContainer<int, InputImagePointer>;

  using WaveletFilterBankType = TWaveletFilterBank;
  using WaveletFilterBankPointer = typename WaveletFilterBankType::Pointer;
  using WaveletFunctionType = typename WaveletFilterBankType::WaveletFunctionType;
  using FunctionValueType = typename WaveletFilterBankType::FunctionValueType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkOverrideGetNameOfClassMacro(WaveletFrequencyInverseUndecimated);

  /** Number of levels/scales. Maximum depends on size of image */
  void
  SetLevels(unsigned int n);
  itkGetMacro(Levels, unsigned int);

  /** Number of high pass subbands, 1 minimum */
  void
  SetHighPassSubBands(unsigned int n);
  itkGetMacro(HighPassSubBands, unsigned int);

  /** Total number of inputs required: Levels*HighPassSubBands + 1 */
  itkGetMacro(TotalInputs, unsigned int);

  /** Shrink/Expand factor at each level.
   * Set to 2 (dyadic), not modifiable, but providing future flexibility */
  itkGetConstReferenceMacro(ScaleFactor, unsigned int);

  /**
   * If On, applies to each input the appropriate Level-Band multiplicative factor. Needed for perfect reconstruction.
   * It has to be turned off for some applications (phase analysis for example) */
  itkGetConstReferenceMacro(ApplyReconstructionFactors, bool);
  itkSetMacro(ApplyReconstructionFactors, bool);
  itkBooleanMacro(ApplyReconstructionFactors);

  /** Flag to use external WaveletFilterBankPyramid generated in ForwardWavelet.
   * Requires to use SetWaveletFilterBankPyramid. */
  itkGetConstReferenceMacro(UseWaveletFilterBankPyramid, bool);
  itkSetMacro(UseWaveletFilterBankPyramid, bool);
  itkBooleanMacro(UseWaveletFilterBankPyramid);

  /**
   * Set vector containing the WaveletFilterBankPyramid.
   * This vector is generated in the ForwardWavelet when StoreWaveletFilterBankPyramid is On.
   */
  void
  SetWaveletFilterBankPyramid(const InputsType & filterBankPyramid)
  {
    this->m_WaveletFilterBankPyramid = filterBankPyramid;
  }

  using IndexPairType = std::pair<unsigned int, unsigned int>;
  /** Get the (Level,Band) from a linear index input */
  IndexPairType
  InputIndexToLevelBand(unsigned int linear_index);

  void
  SetInputs(const InputsType & inputs);

  void
  SetInputLowPass(const InputImagePointer & input_low_pass);

  void
  SetInputsHighPass(const InputsType & inputs);

protected:
  WaveletFrequencyInverseUndecimated();
  ~WaveletFrequencyInverseUndecimated() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Single-threaded version of GenerateData. */
  void
  GenerateData() override;

  /************ Information *************/

  /** WaveletFrequencyInverseUndecimated produces images which are of
   * different resolution and different pixel spacing than its input image.
   * As such, WaveletFrequencyInverseUndecimated needs to provide an
   * implementation for GenerateOutputInformation() in order to inform the
   * pipeline execution model.  The original documentation of this method is
   * below.
   * \sa ProcessObject::GenerateOutputInformation()
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

  /** WaveletFrequencyInverseUndecimated requires a larger input requested
   * region than the output requested regions to accommodate the shrinkage and
   * smoothing operations. As such, WaveletFrequencyInverseUndecimated needs
   * to provide an implementation for GenerateInputRequestedRegion().  The
   * original documentation of this method is below.
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  void
  GenerateInputRequestedRegion() override;

  /** Input images do not occupy the same physical space.
   * Remove the check. */
  void
  VerifyInputInformation() const override {};

private:
  unsigned int             m_Levels{ 1 };
  unsigned int             m_HighPassSubBands{ 1 };
  unsigned int             m_TotalInputs{ 0 };
  unsigned int             m_ScaleFactor{ 2 };
  bool                     m_ApplyReconstructionFactors{ true };
  bool                     m_UseWaveletFilterBankPyramid{ false };
  WaveletFilterBankPointer m_WaveletFilterBank;
  InputsType               m_WaveletFilterBankPyramid;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyInverseUndecimated.hxx"
#endif

#endif
