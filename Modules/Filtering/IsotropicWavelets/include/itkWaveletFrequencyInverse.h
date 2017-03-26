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
#ifndef itkWaveletFrequencyInverse_h
#define itkWaveletFrequencyInverse_h

#include <itkImageRegionConstIterator.h>
#include <itkImageRegionIterator.h>
#include <itkImageConstIterator.h>
#include <complex>
#include <itkFixedArray.h>
#include <itkImageToImageFilter.h>

namespace itk
{
/** \class WaveletFrequencyInverse
 * @brief Wavelet analysis where input is an FFT image.
 * Aim to be Isotropic.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage, typename TOutputImage, typename TWaveletFilterBank>
class WaveletFrequencyInverse : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard classs typedefs. */
  typedef WaveletFrequencyInverse                       Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Inherit types from Superclass. */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::OutputImagePointer     OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  typedef typename itk::ImageRegionIterator<OutputImageType>     OutputRegionIterator;
  typedef typename itk::ImageRegionConstIterator<InputImageType> InputRegionConstIterator;
  typedef typename OutputImageType::RegionType                   OutputImageRegionType;

  typedef TWaveletFilterBank                                  WaveletFilterBankType;
  typedef typename WaveletFilterBankType::WaveletFunctionType WaveletFunctionType;
  typedef typename WaveletFilterBankType::FunctionValueType   FunctionValueType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(WaveletFrequencyInverse, ImageToImageFilter);

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
  itkGetConstReferenceMacro(ScaleFactor, unsigned int)

    /**
     * If On, applies to each input the appropiate Level-Band multiplicative factor. Needed for perfect reconstruction.
     * It has to be turned off for some applications (phase analysis for example) */
    itkGetConstReferenceMacro(ApplyReconstructionFactors, bool) itkSetMacro(ApplyReconstructionFactors, bool)
      itkBooleanMacro(ApplyReconstructionFactors);

  typedef std::pair<unsigned int, unsigned int> IndexPairType;
  /** Get the (Level,Band) from a linear index input */
  IndexPairType
  InputIndexToLevelBand(unsigned int linear_index);

  void
  SetInputs(const std::vector<InputImagePointer> & inputs);

  void
  SetInputLowPass(const InputImagePointer & input_low_pass);

  void
  SetInputsHighPass(const std::vector<InputImagePointer> & inputs);

protected:
  WaveletFrequencyInverse();
  ~WaveletFrequencyInverse() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Single-threaded version of GenerateData. */
  void
  GenerateData() ITK_OVERRIDE;

  /************ Information *************/

  /** WaveletFrequencyInverse produces images which are of
   * different resolution and different pixel spacing than its input image.
   * As such, WaveletFrequencyInverse needs to provide an
   * implementation for GenerateOutputInformation() in order to inform the
   * pipeline execution model.  The original documentation of this method is
   * below.
   * \sa ProcessObject::GenerateOutputInformaton()
   */
  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

  /** Given one output whose requested region has been set, this method sets
   * the requested region for the remaining output images.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputRequestedRegion()
   */
  virtual void
  GenerateOutputRequestedRegion(DataObject * output) ITK_OVERRIDE;

  /** WaveletFrequencyInverse requires a larger input requested
   * region than the output requested regions to accommodate the shrinkage and
   * smoothing operations. As such, WaveletFrequencyInverse needs
   * to provide an implementation for GenerateInputRequestedRegion().  The
   * original documentation of this method is below.
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Input images do not occupy the same physical space.
   * Remove the check. */
  virtual void
  VerifyInputInformation() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WaveletFrequencyInverse);

  unsigned int m_Levels;
  unsigned int m_HighPassSubBands;
  unsigned int m_TotalInputs;
  unsigned int m_ScaleFactor;
  bool         m_ApplyReconstructionFactors;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyInverse.hxx"
#endif

#endif
