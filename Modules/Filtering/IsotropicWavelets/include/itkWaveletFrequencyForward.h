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
  /** Standard typenames typedefs. */
  typedef WaveletFrequencyForward                       Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Inherit types from Superclass. */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::OutputImagePointer     OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  typedef typename std::vector<OutputImagePointer> OutputsType;
  // typedef typename itk::VectorContainer<int, OutputImagePointer> OutputsType;

  typedef typename itk::ImageRegionIterator<OutputImageType>     OutputRegionIterator;
  typedef typename itk::ImageRegionConstIterator<InputImageType> InputRegionConstIterator;
  typedef typename OutputImageType::RegionType                   OutputImageRegionType;

  typedef TWaveletFilterBank                                  WaveletFilterBankType;
  typedef typename WaveletFilterBankType::Pointer             WaveletFilterBankPointer;
  typedef typename WaveletFilterBankType::WaveletFunctionType WaveletFunctionType;
  typedef typename WaveletFilterBankType::FunctionValueType   FunctionValueType;

  typedef TFrequencyShrinkFilterType FrequencyShrinkFilterType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

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
  itkSetMacro(StoreWaveletFilterBankPyramid, bool) itkGetMacro(StoreWaveletFilterBankPyramid, bool)
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
  typedef std::pair<unsigned int, unsigned int> IndexPairType;
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
  ~WaveletFrequencyForward() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Single-threaded version of GenerateData. */
  void
  GenerateData() ITK_OVERRIDE;

  /************ Information *************/

  /** WaveletFrequencyForward produces images which are of
   * different resolution and different pixel spacing than its input image.
   * As such, WaveletFrequencyForward needs to provide an
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

  /** WaveletFrequencyForward requires a larger input requested
   * region than the output requested regions to accommodate the shrinkage and
   * smoothing operations. As such, WaveletFrequencyForward needs
   * to provide an implementation for GenerateInputRequestedRegion().  The
   * original documentation of this method is below.
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WaveletFrequencyForward);

  unsigned int             m_Levels;
  unsigned int             m_HighPassSubBands;
  unsigned int             m_TotalOutputs;
  unsigned int             m_ScaleFactor;
  WaveletFilterBankPointer m_WaveletFilterBank;
  bool                     m_StoreWaveletFilterBankPyramid;
  OutputsType              m_WaveletFilterBankPyramid;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyForward.hxx"
#endif

#endif
