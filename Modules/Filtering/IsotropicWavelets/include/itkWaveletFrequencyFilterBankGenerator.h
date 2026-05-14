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
#ifndef itkWaveletFrequencyFilterBankGenerator_h
#define itkWaveletFrequencyFilterBankGenerator_h

#include <itkImageRegionIterator.h>
#include <complex>
#include <itkGenerateImageSource.h>
#include <itkFrequencyFFTLayoutImageRegionIteratorWithIndex.h>

namespace itk
{
/** \class WaveletFrequencyFilterBankGenerator
 * This implementation generates a pair of low-pass / high-pass wavelet filter.
 * Or if HighPassSubBands > 1 multiple high-pass-subbands instead of only one.
 *
 * It does not generate a pyramid of filters, that is done by the \sa WaveletFrequencyForward, so here there is no
 * parameter involving number of levels/scales.
 *
 * The minimum output are two images, one low-pass and other high-pass,
 * and again if HighPassSubBands > 1, multiple high-sub-bands.
 *
 * The wavelet operation is defined in the chosen TWaveletFunction,
 * using TWaveletFunction::EvaluateForwardSubBand, or EvaluateInverseSubBand,
 * where SubBand can be: LowPass, HighPass, or any HighPassSubBand.
 * Also accepts a template FrequencyIterator, to generate images with different frequency layouts.
 *
 * \sa WaveletFrequencyForward
 * \sa FrequencyFunction
 * \sa IsotropicWaveletFrequencyFunction
 * \sa FrequencyImageRegionConstIterator
 *
 * \ingroup IsotropicWavelets
 */
template <typename TOutputImage,
          typename TWaveletFunction,
          typename TFrequencyRegionIterator = FrequencyFFTLayoutImageRegionIteratorWithIndex<TOutputImage>>
class WaveletFrequencyFilterBankGenerator : public itk::GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(WaveletFrequencyFilterBankGenerator);

  /** Standard type alias */
  using Self = WaveletFrequencyFilterBankGenerator;
  using Superclass = itk::GenerateImageSource<TOutputImage>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Type macro */
  itkNewMacro(Self);

  /** Creation through object factory macro */
  itkOverrideGetNameOfClassMacro(WaveletFrequencyFilterBankGenerator);

  /** Inherit types from Superclass. */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  /** Basic type alias */
  using OutputRegionIterator = TFrequencyRegionIterator;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  /** WaveletFunction types */
  using WaveletFunctionType = TWaveletFunction;
  using WaveletFunctionPointer = typename WaveletFunctionType::Pointer;
  using FunctionValueType = typename WaveletFunctionType::FunctionValueType;

  using OutputsType = typename std::vector<OutputImagePointer>;
  // using OutputsType = typename itk::VectorContainer<int, OutputImagePointer>;

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Number of M-Bands decomposition of the high pass filters */
  itkGetMacro(HighPassSubBands, unsigned int);
  void
  SetHighPassSubBands(unsigned int k);

  /** Flag to call generate inverse(reconstruction) filter bank instead
   * of forward (analysis) */
  itkGetMacro(InverseBank, bool);
  itkSetMacro(InverseBank, bool);
  itkBooleanMacro(InverseBank);

  /** Level to scale the wavelet function. Used in undecimated wavelet.
   * /sa WaveletFrequencyForwardUndecimated
   */
  itkGetMacro(Level, unsigned int);
  virtual void
  SetLevel(const unsigned int & level)
  {
    this->m_Level = level;
    this->m_LevelFactor = std::pow(static_cast<double>(this->m_ScaleFactor), static_cast<int>(level));
    this->Modified();
  }

  /** Get pointer to the instance of the wavelet function in order to access and change wavelet parameters */
  itkGetModifiableObjectMacro(WaveletFunction, WaveletFunctionType);

  /** Get Outputs *****/
  OutputImagePointer
  GetOutputLowPass();

  OutputImagePointer
  GetOutputHighPass();

  OutputImagePointer
  GetOutputSubBand(unsigned int k);

  /** Returns all the outputs, starting at low-pass to highest subband*/
  OutputsType
  GetOutputsAll();

  /** Returns all the high pass subbands in ascending order, but not the low pass*/
  OutputsType
  GetOutputsHighPassBands();

#ifdef ITK_USE_CONCEPT_CHECKING
  /// This ensure that OutputPixelType is complex<float||double>
  itkConceptMacro(OutputPixelTypeIsComplexAndFloatCheck,
                  (Concept::IsFloatingPoint<typename OutputImageType::PixelType::value_type>));
#endif

protected:
  WaveletFrequencyFilterBankGenerator();
  ~WaveletFrequencyFilterBankGenerator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  BeforeThreadedGenerateData() override;
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & threadRegion) override;

private:
  unsigned int           m_HighPassSubBands{ 0 };
  bool                   m_InverseBank{ false };
  WaveletFunctionPointer m_WaveletFunction;
  unsigned int           m_Level{ 0 };
  /** Default to 2 (Dyadic). No modifiable, but allow future extensions */
  unsigned int m_ScaleFactor{ 2 };
  /** m_ScaleFactor^m_Level */
  double m_LevelFactor{ 1 };
}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyFilterBankGenerator.hxx"
#endif

#endif
