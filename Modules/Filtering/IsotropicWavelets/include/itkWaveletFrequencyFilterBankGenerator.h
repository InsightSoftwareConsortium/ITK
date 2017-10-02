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
 * The wavelet operation is defined in the chosen TWaveletFuntion,
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
  /** Standard typedefs */
  typedef WaveletFrequencyFilterBankGenerator    Self;
  typedef itk::GenerateImageSource<TOutputImage> Superclass;
  typedef itk::SmartPointer<Self>                Pointer;
  typedef itk::SmartPointer<const Self>          ConstPointer;

  /** Type macro */
  itkNewMacro(Self);

  /** Creation through object factory macro */
  itkTypeMacro(WaveletFrequencyFilterBankGenerator, GenerateImageSourceFilter);

  /** Inherit types from Superclass. */
  typedef typename Superclass::OutputImageType    OutputImageType;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  /** Basic typedefs */
  typedef TFrequencyRegionIterator             OutputRegionIterator;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  /** WaveletFunction types */
  typedef TWaveletFunction                                WaveletFunctionType;
  typedef typename WaveletFunctionType::Pointer           WaveletFunctionPointer;
  typedef typename WaveletFunctionType::FunctionValueType FunctionValueType;

  typedef typename std::vector<OutputImagePointer> OutputsType;
  // typedef typename itk::VectorContainer<int, OutputImagePointer> OutputsType;

  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

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
  virtual ~WaveletFrequencyFilterBankGenerator() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WaveletFrequencyFilterBankGenerator);

  unsigned int           m_HighPassSubBands;
  bool                   m_InverseBank;
  WaveletFunctionPointer m_WaveletFunction;
  unsigned int           m_Level;
  /** Default to 2 (Dyadic). No modifiable, but allow future extensions */
  unsigned int m_ScaleFactor;
  /** m_ScaleFactor^m_Level */
  double m_LevelFactor;
}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyFilterBankGenerator.hxx"
#endif

#endif
