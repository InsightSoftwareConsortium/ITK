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
 * \brief Generate filter bank of wavelet images.
 *
 * This implementation generates a low-pass / high-pass wavelet filter .
 *
 * The output are two images, one low-pass and other high-pass, downsampled by DownSampleImageFactor.
 *
 * The wavelet operation is defined by an user chosen SpatialFunction
 * The Spatial Function can be in the frequency or spatial domain.

 * \warning This filter requires that the output image pixels are of a floating point type.
 * It won't compile otherwise if Concept_Checking is enabled.
 *
 * \sa WaveletFT
 * \sa SpatialFunction
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
  std::vector<OutputImagePointer>
  GetOutputsAll();

  /** Returns all the high pass subbands in ascending order, but not the low pass*/
  std::vector<OutputImagePointer>
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
}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWaveletFrequencyFilterBankGenerator.hxx"
#endif

#endif
