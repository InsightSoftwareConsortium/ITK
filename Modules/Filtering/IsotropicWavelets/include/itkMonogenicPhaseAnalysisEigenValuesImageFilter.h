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
#ifndef itkMonogenicPhaseAnalysisEigenValuesImageFilter_h
#define itkMonogenicPhaseAnalysisEigenValuesImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkVectorImage.h>
#include <itkImageRegionConstIterator.h>
#include <itkImageRegionIterator.h>
#include <itkImage.h>
#include <itkFixedArray.h>
#include <itkSymmetricSecondRankTensor.h>
#include "itkBarrier.h"
namespace itk
{
/** \class MonogenicPhaseAnalysisEigenValuesImageFilter
 * This filter operates on an input MonogenicSignal in the Spatial Domain.
 * Represented as a VectorImage of ImageDimension + 1.
 * f_m ={ f, R_x*f R_y*f ... }
 * where f is a real image. R_x, R_y, ... are directional Riesz transforms in the spatial domain.
 *
 * Dev note: The best way to get the spatial Riesz, is doing an inverse FFT of the output of
 * \sa MonogenicSignalFrequencyImageFilter
 * All filters in the module IsotropicWavelets avoid doing any FFT,
 * even though they work on the frequency domain. This is a dev decission to decouple algorithms from
 * the specific frequency layout of the FFT of choice.
 * User just have to modify the GetFrequency in a new FrequencyIterator if other FFT library is chosen.
 *
 * The output should be a new real image f', so it can be integrated to an inverse Wavelet pyramid.
 * \sa itkWaveletFrequencyInverse
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::ComponentType, TInputImage::ImageDimension>>
class MonogenicPhaseAnalysisEigenValuesImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef MonogenicPhaseAnalysisEigenValuesImageFilter  Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MonogenicPhaseAnalysisEigenValuesImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType;
  typedef typename InputImageType::PixelType      InputImagePixelType;
  typedef typename InputImageType::SpacingType    SpacingType;
  typedef typename InputImageRegionType::SizeType SizeType;

  typedef SpacingType                               DirectionType;
  typedef typename InputImageType::SpacingValueType FloatType;

  typedef typename OutputImageType::Pointer                      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer                 OutputImageConstPointer;
  typedef typename OutputImageType::RegionType                   OutputImageRegionType;
  typedef typename itk::ImageRegionIterator<OutputImageType>     OutputImageRegionIterator;
  typedef typename OutputImageType::PixelType                    OutputImagePixelType;
  typedef typename itk::ImageRegionConstIterator<InputImageType> InputImageRegionConstIterator;

#ifdef ITK_USE_CONCEPT_CHECKING
  /// This ensure that PixelType is float||double, and not complex.
  itkConceptMacro(OutputPixelTypeIsFloatCheck, (Concept::IsFloatingPoint<typename TOutputImage::PixelType>));
#endif
  typedef itk::Image<itk::Matrix<FloatType, ImageDimension, ImageDimension>, ImageDimension> EigenVectorsImageType;
  typedef itk::Image<itk::FixedArray<FloatType, ImageDimension>, ImageDimension>             EigenValuesImageType;

  itkSetMacro(GaussianWindowRadius, FloatType);
  itkGetConstMacro(GaussianWindowRadius, FloatType);
  itkSetMacro(GaussianWindowSigma, FloatType);
  itkGetConstMacro(GaussianWindowSigma, FloatType);
  itkGetConstMacro(NC, unsigned int) itkSetMacro(ApplySoftThreshold, bool);
  itkGetConstMacro(ApplySoftThreshold, bool);
  itkGetConstMacro(EigenVectorsImage, typename EigenVectorsImageType::Pointer);
  itkGetConstMacro(EigenValuesImage, typename EigenValuesImageType::Pointer);

  inline OutputImagePixelType
  ComputeRieszNormSquare(const InputImagePixelType & monoPixel) const;
  /**************** Helpers requiring the square norm of Riesz *******************/
  inline OutputImagePixelType
  ComputeAmplitude(const InputImagePixelType & monoPixel, const OutputImagePixelType & rieszNormSquare) const;
  inline OutputImagePixelType
  ComputePhase(const InputImagePixelType & monoPixel, const OutputImagePixelType & rieszNormSquare) const;
  inline itk::FixedArray<OutputImagePixelType, ImageDimension - 1>
  ComputePhaseOrientation(const InputImagePixelType & monoPixel, const OutputImagePixelType & rieszNormSquare) const;
  /**************** With Directions *******************/
  OutputImagePixelType
  ComputeRieszProjection(const InputImagePixelType & monoPixel, const DirectionType & direction) const;

  std::pair<typename EigenVectorsImageType::Pointer, typename EigenValuesImageType::Pointer>
  ComputeEigenAnalysisInNeighborhoodWindow(const unsigned int &          gaussian_window_radius,
                                           const FloatType &             gaussian_window_sigma,
                                           const OutputImageRegionType & outputRegionForThread) const;

protected:
  MonogenicPhaseAnalysisEigenValuesImageFilter();
  ~MonogenicPhaseAnalysisEigenValuesImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MonogenicPhaseAnalysisEigenValuesImageFilter);
  // User can select value
  unsigned int m_GaussianWindowRadius;
  FloatType    m_GaussianWindowSigma;
  // Inner variables.
  /** Number of Components of input image (Helper) */
  unsigned int         m_NC;
  bool                 m_ApplySoftThreshold;
  OutputImagePixelType m_MeanAmp;
  OutputImagePixelType m_SigmaAmp;
  OutputImagePixelType m_Threshold;
  Barrier::Pointer     m_Barrier1;
  Barrier::Pointer     m_Barrier2;
  // mutable for caching results of methods that are logically const.
  mutable typename EigenVectorsImageType::Pointer m_EigenVectorsImage;
  mutable typename EigenValuesImageType::Pointer  m_EigenValuesImage;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMonogenicPhaseAnalysisEigenValuesImageFilter.hxx"
#endif

#endif
