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
#include "itkComplexToComplexFFTImageFilter.h"

#ifndef itkFFTWComplexToComplexFFTImageFilter_h
#  define itkFFTWComplexToComplexFFTImageFilter_h

#  include "itkFFTWCommon.h"


namespace itk
{
/**
 *\class FFTWComplexToComplexFFTImageFilter
 *
 *  \brief Implements an API to enable the Fourier transform or the inverse
 *  Fourier transform of images with complex valued voxels to be computed using
 *  either FFTW from MIT or the FFTW interface in Intel MKL.
 *
 * This filter is multithreaded and supports input images with sizes which are not
 * a power of two.
 *
 * This code was contributed in the Insight Journal paper:
 * "FFT Complex to Complex filters and helper classes"
 * by Warfield S.
 * https://www.insight-journal.org/browse/publication/128
 *
 * \author Simon K. Warfield simon.warfield\@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by
 * Grant Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * \ingroup FourierTransform
 * \ingroup MultiThreaded
 * \ingroup ITKFFT
 *
 * \sa FFTWGlobalConfiguration
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT FFTWComplexToComplexFFTImageFilter : public ComplexToComplexFFTImageFilter<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTWComplexToComplexFFTImageFilter);

  /** Standard class type aliases. */
  using Self = FFTWComplexToComplexFFTImageFilter;
  using Superclass = ComplexToComplexFFTImageFilter<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using ImageType = TImage;
  using PixelType = typename ImageType::PixelType;
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  // the proxy type is a wrapper for the fftw API
  // since the proxy is only defined over double and float,
  // trying to use any other pixel type is inoperative, as
  // is trying to use double if only the float FFTW version is
  // configured in, or float if only double is configured.
  //
  using FFTWProxyType = typename fftw::Proxy<typename PixelType::value_type>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWComplexToComplexFFTImageFilter, ComplexToComplexFFTImageFilter);

  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  /** Image type type alias support */
  using ImageSizeType = typename ImageType::SizeType;

  /**
   * Set/Get the behavior of wisdom plan creation. The default is
   * provided by FFTWGlobalConfiguration::GetPlanRigor().
   *
   * The parameter is one of the FFTW planner rigor flags FFTW_ESTIMATE, FFTW_MEASURE,
   * FFTW_PATIENT, FFTW_EXHAUSTIVE provided by FFTWGlobalConfiguration.
   *
   * This is not used when ITK_USE_CUFFTW is enabled.
   *
   * /sa FFTWGlobalConfiguration
   */
  virtual void
  SetPlanRigor(const int & value)
  {
#  ifndef ITK_USE_CUFFTW
    // use that method to check the value
    FFTWGlobalConfiguration::GetPlanRigorName(value);
#  endif
    if (m_PlanRigor != value)
    {
      m_PlanRigor = value;
      this->Modified();
    }
  }
  itkGetConstReferenceMacro(PlanRigor, int);
  void
  SetPlanRigor(const std::string & name)
  {
#  ifndef ITK_USE_CUFFTW
    this->SetPlanRigor(FFTWGlobalConfiguration::GetPlanRigorValue(name));
#  endif
  }

protected:
  FFTWComplexToComplexFFTImageFilter();
  ~FFTWComplexToComplexFFTImageFilter() override = default;

  void
  UpdateOutputData(DataObject * output) override;

  void
  BeforeThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool m_CanUseDestructiveAlgorithm;

  int m_PlanRigor;
};


} // namespace itk

#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkFFTWComplexToComplexFFTImageFilter.hxx"
#  endif

#endif // itkFFTWComplexToComplexFFTImageFilter_h
