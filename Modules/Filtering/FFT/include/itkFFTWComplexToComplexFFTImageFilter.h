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
#ifndef itkFFTWComplexToComplexFFTImageFilter_h
#define itkFFTWComplexToComplexFFTImageFilter_h

#include "itkComplexToComplexFFTImageFilter.h"
#include "itkFFTWCommon.h"


namespace itk
{
/** \class FFTWComplexToComplexFFTImageFilter
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
 * https://hdl.handle.net/1926/326
 * http://www.insight-journal.org/browse/publication/128
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
template< typename TImage >
class ITK_TEMPLATE_EXPORT FFTWComplexToComplexFFTImageFilter:
  public ComplexToComplexFFTImageFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef FFTWComplexToComplexFFTImageFilter       Self;
  typedef ComplexToComplexFFTImageFilter< TImage > Superclass;
  typedef SmartPointer< Self >                     Pointer;
  typedef SmartPointer< const Self >               ConstPointer;

  typedef TImage                               ImageType;
  typedef typename ImageType::PixelType        PixelType;
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  // the proxy type is a wrapper for the fftw API
  // since the proxy is only defined over double and float,
  // trying to use any other pixel type is inoperative, as
  // is trying to use double if only the float FFTW version is
  // configured in, or float if only double is configured.
  //
  typedef typename fftw::Proxy< typename PixelType::value_type > FFTWProxyType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTWComplexToComplexFFTImageFilter,
               ComplexToComplexFFTImageFilter);

  itkStaticConstMacro(ImageDimension, unsigned int,
                      ImageType::ImageDimension);

  /** Image type typedef support. */
  typedef typename ImageType::SizeType ImageSizeType;

  /**
   * Set/Get the behavior of wisdom plan creation. The default is
   * provided by FFTWGlobalConfiguration::GetPlanRigor().
   *
   * The parameter is one of the FFTW planner rigor flags FFTW_ESTIMATE, FFTW_MEASURE,
   * FFTW_PATIENT, FFTW_EXHAUSTIVE provided by FFTWGlobalConfiguration.
   * /sa FFTWGlobalConfiguration
   */
  virtual void SetPlanRigor( const int & value )
  {
    // use that method to check the value
    FFTWGlobalConfiguration::GetPlanRigorName( value );
    if( m_PlanRigor != value )
      {
      m_PlanRigor = value;
      this->Modified();
      }
  }
  itkGetConstReferenceMacro( PlanRigor, int );
  void SetPlanRigor( const std::string & name )
  {
    this->SetPlanRigor( FFTWGlobalConfiguration::GetPlanRigorValue( name ) );
  }

protected:
  FFTWComplexToComplexFFTImageFilter();
  virtual ~FFTWComplexToComplexFFTImageFilter() {}

  virtual void UpdateOutputData(DataObject *output) ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            ThreadIdType threadId ) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTWComplexToComplexFFTImageFilter);

  bool m_CanUseDestructiveAlgorithm;

  int m_PlanRigor;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTWComplexToComplexFFTImageFilter.hxx"
#endif

#endif //itkFFTWComplexToComplexFFTImageFilter_h
