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
#ifndef itkPhaseCorrelationOperator_h
#define itkPhaseCorrelationOperator_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"
#include <complex>

namespace itk
{

/** \class PhaseCorrelationOperator
 *  \brief Computes the spectrum ratio in phase correlation method.
 *
 *  The class is templated over the type of the real-valued pixel it will be
 *  operating on and the image dimension.
 *
 *  This frequency ratio is computed at every index of output correlation
 *  surface.
 *
 *  As this is a convenient place for band-pass filtering of the input images.
 *  the interface for that is provided by SetBandPassControlPoints() method.
 *
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 * \ingroup Montage
 */
template < typename TRealPixel, unsigned int VImageDimension >
class ITK_TEMPLATE_EXPORT PhaseCorrelationOperator :
  public ImageToImageFilter<
      Image< std::complex< TRealPixel >, VImageDimension >,
      Image< std::complex< TRealPixel >, VImageDimension > >
{

public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhaseCorrelationOperator);

  using Self = PhaseCorrelationOperator;
  using Superclass = ImageToImageFilter<Image< std::complex< TRealPixel >, VImageDimension >, Image< std::complex< TRealPixel >, VImageDimension > >;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using BandPassPointsType = FixedArray<TRealPixel, 4>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhaseCorrelationOperator, ImageToImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Set/Get control points for band-pass filtering.
   * These points represent radial distances from constant component.
   * The values of these points should be specified on 0.0-1.0 scale,
   * 0.0 relating to constant frequency component, and 1.0 relating 
   * to highest frequency along all dimensions.
   * Point values should be monotonitcally increasing:
   * 0 <= p[0] < p[1] < p[2] < p[3] <= 1.0
   * All frequencies below p[0] and above p[3] are completely removed.
   * All frequencies between p[1] and p[2] are completely kept.
   * Frequencies between p[0] and p[1]; and between p[2] and p[3]
   * are transition zones with linearly increasing (p0-p1)
   * and decreasing (p2-p3) coefficient of influence. Function plot:
   *   ^
   *  1|             _________
   *   |            /         \
   *   |           /           \
   *   |          /             \
   *  0|    *----*---*-------*---*----*
   * coef  0.0   p0  p1      p2  p3  1.0
   */
  itkSetMacro(BandPassControlPoints, BandPassPointsType);
  virtual void SetBandPassControlPoints(const BandPassPointsType& points);
  itkGetConstMacro(BandPassControlPoints, BandPassPointsType);

  /** Image type aliases. */
  using PixelType = TRealPixel;
  using ComplexType = std::complex<PixelType>;
  using ImageType = Image< ComplexType, ImageDimension >;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Connect the fixed image. */
  void SetFixedImage( ImageType * fixedImage );

  /** Connect the moving image. */
  void SetMovingImage( ImageType * movingImage );

protected:
  PhaseCorrelationOperator();
  virtual ~PhaseCorrelationOperator() {};
  void PrintSelf(std::ostream& os, Indent indent) const override;

  /** PhaseCorrelationOperator produces an image which is a different
   * resolution and with a different pixel spacing than its input
   * images. */
  void GenerateOutputInformation() override;

  /** PhaseCorrelationOperator needs a larger input requested region than the
   *  output requested region. */
  void GenerateInputRequestedRegion() override;
  void EnlargeOutputRequestedRegion(DataObject *output) override;

  /** PhaseCorrelationOperator can be implemented as a multithreaded filter.
   *  This method performs the computation. */
  void DynamicThreadedGenerateData( const OutputImageRegionType& outputRegionForThread ) override;

protected:
  BandPassPointsType m_BandPassControlPoints;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPhaseCorrelationOperator.hxx"
#endif

#endif
