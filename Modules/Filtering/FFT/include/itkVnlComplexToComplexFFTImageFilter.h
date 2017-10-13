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
#ifndef itkVnlComplexToComplexFFTImageFilter_h
#define itkVnlComplexToComplexFFTImageFilter_h

#include "itkComplexToComplexFFTImageFilter.h"

namespace itk
{
/** \class VnlComplexToComplexFFTImageFilter
 *
 * \brief VNL based complex to complex Fast Fourier Transform.
 *
 * This filter requires input images with sizes which are a power of two.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 *
 * \sa ComplexToComplexFFTImageFilter
 * \sa FFTWComplexToComplexFFTImageFilter
 * \sa VnlForwardFFTImageFilter
 * \sa VnlInverseFFTImageFilter
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT VnlComplexToComplexFFTImageFilter:
  public ComplexToComplexFFTImageFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef VnlComplexToComplexFFTImageFilter        Self;
  typedef ComplexToComplexFFTImageFilter< TImage > Superclass;
  typedef SmartPointer< Self >                     Pointer;
  typedef SmartPointer< const Self >               ConstPointer;

  typedef TImage                               ImageType;
  typedef typename ImageType::PixelType        PixelType;
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlComplexToComplexFFTImageFilter,
               ComplexToComplexFFTImageFilter);

  itkStaticConstMacro(ImageDimension, unsigned int,
                      ImageType::ImageDimension);

protected:
  VnlComplexToComplexFFTImageFilter();
  virtual ~VnlComplexToComplexFFTImageFilter() ITK_OVERRIDE {}

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread, ThreadIdType itkNotUsed(threadId) ) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VnlComplexToComplexFFTImageFilter);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlComplexToComplexFFTImageFilter.hxx"
#endif

#endif //itkVnlComplexToComplexFFTImageFilter_h
