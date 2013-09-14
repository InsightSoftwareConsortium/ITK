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
#ifndef __itkFFTComplexToComplexImageFilter_h
#define __itkFFTComplexToComplexImageFilter_h

#include "itkImageToImageFilter.h"
#include <complex>

namespace itk
{
/** \class FFTComplexToComplexImageFilter
 *
 * \brief Implements an API to enable the Fourier transform or the inverse
 * Fourier transform of images with complex valued voxels to be computed.
 *
 * \ingroup FourierTransform
 *
 * \author Simon K. Warfield simon.warfield@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by
 * Grant Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * This class was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/326
 *
 * \ingroup FourierTransform
 *
 * \sa ForwardFFTImageFilter
 * \ingroup ITKReview
 */
template< typename TImage >
class FFTComplexToComplexImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:
  /** Input and output image types. */
  typedef TImage ImageType;
  typedef TImage InputImageType;
  typedef TImage OutputImageType;

  /** Standard class typedefs. */
  typedef FFTComplexToComplexImageFilter                        Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTComplexToComplexImageFilter, ImageToImageFilter);

  /** Customized object creation methods that support configuration-based
    * selection of FFT implementation.
    *
    * Default implementation is FFTW.
    */
  static Pointer New(void);

  /** Transform Direction */
  typedef enum {
    DIRECT = 1,
    INVERSE
    }                                             TransformDirectionType;

  /** Image type typedef support. */
  typedef typename ImageType::SizeType ImageSizeType;

  /** Set/Get the direction in which the transform will be applied.
   * By selecting DIRECT, this filter will perform a direct Fourier Transform,
   * By selecting INVERSE, this filter will perform an inverse Fourier Transform,
   */
  itkSetMacro(TransformDirection, TransformDirectionType);
  itkGetConstMacro(TransformDirection, TransformDirectionType);

protected:
  FFTComplexToComplexImageFilter() {}
  virtual ~FFTComplexToComplexImageFilter(){}

  /** methods needed for the image filter pipeline */
  virtual void GenerateOutputInformation(); // figure out allocation for output
                                            // image

  virtual void GenerateInputRequestedRegion();

  virtual bool FullMatrix() = 0; // must be implemented in child

private:
  FFTComplexToComplexImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  TransformDirectionType m_TransformDirection;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTComplexToComplexImageFilter.hxx"
#endif

#endif
