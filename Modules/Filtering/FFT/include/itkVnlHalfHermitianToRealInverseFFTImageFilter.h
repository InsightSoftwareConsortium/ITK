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
#ifndef itkVnlHalfHermitianToRealInverseFFTImageFilter_h
#define itkVnlHalfHermitianToRealInverseFFTImageFilter_h

#include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkVnlFFTCommon.h"

#include "itkImage.h"
#include "vnl/algo/vnl_fft_base.h"

namespace itk
{
/** \class VnlHalfHermitianToRealInverseFFTImageFilter
 *
 * \brief VNL-based reverse Fast Fourier Transform.
 *
 * The input image size must be a multiple of combinations of 2s, 3s,
 * and/or 5s in all dimensions.
 *
 * \ingroup FourierTransform
 *
 * \sa HalfHermitianToRealInverseFFTImageFilter
 * \ingroup ITKFFT
 *
 */
template< typename TInputImage, typename TOutputImage=Image< typename TInputImage::PixelType::value_type, TInputImage::ImageDimension> >
class ITK_TEMPLATE_EXPORT VnlHalfHermitianToRealInverseFFTImageFilter:
  public HalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                              InputImageType;
  typedef typename InputImageType::PixelType       InputPixelType;
  typedef typename InputImageType::SizeType        InputSizeType;
  typedef typename InputImageType::IndexType       InputIndexType;
  typedef typename InputImageType::SizeValueType   InputSizeValueType;
  typedef TOutputImage                             OutputImageType;
  typedef typename OutputImageType::PixelType      OutputPixelType;
  typedef typename OutputImageType::IndexType      OutputIndexType;
  typedef typename OutputImageType::SizeType       OutputSizeType;
  typedef typename OutputImageType::IndexValueType OutputIndexValueType;

  typedef VnlHalfHermitianToRealInverseFFTImageFilter                           Self;
  typedef HalfHermitianToRealInverseFFTImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                                  Pointer;
  typedef SmartPointer< const Self >                                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlHalfHermitianToRealInverseFFTImageFilter,
               HalfHermitianToRealInverseFFTImageFilter);

  /** Extract the dimensionality of the images. They must be the
   * same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  SizeValueType GetSizeGreatestPrimeFactor() const ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( PixelUnsignedIntDivisionOperatorsCheck,
                   ( Concept::DivisionOperators< OutputPixelType, unsigned int > ) );
  itkConceptMacro( ImageDimensionsMatchCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  // End concept checking
#endif

protected:
  VnlHalfHermitianToRealInverseFFTImageFilter()  {}
  virtual ~VnlHalfHermitianToRealInverseFFTImageFilter() ITK_OVERRIDE {}

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VnlHalfHermitianToRealInverseFFTImageFilter);

  typedef vnl_vector< InputPixelType  > SignalVectorType;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.hxx"
#endif

#endif
