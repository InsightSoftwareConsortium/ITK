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
#ifndef itkFFTShiftImageFilter_h
#define itkFFTShiftImageFilter_h

#include "itkCyclicShiftImageFilter.h"

namespace itk
{
/** \class FFTShiftImageFilter
 * \brief Shift the zero-frequency components of a Fourier transform
 * to the center of the image.
 *
 * The Fourier transform produces an image where the zero frequency
 * components are in the corner of the image, making it difficult to
 * understand. This filter shifts the component to the center of the
 * image.
 *
 * \note For images with an odd-sized dimension, applying this filter
 * twice will not produce the same image as the original one without
 * using SetInverse(true) on one (and only one) of the two filters.
 *
 * https://hdl.handle.net/1926/321
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ForwardFFTImageFilter, InverseFFTImageFilter
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT FFTShiftImageFilter :
  public CyclicShiftImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef FFTShiftImageFilter                                 Self;
  typedef CyclicShiftImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef TOutputImage                           OutputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;
  typedef typename OutputImageType::IndexType    IndexType;
  typedef typename OutputImageType::SizeType     SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FFTShiftImageFilter, CyclicShiftImageFilter);

  /** Set/Get whether the filter must invert the transform or not.
   * This option has no effect if none of the size of the input image is even,
   * but is required to restore the original image if at least one
   * of the dimensions has an odd size. */
  itkSetMacro(Inverse, bool);
  itkGetConstReferenceMacro(Inverse, bool);
  itkBooleanMacro(Inverse);

protected:
  FFTShiftImageFilter();
  ~FFTShiftImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Override GenerateData method to set some parameters in the
   * superclass. */
  void  GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTShiftImageFilter);

  bool m_Inverse;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTShiftImageFilter.hxx"
#endif

#endif
