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
#ifndef __itkVnlFFTRealToComplexConjugateImageFilter_h
#define __itkVnlFFTRealToComplexConjugateImageFilter_h
#include "itkFFTRealToComplexConjugateImageFilter.h"
#include <complex>
namespace itk
{
/** \class VnlFFTRealToComplexConjugateImageFilter
 *
 * \brief VNL based forward Fast Fourier Transform.
 *
 * Input image must be a power of two in all directions.
 *
 * \sa ConstantPadImageFilter
 * \sa CropImageFilter
 *
 * \ingroup FourierTransform
 *
 * \sa FFTRealToComplexConjugateImageFilter
 */
template< class TPixel, unsigned int VDimension = 3 >
class VnlFFTRealToComplexConjugateImageFilter:
  public FFTRealToComplexConjugateImageFilter< TPixel, VDimension >
{
public:
  /** Standard class typedefs. */
  typedef VnlFFTRealToComplexConjugateImageFilter                    Self;
  typedef FFTRealToComplexConjugateImageFilter< TPixel, VDimension > Superclass;
  typedef SmartPointer< Self >                                       Pointer;
  typedef SmartPointer< const Self >                                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlFFTRealToComplexConjugateImageFilter,
               FFTRealToComplexConjugateImageFilter);

  //
  // these should be defined in every FFT filter class
  virtual void GenerateData();  // generates output from input

  virtual bool FullMatrix();

protected:
  VnlFFTRealToComplexConjugateImageFilter() {}
  ~VnlFFTRealToComplexConjugateImageFilter() {}
  ///** Method to check if an array dimension is legal for PFA FFT */
  bool Legaldim(int n);

private:
  inline std::complex< TPixel > myConj(const std::complex< TPixel > & __z)
  {
    return std::complex< TPixel >( __z.real(), -__z.imag() );
  }

  VnlFFTRealToComplexConjugateImageFilter(const Self &); //purposely not
                                                         // implemented
  void operator=(const Self &);                          //purposely not
                                                         // implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVnlFFTRealToComplexConjugateImageFilter.txx"
#endif

#endif
