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
#ifndef __itkRealAndImaginaryToComplexImageFilter_h
#define __itkRealAndImaginaryToComplexImageFilter_h

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{
/** \class RealAndImaginaryToComplexImageFilter
 * \brief Implements pixel-wise conversion of real and imaginary data
 * into complex voxels.
 *
 * This filter is parametrized over the types of the two
 * input images and the type of the output image.
 *
 * The filter expect all images to have the same dimension
 * (e.g. all 2D, or all 3D, or all ND)
 *
 * \ingroup IntensityImageFilters Multithreaded
 *
 * \weakgroup FourierTransform
 *
 * \author Simon K. Warfield simon.warfield@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by Grant
 * Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * This class was taken from the Insight Journal paper:
 * http://insight-journal.org/midas/handle.php?handle=1926/326
 *
 * \sa MagnitudeAndPhaseToComplexImageFilter
 */

namespace Functor
{
template< class TInput1, class TInput2, class TOutputValueType >
class RealAndImaginaryToComplex
{
public:
  RealAndImaginaryToComplex() {}
  ~RealAndImaginaryToComplex() {}
  bool operator!=(const RealAndImaginaryToComplex &) const
  {
    return false;
  }

  bool operator==(const RealAndImaginaryToComplex & other) const
  {
    return !( *this != other );
  }

  inline std::complex< TOutputValueType > operator()(const TInput1 & A, const TInput2 & B) const
  {
    return std::complex< TOutputValueType >( static_cast< TOutputValueType >( A ),
                                    static_cast< TOutputValueType >( B ) );
  }
};
}

template< class TInputImage1,
          class TInputImage2 = TInputImage1,
          class TOutputImage = itk::Image< std::complex< typename TInputImage1::PixelType>,
                                           TInputImage1::ImageDimension > >
class ITK_EXPORT RealAndImaginaryToComplexImageFilter:
  public BinaryFunctorImageFilter<
    TInputImage1,
    TInputImage2,
    TOutputImage,
    Functor::RealAndImaginaryToComplex<
      typename TInputImage1::PixelType,
      typename TInputImage2::PixelType,
      typename TOutputImage::PixelType::value_type > >
{
public:
  /** Standard class typedefs. */
  typedef RealAndImaginaryToComplexImageFilter Self;

  typedef BinaryFunctorImageFilter<
  TInputImage1,
    TInputImage2,
    TOutputImage,
    Functor::RealAndImaginaryToComplex<
      typename TInputImage1::PixelType,
      typename TInputImage2::PixelType,
      typename TOutputImage::PixelType::value_type > >     Superclass;

  typedef typename TInputImage1::PixelType InputPixel1Type;
  typedef typename TInputImage2::PixelType InputPixel2Type;
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RealAndImaginaryToComplexImageFilter, BinaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( Input1ConvertibleToDoubleCheck,
                   ( Concept::Convertible< InputPixel1Type, double > ) );
  itkConceptMacro( Input2ConvertibleToDoubleCheck,
                   ( Concept::Convertible< InputPixel2Type, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, OutputPixelType > ) );
  /** End concept checking */
#endif
protected:
  RealAndImaginaryToComplexImageFilter() {}
  virtual ~RealAndImaginaryToComplexImageFilter() {}
private:
  RealAndImaginaryToComplexImageFilter(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented
};
} // end namespace itk

#endif
