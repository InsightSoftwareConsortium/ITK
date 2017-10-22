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
#ifndef itkConstrainedValueAdditionImageFilter_h
#define itkConstrainedValueAdditionImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
namespace Functor
{
/**
 * \class ConstrainedValueAddition
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput1, typename TInput2, typename TOutput >
class ConstrainedValueAddition
{
public:
  ConstrainedValueAddition() {}
  ~ConstrainedValueAddition() {}
  bool operator!=(const ConstrainedValueAddition &) const
  {
    return false;
  }

  bool operator==(const ConstrainedValueAddition & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput1 & A,
                            const TInput2 & B) const
  {
    const double dA = static_cast< double >( A );
    const double dB = static_cast< double >( B );
    const double add = dA + dB;
    const double cadd1 = ( add < NumericTraits< TOutput >::max() ) ?
                         add : NumericTraits< TOutput >::max();
    const double cadd2 = ( cadd1 > NumericTraits< TOutput >::NonpositiveMin() ) ?
                         cadd1 : NumericTraits< TOutput >::NonpositiveMin();

    return static_cast< TOutput >( cadd2 );
  }
};
}
/** \class ConstrainedValueAdditionImageFilter
 * \brief Implements pixel-wise the computation of constrained value addition.
 *
 * This filter is templated over the types of the two
 * input images and the type of the output image.
 *
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The filter will walk over all the pixels in the two input images, and for
 * each one of them it will do the following:
 *
 * \li cast the input 1 pixel value to \c double
 * \li cast the input 2 pixel value to \c double
 * \li compute the addition of the two pixel values
 * \li compute the constrained value (constrained to be between the
 *   NonpositiveMin and max of the output pixel type)
 * \li cast the \c double value resulting from \c the constrained value
 *   to the pixel type of the output image
 * \li store the cast value into the output image.
 *
 * The filter expects all images to have the same dimension
 * (e.g. all 2D, or all 3D, or all ND)
 *
 * \author Lino Ramirez. Dept. of Electrical and Computer
 * Engineering. University of Alberta. Canada
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage1, typename TInputImage2, typename TOutputImage >
class ConstrainedValueAdditionImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::ConstrainedValueAddition<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ConstrainedValueAdditionImageFilter Self;
  typedef BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                                    Functor::ConstrainedValueAddition<
                                      typename TInputImage1::PixelType,
                                      typename TInputImage2::PixelType,
                                      typename TOutputImage::PixelType > >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ConstrainedValueAdditionImageFilter,
               BinaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( Input1ConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage1::PixelType, double > ) );
  itkConceptMacro( Input2ConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage2::PixelType, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCastCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  itkConceptMacro( DoubleLessThanOutputCheck,
                   ( Concept::LessThanComparable< double, typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  ConstrainedValueAdditionImageFilter() {}
  virtual ~ConstrainedValueAdditionImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConstrainedValueAdditionImageFilter);
};
} // end namespace itk

#endif
