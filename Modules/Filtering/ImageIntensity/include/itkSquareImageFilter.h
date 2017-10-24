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
#ifndef itkSquareImageFilter_h
#define itkSquareImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
/** \class SquareImageFilter
 * \brief Computes the square of the intensity values pixel-wise
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/SquareImageFilter,Square every pixel in an image}
 * \endwiki
 */

namespace Functor
{
template< typename TInput, typename TOutput >
class Square
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  Square() {}
  ~Square() {}
  bool operator!=(const Square &) const
  {
    return false;
  }

  bool operator==(const Square & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    const RealType ra = static_cast< RealType >( A );

    return static_cast< TOutput >( ra * ra );
  }
};
}
template< typename TInputImage, typename TOutputImage >
class SquareImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::Square< typename TInputImage::PixelType,
                                            typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef SquareImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::Square< typename TInputImage::PixelType,
                     typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SquareImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType > ) );
  itkConceptMacro( RealTypeMultiplyOperatorCheck,
                   ( Concept::MultiplyOperator< typename NumericTraits< typename TInputImage::PixelType >::RealType > ) );
  // End concept checking
#endif

protected:
  SquareImageFilter() {}
  virtual ~SquareImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SquareImageFilter);
};
} // end namespace itk

#endif
