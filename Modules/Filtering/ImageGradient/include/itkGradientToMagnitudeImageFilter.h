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
#ifndef __itkGradientToMagnitudeImageFilter_h
#define __itkGradientToMagnitudeImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
/** \class GradientToMagnitudeImageFilter
 *
 * \brief Take an image of vectors as input and produce an image with the
 *  magnitude of those vectors.
 *
 * The filter expects the input image pixel type to be a vector and
 * the output image pixel type to be a scalar.
 *
 * This filter assumes that the PixelType of the input image is a VectorType
 * that provides a GetNorm() method.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 * \ingroup ITK-ImageGradient
 */

namespace Functor
{
template< class TInput, class TOutput >
class GradientMagnitude
{
public:
  GradientMagnitude() {}
  ~GradientMagnitude() {}

  bool operator!=(const GradientMagnitude &) const
  {
    return false;
  }

  bool operator==(const GradientMagnitude & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( A.GetNorm() );
  }
};
}

template< class TInputImage, class TOutputImage >
class ITK_EXPORT GradientToMagnitudeImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::GradientMagnitude< typename TInputImage::PixelType,
                                                       typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef GradientToMagnitudeImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::GradientMagnitude< typename TInputImage::PixelType,
                                typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GradientToMagnitudeImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  /** End concept checking */
#endif
protected:
  GradientToMagnitudeImageFilter() {}
  virtual ~GradientToMagnitudeImageFilter() {}
private:
  GradientToMagnitudeImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented
};
} // end namespace itk

#endif
