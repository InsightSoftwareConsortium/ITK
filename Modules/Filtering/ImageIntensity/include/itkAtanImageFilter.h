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
#ifndef itkAtanImageFilter_h
#define itkAtanImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Atan
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput >
class Atan
{
public:
  Atan() {}
  ~Atan() {}
  bool operator!=(const Atan &) const
  {
    return false;
  }

  bool operator==(const Atan & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( std::atan( static_cast< double >( A ) ) );
  }
};
}
/** \class AtanImageFilter
 * \brief Computes the one-argument inverse tangent of each pixel.
 *
 * This filter is templated over the pixel type of the input image
 * and the pixel type of the output image.
 *
 * The filter walks over all the pixels in the input image, and for
 * each pixel does the following:
 *
 * \li cast the pixel value to \c double,
 * \li apply the \c std::atan() function to the \c double value,
 * \li cast the \c double value resulting from \c std::atan() to the pixel
 * type of the output image,
 * \li store the cast value into the output image.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class AtanImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::Atan<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef AtanImageFilter Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::Atan<
                                     typename TInputImage::PixelType,
                                     typename TOutputImage::PixelType >
                                   >                                 Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AtanImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  AtanImageFilter() {}
  virtual ~AtanImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AtanImageFilter);
};
} // end namespace itk

#endif
