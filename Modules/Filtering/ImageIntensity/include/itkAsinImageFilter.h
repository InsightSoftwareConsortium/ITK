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
#ifndef __itkAsinImageFilter_h
#define __itkAsinImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "vnl/vnl_math.h"

namespace itk
{
/** \class AsinImageFilter
 * \brief Computes the vcl_asin(x) pixel-wise
 *
 * This filter is templated over the pixel type of the input image
 * and the pixel type of the output image.
 *
 * The filter will walk over all the pixels in the input image, and for
 * each one of them it will do the following:
 *
 * - cast the pixel value to \c double,
 * - apply the \c vcl_asin() function to the \c double value
 * - cast the \c double value resulting from \c vcl_asin() to the pixel type of the output image
 * - store the casted value into the output image.
 *
 * The filter expect both images to have the same dimension (e.g. both 2D, or both 3D, or both ND)
 *
 * \ingroup IntensityImageFilters   Multithreaded
 * \ingroup ITK-ImageIntensity
 */
namespace Functor
{
template< class TInput, class TOutput >
class Asin
{
public:
  Asin() {}
  ~Asin() {}
  bool operator!=(const Asin &) const
  {
    return false;
  }

  bool operator==(const Asin & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >(
             vcl_asin(
               static_cast< double >( A )
               )
             );
  }
};
}
template< class TInputImage, class TOutputImage >
class ITK_EXPORT AsinImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::Asin<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef AsinImageFilter Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::Asin<
                                     typename TInputImage::PixelType,
                                     typename TOutputImage::PixelType >
                                   >                                 Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AsinImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputConvertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  /** End concept checking */
#endif
protected:
  AsinImageFilter() {}
  virtual ~AsinImageFilter() {}
private:
  AsinImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented
};
} // end namespace itk

#endif
