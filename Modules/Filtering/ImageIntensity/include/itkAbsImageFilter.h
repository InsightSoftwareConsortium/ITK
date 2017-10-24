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
#ifndef itkAbsImageFilter_h
#define itkAbsImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
namespace Functor
{
/** \class Abs
 * \brief Computes the absolute value of a pixel.
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput >
class Abs
{
public:
  Abs() {}
  ~Abs() {}
  bool operator!=(const Abs &) const
  {
    return false;
  }

  bool operator==(const Abs & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast<TOutput>( itk::Math::abs( A ) );
  }
};
}

/** \class AbsImageFilter
 * \brief Computes the absolute value of each pixel.
 *
 * itk::Math::abs() is used to perform the computation.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/AbsImageFilter,Compute the absolute value of an image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class AbsImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::Abs<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef AbsImageFilter Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::Abs< typename TInputImage::PixelType,
                                                 typename TOutputImage::PixelType > >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AbsImageFilter,
               UnaryFunctorImageFilter);

  typedef typename TInputImage::PixelType     InputPixelType;
  typedef typename TOutputImage::PixelType    OutputPixelType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( ConvertibleCheck,
                   ( Concept::Convertible< InputPixelType, OutputPixelType > ) );
  itkConceptMacro( InputGreaterThanIntCheck,
                   ( Concept::GreaterThanComparable< InputPixelType, InputPixelType > ) );
  // End concept checking
#endif

protected:
  AbsImageFilter() {}
  virtual ~AbsImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AbsImageFilter);
};
} // end namespace itk

#endif
