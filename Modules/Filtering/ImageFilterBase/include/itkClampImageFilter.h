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
#ifndef __itkClampImageFilter_h
#define __itkClampImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkProgressReporter.h"

namespace itk
{

/** \class ClampImageFilter
 *
 * \brief Casts input pixels to output pixel type and clamps the
 * output pixel values to the range supported by the output pixel type.
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters
 * \ingroup Multithreaded
 * \ingroup ITK-ImageFilterBase
 *
 * \sa UnaryFunctorImageFilter
 * \sa CastImageFilter
 *
 */
namespace Functor {

template< class TInput, class TOutput>
class Clamp
{
public:
  Clamp() {};
  virtual ~Clamp() {};
  bool operator!=( const Clamp & ) const
  {
    return false;
  }
  bool operator==( const Clamp & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A ) const
  {
    double dA = static_cast< double >( A );
    double typeMin =
      static_cast< double >( NumericTraits< TOutput >::NonpositiveMin() );
    double typeMax =
      static_cast< double >( NumericTraits< TOutput >::max() );

    if ( dA < typeMin )
      {
      return NumericTraits< TOutput >::NonpositiveMin();
      }

    if ( dA > typeMax )
      {
      return NumericTraits< TOutput >::max();
      }

    return static_cast< TOutput >( A );
  }
};
}

template <class TInputImage, class TOutputImage>
class ITK_EXPORT ClampImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage,
                        Functor::Clamp<
  typename TInputImage::PixelType,
  typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef ClampImageFilter               Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                        Functor::Clamp<
                          typename TInputImage::PixelType,
                          typename TOutputImage::PixelType > >
                                        Superclass;

  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ClampImageFilter, UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible< typename TInputImage::PixelType,
                           typename TOutputImage::PixelType >));
  itkConceptMacro(InputConvertibleToDoubleCheck,
    (Concept::Convertible< typename TInputImage::PixelType,
                           double > ));
  itkConceptMacro(OutputConvertibleToDoubleCheck,
    (Concept::Convertible< typename TOutputImage::PixelType,
                           double > ));
  /** End concept checking */
#endif

protected:
  ClampImageFilter() {}
  virtual ~ClampImageFilter() {}

  void GenerateData()
    {
    if( this->GetInPlace() && this->CanRunInPlace() )
      {
      // Nothing to do, so avoid iterating over all the pixels for
      // nothing! Allocate the output, generate a fake progress and exit.
      this->AllocateOutputs();
      ProgressReporter progress(this, 0, 1);
      return;
      }
    Superclass::GenerateData();
    }

private:
  ClampImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&);   //purposely not implemented

};


} // end namespace itk

#endif
