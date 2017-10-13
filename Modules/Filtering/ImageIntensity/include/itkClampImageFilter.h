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
#ifndef itkClampImageFilter_h
#define itkClampImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

namespace Functor
{

/** \class Clamp
 *
 * \brief Functor used to clamp a value to a specified range.
 *
 * Default range corresponds to the range supported by the
 * output type.
 *
 * It is templated over an input and an output-type in order
 * to be able to cast the output value.
 *
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput = TInput >
class ITK_TEMPLATE_EXPORT Clamp
{
public:

  typedef Clamp   Self;

  typedef TInput  InputType;
  typedef TOutput OutputType;

  /** Creates the functor and initializes the bounds to the
   * output-type limits.
   */
  Clamp();

  ~Clamp();

  OutputType GetLowerBound() const;
  OutputType GetUpperBound() const;

  /** Set the bounds of the range in which the data will be clamped.
   * If the lower-bound is greater than the upper-bound,
   * an itk::ExceptionObject will be thrown.
   */
  void SetBounds( const OutputType lowerBound, const OutputType upperBound);

  bool operator!=( const Self & other ) const;
  bool operator==( const Self & other ) const;

  OutputType operator()( const InputType & A ) const;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible< InputType, OutputType >));
  itkConceptMacro(InputConvertibleToDoubleCheck,
    (Concept::Convertible< InputType, double > ));
  itkConceptMacro(DoubleLessThanComparableToOutputCheck,
    (Concept::LessThanComparable< double, OutputType > ));
  itkConceptMacro(DoubleGreaterThanComparableToOutputCheck,
    (Concept::GreaterThanComparable< double, OutputType > ));
#endif

private:
  OutputType m_LowerBound;
  OutputType m_UpperBound;
};


template< typename TInput, typename TOutput >
inline
typename Clamp< TInput, TOutput >::OutputType
Clamp< TInput, TOutput >
::operator()( const InputType & A ) const
  {
  const double dA = static_cast< double >( A );

  if ( dA < m_LowerBound )
    {
    return m_LowerBound;
    }

  if ( dA > m_UpperBound )
    {
    return m_UpperBound;
    }

  return static_cast< OutputType >( A );
  }

} // end namespace Functor


/** \class ClampImageFilter
 *
 * \brief Casts input pixels to output pixel type and clamps the
 * output pixel values to a specified range.
 *
 * Default range corresponds to the range supported by the
 * pixel type of the output image.
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \sa UnaryFunctorImageFilter
 * \sa CastImageFilter
 *
 * \wiki
 * \wikiexample{ImageProcessing/ClampImageFilter,Cast an image from one type to another but clamp to the output value range}
 * \endwiki
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ClampImageFilter :
  public UnaryFunctorImageFilter< TInputImage,TOutputImage,
                                  Functor::Clamp< typename TInputImage::PixelType,
                                                  typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef ClampImageFilter               Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::Clamp< typename TInputImage::PixelType,
                                                   typename TOutputImage::PixelType > >
                                         Superclass;

  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ClampImageFilter, UnaryFunctorImageFilter);

  OutputPixelType GetLowerBound() const;
  OutputPixelType GetUpperBound() const;

  /** Set the bounds of the range in which the data will be clamped.
   * If the lower-bound is greater than the upper-bound,
   * an itk::ExceptionObject will be thrown.
   */
  void SetBounds(const OutputPixelType lowerBound, const OutputPixelType upperBound);

protected:
  ClampImageFilter();
  virtual ~ClampImageFilter() ITK_OVERRIDE {}

  void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ClampImageFilter);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkClampImageFilter.hxx"
#endif

#endif
