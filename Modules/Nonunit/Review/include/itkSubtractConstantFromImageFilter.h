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
#ifndef __itkSubtractConstantFromImageFilter_h
#define __itkSubtractConstantFromImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class SubtractConstantFromImageFilter
 *
 * \brief Subract a constant from all input pixels.
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/510
 *
 * \ingroup IntensityImageFilters  Multithreaded
 * \sa UnaryFunctorImageFilter
 * \ingroup ITK-Review
 */
namespace Functor
{
template< class TInput, class TConstant, class TOutput >
class SubtractConstantFrom
{
public:
  SubtractConstantFrom():m_Constant(NumericTraits< TConstant >::One) {}
  ~SubtractConstantFrom() {}
  bool operator!=(const SubtractConstantFrom & other) const
  {
    return !( *this == other );
  }

  bool operator==(const SubtractConstantFrom & other) const
  {
    return other.m_Constant == m_Constant;
  }

  inline TOutput operator()(const TInput & A) const
  {
    // Because the user has to specify the constant we don't
    // check if the cte is not 0;
    return static_cast< TOutput >( A - m_Constant );
  }

  void SetConstant(TConstant ct) { this->m_Constant = ct; }
  const TConstant & GetConstant() const { return m_Constant; }

  TConstant m_Constant;
};
}

template< class TInputImage, class TConstant, class TOutputImage >
class ITK_EXPORT SubtractConstantFromImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::SubtractConstantFrom<
                             typename TInputImage::PixelType, TConstant,
                             typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef SubtractConstantFromImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::SubtractConstantFrom<
      typename TInputImage::PixelType, TConstant,
      typename TOutputImage::PixelType >   >             Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SubtractConstantFromImageFilter, UnaryFunctorImageFilter);

  /** Set the constant that will be used to multiply all the image pixels */
  void SetConstant(TConstant ct)
  {
    if ( ct != this->GetFunctor().GetConstant() )
      {
      this->GetFunctor().SetConstant(ct);
      this->Modified();
      }
  }

  const TConstant & GetConstant() const
  {
    return this->GetFunctor().GetConstant();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           typename TOutputImage::PixelType > ) );
  itkConceptMacro( Input1Input2OutputSubtractOperatorCheck,
                   ( Concept::AdditiveOperators< typename TInputImage::PixelType,
                                                 TConstant,
                                                 typename TOutputImage::PixelType > ) );
  /** End concept checking */
#endif
protected:
  SubtractConstantFromImageFilter() {}
  virtual ~SubtractConstantFromImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Constant: "
       << static_cast< typename NumericTraits< TConstant >::PrintType >( this->GetConstant() )
       << std::endl;
  }

  SubtractConstantFromImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented
};
} // end namespace itk

#endif
