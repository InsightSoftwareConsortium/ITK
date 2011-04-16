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
#ifndef __itkDivideByConstantImageFilter_h
#define __itkDivideByConstantImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class DivideByConstantImageFilter
 *
 * \brief Divide input pixels by a constant.
 *
 * This filter is templated over the input image type
 * and the output image type.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
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
class DivideByConstant
{
public:
  DivideByConstant():m_Constant(NumericTraits< TConstant >::One) {}
  ~DivideByConstant() {}
  bool operator!=(const DivideByConstant & other) const
  {
    return !( *this == other );
  }

  bool operator==(const DivideByConstant & other) const
  {
    return other.m_Constant == m_Constant;
  }

  inline TOutput operator()(const TInput & A) const
  {
    // Because the user has to specify the constant we don't
    // check if the constant is not too small (i.e. almost equal to zero);
    return static_cast< TOutput >( A / m_Constant );
  }

  void SetConstant(TConstant ct)
  {
    if ( ct == NumericTraits< TConstant >::Zero )
      {
      itkGenericExceptionMacro(
        << "The constant value used as denominator should not be set to zero");
      }

    this->m_Constant = ct;
  }

  const TConstant & GetConstant() const { return m_Constant; }

  TConstant m_Constant;
};
}

template< class TInputImage, class TConstant, class TOutputImage >
class ITK_EXPORT DivideByConstantImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::DivideByConstant<
                             typename TInputImage::PixelType, TConstant,
                             typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef DivideByConstantImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage, Functor::DivideByConstant<
      typename TInputImage::PixelType, TConstant,
      typename TOutputImage::PixelType >   >                  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DivideByConstantImageFilter, UnaryFunctorImageFilter);

  /** Set the constant value that will be used for dividing all the image
   * pixels */
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
  // The following concept check doesn't seem to work with vector immages
  //itkConceptMacro(Input1Input2OutputDivisionOperatorsCheck,
  //                (Concept::DivisionOperators<typename TInputImage::PixelType,
  //                 TConstant,
  //                 typename TOutputImage::PixelType>));
  /** End concept checking */
#endif
protected:
  DivideByConstantImageFilter() {}
  virtual ~DivideByConstantImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Constant: "
       << static_cast< typename NumericTraits< TConstant >::PrintType >( this->GetConstant() )
       << std::endl;
  }

private:
  DivideByConstantImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented
};
} // end namespace itk

#endif
