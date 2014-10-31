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
#ifndef __itkModulusImageFilter_h
#define __itkModulusImageFilter_h

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{

/** \class Modulus
 *
 * \ingroup ITKImageIntensity
 */
template< typename TInput1, typename TInput2, typename TOutput >
class Modulus
{
public:
  Modulus() {  }
  ~Modulus() {}

  bool operator!=(const Modulus &) const
  {
    return false;
  }

  bool operator==(const Modulus & other) const
  {
    return !( *this != other );
  }

 inline TOutput operator()(const TInput1 & A, const TInput2 & B) const
 {
   if ( B != NumericTraits<TInput2>::ZeroValue() )
     {
     return static_cast< TOutput >( A % B );
     }
   else
     {
     return NumericTraits< TOutput >::max( static_cast<TOutput>(A) );
     }
  }

};

/** \class ModulusTransform
 *
 * \deprecated The two template parametered ModulusTransform functor
 * is depricated. Please use the version with 3 template parameters.
 *
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename  TOutput >
class ModulusTransform
{
public:
  ModulusTransform() { m_Dividend = 5; }
  ~ModulusTransform() {}
  void SetDividend(TOutput dividend) { m_Dividend = dividend; }

  bool operator!=(const ModulusTransform & other) const
    {
      if ( m_Dividend != other.m_Dividend )
        {
        return true;
        }
      return false;
    }

  bool operator==(const ModulusTransform & other) const
    {
      return !( *this != other );
    }

  inline TOutput operator()(const TInput & x) const
    {
      TOutput result = static_cast< TOutput >( x % m_Dividend );

      return result;
    }

private:
  TInput m_Dividend;
};

}  // end namespace functor

/** \class ModulusImageFilter
 * \brief Computes the modulus (x % dividend) pixel-wise
 *
 * The input pixel type must support the c++ modulus operator (%).
 *
 * If the dividend is zero, the maximum value will be returned.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 */
template< typename  TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1 >
class ModulusImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::Modulus<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ModulusImageFilter Self;
  typedef BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::Modulus<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType                    OutputPixelType;
  typedef typename TInputImage1::PixelType                    InputPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ModulusImageFilter,
               UnaryFunctorImageFilter);

  /** Set/Get the dividend */
  virtual void SetDividend( InputPixelType _arg ) { this->SetConstant2(_arg); }
  virtual const InputPixelType &GetDividend () const { return this->GetConstant2(); }


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  ModulusImageFilter();
  virtual ~ModulusImageFilter() {}

private:
  ModulusImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkModulusImageFilter.hxx"
#endif

#endif
