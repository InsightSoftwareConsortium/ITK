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

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
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
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 *
 * \ingroup ITK-ImageIntensity
 */
template< typename  TInputImage, typename  TOutputImage = TInputImage >
class ITK_EXPORT ModulusImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::ModulusTransform<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef ModulusImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::ModulusTransform<
      typename TInputImage::PixelType,
      typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType                   OutputPixelType;
  typedef typename TInputImage::PixelType                    InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ModulusImageFilter,
               UnaryFunctorImageFilter);

  /** Set/Get the dividend */
  itkSetMacro(Dividend, InputPixelType);
  itkGetConstReferenceMacro(Dividend, InputPixelType);

  /** Print internal ivars */
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  /** End concept checking */
#endif
protected:
  ModulusImageFilter();
  virtual ~ModulusImageFilter() {}
private:
  ModulusImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

  InputPixelType m_Dividend;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkModulusImageFilter.txx"
#endif

#endif
