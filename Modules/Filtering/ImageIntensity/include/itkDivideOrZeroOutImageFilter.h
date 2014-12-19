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
#ifndef itkDivideOrZeroOutImageFilter_h
#define itkDivideOrZeroOutImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{

namespace Functor {

template< typename TNumerator, typename TDenominator=TNumerator, typename TOutput=TNumerator >
class DivideOrZeroOut
{
public:
  DivideOrZeroOut()
  {
    m_Threshold = 1e-5 * NumericTraits< TDenominator >::OneValue();
    m_Constant = NumericTraits< TOutput >::ZeroValue();
  };

  ~DivideOrZeroOut() {};

  bool operator!=( const DivideOrZeroOut & other ) const
  {
    return !(*this == other);
  }

  bool operator==( const DivideOrZeroOut & itkNotUsed(other) ) const
  {
    // Always return true for now.  Do a comparison to m_Threshold if it is
    // every made set-able.
    return true;
  }

  inline TOutput operator()( const TNumerator & n, const TDenominator & d )
  {
    if ( d < m_Threshold )
      {
      return m_Constant;
      }
    return static_cast< TOutput >( n ) / static_cast< TOutput >( d );
  }
  TDenominator m_Threshold;
  TOutput      m_Constant;
};
}  // end namespace functor


/** \class DivideOrZeroOutImageFilter
 * \brief
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la
 * Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 * \ingroup ITKImageIntensity
 *
 */
template< typename TInputImage1,
          typename TInputImage2=TInputImage1,
          typename TOutputImage=TInputImage1 >
class DivideOrZeroOutImageFilter :
  public BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                                   Functor::DivideOrZeroOut<
                                     typename TInputImage1::PixelType,
                                     typename TInputImage2::PixelType,
                                     typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef DivideOrZeroOutImageFilter       Self;
  typedef BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage,
    Functor::DivideOrZeroOut<
      typename TInputImage1::PixelType,
      typename TInputImage2::PixelType,
      typename TOutputImage::PixelType > > Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  typedef typename TInputImage1::PixelType NumeratorPixelType;
  typedef typename TInputImage2::PixelType DenominatorPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(DivideOrZeroOutImageFilter, BinaryFunctorImageFilter);

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Threshold: "  << GetThreshold() << std::endl;
  }

  /** Set/get the threshold below which pixels in the denominator will
   * be considered zero. */
  void SetThreshold( DenominatorPixelType threshold  )
  {
    if ( threshold != this->GetFunctor().m_Threshold )
      {
      this->GetFunctor().m_Threshold = threshold;
      this->Modified();
      }
  }
  DenominatorPixelType GetThreshold() const
  {
    return this->GetFunctor().m_Threshold;
  }

  /** Set/get the constant value returned when the denominator input
   * value is considered zero. */
  void SetConstant( OutputPixelType constant )
  {
    if ( constant != this->GetFunctor().m_Constant )
      {
      this->GetFunctor().m_Constant = constant;
      this->Modified();
      }
  }
  OutputPixelType GetConstant() const
  {
    return this->GetFunctor().m_Constant;
  }

protected:
  DivideOrZeroOutImageFilter() {};
  virtual ~DivideOrZeroOutImageFilter() {};

private:
  DivideOrZeroOutImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk
#endif
