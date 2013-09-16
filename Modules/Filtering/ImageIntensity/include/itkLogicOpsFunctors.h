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
#ifndef __itkLogicOpsFunctors_h
#define __itkLogicOpsFunctors_h

#include "itkNumericTraits.h"


namespace itk
{
namespace Functor
{

/** \class LogicOpBase
 * \brief Base class for some logic functors. Provides the Foreground
 * and background setting methods.
 *
 * The derived classes can be used as follows:
 *
 *  typedef itk::BinaryFunctorImageFilter<
 *    myImageType1,
 *    myImageType2,
 *    myImageType3,
 *    itk::Functor::Equal2<myImageType1::PixelType,
 *                          myImageType2::PixelType,
 *                          myImageType3::PixelType>
 *  >       myFilterType;
 *
 *  typedef myFilterType::Pointer   myFilterTypePointer;
 *
 *  myFilterTypePointer filter = myFilterType::New();
 *
 *  filter->SetInput1( inputImageA );
 *  filter->SetInput2( inputImageB );
 *
 *
 *  filter->SetConstant1(3.0);
 *  filter->SetInput2(inputImageB);
 *
 * \ingroup ITKImageIntensity
*/
template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class LogicOpBase
{
public:
  typedef LogicOpBase Self;
  LogicOpBase()
  {
    m_ForegroundValue=itk::NumericTraits<TOutput>::One;
    m_BackgroundValue=itk::NumericTraits<TOutput>::Zero;
  }

  ~LogicOpBase(){};


  bool operator!=( const Self & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }

  void SetForegroundValue(const TOutput &FG)
  {
    m_ForegroundValue=FG;
  }
  void SetBackgroundValue(const TOutput &BG)
  {
    m_BackgroundValue=BG;
  }

  TOutput GetForegroundValue() const
  {
    return(m_ForegroundValue);
  }
  TOutput GetBackgroundValue() const
  {
    return(m_BackgroundValue);
  }

protected:
  TOutput m_ForegroundValue;
  TOutput m_BackgroundValue;

};

/** \class Equal
 * \brief Functor for == operation on images and constants.
 *
 * Operations by c++ casting defaults. Foreground and background
 * values are set by methods. Defaults are 1, 0.
 *
 * \ingroup ITKImageIntensity
*/

template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class Equal : public LogicOpBase<TInput1, TInput2, TOutput>
{
public:
  typedef Equal Self;

  Equal()
  {};
  ~Equal()
  {};

  bool operator!=( const Self & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  {
    if( A == static_cast<TInput1>(B) )
      {
      return this->m_ForegroundValue;
      }
    return this->m_BackgroundValue;
  }

};
/** \class NotEqual
 * \brief Functor for != operation on images and constants.
 *
 * Operations by c++ casting defaults. Foreground and background
 * values are set by methods. Defaults are 1, 0.
 *
 * \ingroup ITKImageIntensity
*/

template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class NotEqual : public LogicOpBase<TInput1, TInput2, TOutput>
{
public:
  typedef NotEqual Self;

  NotEqual() {};
  ~NotEqual() {};
  bool operator!=( const Self & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  {
    if( A != B )
      {
      return this->m_ForegroundValue;
      }
    return this->m_BackgroundValue;
  }

};

/** \class GreaterEqual
 * \brief Functor for >= operation on images and constants.
 *
 * Operations by c++ casting defaults. Foreground and background
 * values are set by methods. Defaults are 1, 0.
 *
 * \ingroup ITKImageIntensity
*/

template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class GreaterEqual : public LogicOpBase<TInput1, TInput2, TOutput>
{
public:
  typedef GreaterEqual Self;
  GreaterEqual() {};
  ~GreaterEqual() {};

  bool operator!=( const Self & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  {
    if( A >= B )
      {
      return this->m_ForegroundValue;
      }
    return this->m_BackgroundValue;
  }

};
/** \class Greater
 * \brief Functor for > operation on images and constants.
 *
 * Operations by c++ casting defaults. Foreground and background
 * values are set by methods. Defaults are 1, 0.
 *
 * \ingroup ITKImageIntensity
*/

template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class Greater : public LogicOpBase<TInput1, TInput2, TOutput>
{
public:
  typedef Greater Self;
  Greater() {};
  ~Greater() {};
  bool operator!=( const Self & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  {
    if( A > B )
      {
      return this->m_ForegroundValue;
      }
    return this->m_BackgroundValue;
  }
};
/** \class LessEqual
 * \brief Functor for <= operation on images and constants.
 *
 * Operations by c++ casting defaults. Foreground and background
 * values are set by methods. Defaults are 1, 0.
 *
 * \ingroup ITKImageIntensity
*/

template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class LessEqual : public LogicOpBase<TInput1, TInput2, TOutput>
{
public:
  typedef LessEqual Self;

  LessEqual(){};
  ~LessEqual(){};
  bool operator!=( const Self & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  {
    if( A <= B )
      {
      return this->m_ForegroundValue;
      }
    return this->m_BackgroundValue;
  }

};
/** \class Less
 * \brief Functor for < operation on images and constants.
 *
 * Operations by c++ casting defaults. Foreground and background
 * values are set by methods. Defaults are 1, 0.
 *
 * \ingroup ITKImageIntensity
*/

template< typename TInput1, typename TInput2=TInput1, typename TOutput=TInput1 >
class Less : public LogicOpBase<TInput1, TInput2, TOutput>
{
public:
  typedef Less Self;
  Less() {};
  ~Less() {};
  bool operator!=( const Self  & ) const
  {
    return false;
  }
  bool operator==( const Self & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput1 & A, const TInput2 & B)
  {
    if( A < B )
      {
      return this->m_ForegroundValue;
      }
    return this->m_BackgroundValue;
  }

};


}
}

#endif
