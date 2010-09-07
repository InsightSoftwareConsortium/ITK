/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkColormapFunctor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkColormapFunctor_h
#define __itkColormapFunctor_h

#include "itkLightObject.h"
#include "itkNumericTraits.h"
#include "itkRGBPixel.h"

namespace itk
{
namespace Functor
{
/**
 * \class ColormapFunctor
 * \brief Function object which maps a scalar value into an RGB colormap value.
 *
 *
 * \author Nicholas Tustison, Hui Zhang, Gaetan Lehmann, Paul Yushkevich and James C. Gee
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * http://www.insight-journal.org/browse/publication/285
 * http://hdl.handle.net/1926/1452
 *
 */
template< class TScalar, class TRGBPixel >
class ITK_EXPORT ColormapFunctor:public Object
{
public:

  typedef ColormapFunctor            Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ColormapFunctor, Object);

  typedef TRGBPixel                                      RGBPixelType;
  typedef typename TRGBPixel::ComponentType              RGBComponentType;
  typedef TScalar                                        ScalarType;
  typedef typename NumericTraits< ScalarType >::RealType RealType;

  itkSetMacro(MinimumRGBComponentValue, RGBComponentType);
  itkGetConstMacro(MinimumRGBComponentValue, RGBComponentType);

  itkSetMacro(MaximumRGBComponentValue, RGBComponentType);
  itkGetConstMacro(MaximumRGBComponentValue, RGBComponentType);

  itkSetMacro(MinimumInputValue, ScalarType);
  itkGetConstMacro(MinimumInputValue, ScalarType);

  itkSetMacro(MaximumInputValue, ScalarType);
  itkGetConstMacro(MaximumInputValue, ScalarType);

  virtual bool operator!=(const ColormapFunctor &) const
  {
    return false;
  }

  virtual bool operator==(const ColormapFunctor & other) const
  {
    return !( *this != other );
  }

  virtual RGBPixelType operator()(const ScalarType &) const = 0;

protected:
  ColormapFunctor()
  {
    this->m_MinimumInputValue = NumericTraits< TScalar >::min();
    this->m_MaximumInputValue = NumericTraits< TScalar >::max();
    this->m_MinimumRGBComponentValue = NumericTraits< RGBComponentType >::min();
    this->m_MaximumRGBComponentValue = NumericTraits< RGBComponentType >::max();
  }

  ~ColormapFunctor() {}

  /**
   * Map [min, max] input values to [0, 1].
   */
  RealType RescaleInputValue(ScalarType v) const
  {
    RealType d = static_cast< RealType >( this->m_MaximumInputValue - this->m_MinimumInputValue );
    RealType value = ( static_cast< RealType >( v )
                       - static_cast< RealType >( this->m_MinimumInputValue ) ) / d;

    value = vnl_math_max(0.0, value);
    value = vnl_math_min(1.0, value);
    return value;
  }

  /**
   * Map [0, 1] value to [min, max] rgb component values.
   */
  RGBComponentType RescaleRGBComponentValue(RealType v) const
  {
    RealType               d = static_cast< RealType >( m_MaximumRGBComponentValue - m_MinimumRGBComponentValue );
    const RGBComponentType rescaled =
      static_cast< RGBComponentType >( d * v ) + this->m_MinimumRGBComponentValue;

    return rescaled;
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);

    os << indent << "Minimum RGB Component Value: "
       << static_cast< typename NumericTraits< RGBComponentType >::PrintType >(
      this->GetMinimumRGBComponentValue() ) << std::endl;
    os << indent << "Maximum RGB Component Value: "
       << static_cast< typename NumericTraits< RGBComponentType >::PrintType >(
      this->GetMaximumRGBComponentValue() ) << std::endl;
    os << indent << "Minimum Input Value: "
       << static_cast< typename NumericTraits< ScalarType >::PrintType >(
      this->GetMinimumInputValue() ) << std::endl;
    os << indent << "Maximum Input Value: "
       << static_cast< typename NumericTraits< ScalarType >::PrintType >(
      this->GetMaximumInputValue() ) << std::endl;
  }

private:
  ColormapFunctor(const Self &); //purposely not implemented
  void operator=(const Self &);  //purposely not implemented

  ScalarType m_MinimumInputValue;
  ScalarType m_MaximumInputValue;

  RGBComponentType m_MinimumRGBComponentValue;
  RGBComponentType m_MaximumRGBComponentValue;
};
} // end namespace functor
} // end namespace itk

#endif
