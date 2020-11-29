/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkColormapFunction_h
#define itkColormapFunction_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkRGBPixel.h"

namespace itk
{
namespace Function
{
/**
 * \class ColormapFunction
 * \brief Function object which maps a scalar value into an RGB colormap value.
 *
 * \author Nicholas Tustison, Hui Zhang, Gaetan Lehmann, Paul Yushkevich
 * and James C. Gee
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * https://www.insight-journal.org/browse/publication/285
 *
 * \ingroup ITKColormap
 */
template <typename TScalar, typename TRGBPixel>
class ColormapFunction : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ColormapFunction);

  using Self = ColormapFunction;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ColormapFunction, Object);

  using RGBPixelType = TRGBPixel;
  using RGBComponentType = typename TRGBPixel::ComponentType;
  using ScalarType = TScalar;
  using RealType = typename NumericTraits<ScalarType>::RealType;

  itkSetMacro(MinimumRGBComponentValue, RGBComponentType);
  itkGetConstMacro(MinimumRGBComponentValue, RGBComponentType);

  itkSetMacro(MaximumRGBComponentValue, RGBComponentType);
  itkGetConstMacro(MaximumRGBComponentValue, RGBComponentType);

  itkSetMacro(MinimumInputValue, ScalarType);
  itkGetConstMacro(MinimumInputValue, ScalarType);

  itkSetMacro(MaximumInputValue, ScalarType);
  itkGetConstMacro(MaximumInputValue, ScalarType);

  virtual bool
  operator!=(const ColormapFunction &) const
  {
    return false;
  }

  virtual bool
  operator==(const ColormapFunction & other) const
  {
    return !(*this != other);
  }

  virtual RGBPixelType
  operator()(const ScalarType &) const = 0;

protected:
  ColormapFunction()
  {
    this->m_MinimumInputValue = NumericTraits<TScalar>::min();
    this->m_MaximumInputValue = NumericTraits<TScalar>::max();
    this->m_MinimumRGBComponentValue = NumericTraits<RGBComponentType>::min();
    this->m_MaximumRGBComponentValue = NumericTraits<RGBComponentType>::max();
  }

  ~ColormapFunction() override = default;

  /**
   * Map [min, max] input values to [0, 1].
   */
  RealType
  RescaleInputValue(ScalarType v) const
  {
    auto maxInputValue = static_cast<RealType>(this->m_MaximumInputValue);
    auto minInputValue = static_cast<RealType>(this->m_MinimumInputValue);

    auto     d = static_cast<RealType>(maxInputValue - minInputValue);
    RealType value = (static_cast<RealType>(v) - static_cast<RealType>(minInputValue)) / d;

    value = std::max(0.0, value);
    value = std::min(1.0, value);
    return value;
  }

  /**
   * Map [0, 1] value to [min, max] rgb component values.
   */
  RGBComponentType
  RescaleRGBComponentValue(RealType v) const
  {
    auto                   d = static_cast<RealType>(m_MaximumRGBComponentValue - m_MinimumRGBComponentValue);
    const RGBComponentType rescaled = static_cast<RGBComponentType>(d * v) + this->m_MinimumRGBComponentValue;

    return rescaled;
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);

    os << indent << "Minimum RGB Component Value: "
       << static_cast<typename NumericTraits<RGBComponentType>::PrintType>(this->GetMinimumRGBComponentValue())
       << std::endl;
    os << indent << "Maximum RGB Component Value: "
       << static_cast<typename NumericTraits<RGBComponentType>::PrintType>(this->GetMaximumRGBComponentValue())
       << std::endl;
    os << indent << "Minimum Input Value: "
       << static_cast<typename NumericTraits<ScalarType>::PrintType>(this->GetMinimumInputValue()) << std::endl;
    os << indent << "Maximum Input Value: "
       << static_cast<typename NumericTraits<ScalarType>::PrintType>(this->GetMaximumInputValue()) << std::endl;
  }

private:
  ScalarType m_MinimumInputValue;
  ScalarType m_MaximumInputValue;

  RGBComponentType m_MinimumRGBComponentValue;
  RGBComponentType m_MaximumRGBComponentValue;
};
} // end namespace Function
} // end namespace itk

#endif
