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
#ifndef itkDefaultConvertPixelTraits_h
#define itkDefaultConvertPixelTraits_h

#include "itkOffset.h"
#include "itkVector.h"
#include "itkMatrix.h"
#include "itkVariableLengthVector.h"
#include "itkVariableSizeMatrix.h"

namespace itk
{
/** \class DefaultConvertPixelTraits
 *  \brief Traits class used to by ConvertPixels to convert blocks of pixels.
 *
 *  TOutputPixelType is the destination type. The input type is inferred
 *  by the templated static function Convert.
 *
 *  This implementation, does a simple assignment operator, so if you are
 *  going from from a higher bit representation to a lower bit one (int to
 *  char), you may want to specialize and add some sort of transfer function.
 * \ingroup ITKCommon
 */
template <typename PixelType>
class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits
{
public:
  /** Determine the pixel data type. */
  using ComponentType = typename PixelType::ComponentType;

  /** Return the number of components per pixel. */
  static unsigned int
  GetNumberOfComponents()
  {
    return PixelType::GetNumberOfComponents();
  }

  static unsigned int
  GetNumberOfComponents(const PixelType itkNotUsed(pixel))
  {
    return PixelType::GetNumberOfComponents();
  }

  /** Return the nth component of the pixel. */
  static ComponentType
  GetNthComponent(int c, const PixelType & pixel)
  {
    return pixel.GetNthComponent(c);
  }

  /** Set the nth component of the pixel. */
  static void
  SetNthComponent(int c, PixelType & pixel, const ComponentType & v)
  {
    pixel.SetNthComponent(c, v);
  }

  /** Return a single scalar value from this pixel. */
  static ComponentType
  GetScalarValue(const PixelType & pixel)
  {
    return pixel.GetScalarValue();
  }
};

#define ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(type)                                                                  \
  template <>                                                                                                          \
  class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<type>                                                            \
  {                                                                                                                    \
  public:                                                                                                              \
    using ComponentType = type;                                                                                        \
    static unsigned int                                                                                                \
    GetNumberOfComponents()                                                                                            \
    {                                                                                                                  \
      return 1;                                                                                                        \
    }                                                                                                                  \
    static unsigned int                                                                                                \
    GetNumberOfComponents(const type)                                                                                  \
    {                                                                                                                  \
      return 1;                                                                                                        \
    }                                                                                                                  \
    static void                                                                                                        \
    SetNthComponent(int, type & pixel, const ComponentType & v)                                                        \
    {                                                                                                                  \
      pixel = v;                                                                                                       \
    }                                                                                                                  \
    static type                                                                                                        \
    GetNthComponent(int, const type pixel)                                                                             \
    {                                                                                                                  \
      return pixel;                                                                                                    \
    }                                                                                                                  \
    static type                                                                                                        \
    GetScalarValue(const type & pixel)                                                                                 \
    {                                                                                                                  \
      return pixel;                                                                                                    \
    }                                                                                                                  \
  };

ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(float)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(double)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(long double)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(int)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(char)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(short)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned int)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(signed char)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned char)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned short)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(long)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned long)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(long long)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned long long)
ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(bool)

#undef ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL

//
//  Default traits for the Offset<> pixel type
//

template <unsigned int VDimension>
class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<Offset<VDimension>>
{
public:
  using TargetType = Offset<VDimension>;
  using ComponentType = typename TargetType::OffsetValueType;
  static unsigned int
  GetNumberOfComponents()
  {
    return VDimension;
  }
  static void
  SetNthComponent(int i, TargetType & pixel, const ComponentType & v)
  {
    pixel[i] = v;
  }
  static ComponentType
  GetScalarValue(const TargetType & pixel)
  {
    return pixel[0];
  }
};

//
//  Default traits for the pixel types deriving from FixedArray<>
//

#define ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(type)                                                                 \
  template <typename TComponentType, unsigned VDimension>                                                              \
  class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<type<TComponentType, VDimension>>                                \
  {                                                                                                                    \
  public:                                                                                                              \
    using TargetType = type<TComponentType, VDimension>;                                                               \
    using ComponentType = TComponentType;                                                                              \
    static unsigned int                                                                                                \
    GetNumberOfComponents()                                                                                            \
    {                                                                                                                  \
      return VDimension;                                                                                               \
    }                                                                                                                  \
    static unsigned int                                                                                                \
    GetNumberOfComponents(const TargetType)                                                                            \
    {                                                                                                                  \
      return VDimension;                                                                                               \
    }                                                                                                                  \
    static void                                                                                                        \
    SetNthComponent(int i, TargetType & pixel, const ComponentType & v)                                                \
    {                                                                                                                  \
      pixel[i] = v;                                                                                                    \
    }                                                                                                                  \
    static ComponentType                                                                                               \
    GetNthComponent(int i, const TargetType pixel)                                                                     \
    {                                                                                                                  \
      return pixel[i];                                                                                                 \
    }                                                                                                                  \
    static ComponentType                                                                                               \
    GetScalarValue(const TargetType & pixel)                                                                           \
    {                                                                                                                  \
      return pixel[0];                                                                                                 \
    }                                                                                                                  \
  }

ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Vector);
ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(CovariantVector);
ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(Point);
ITK_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(FixedArray);

//
//  End of Traits for the classes deriving from FixedArray.
//
//

//
//  Default traits for pixel types deriving from VariableLengthVector<>
//
template <typename VComponent>
class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<VariableLengthVector<VComponent>>
{
public:
  using TargetType = VariableLengthVector<VComponent>;
  using ComponentType = VComponent;
  static unsigned int
  GetNumberOfComponents()
  {
    return 0;
  }
  static unsigned int
  GetNumberOfComponents(const TargetType pixel)
  {
    return pixel.Size();
  }
  static void
  SetNthComponent(int i, TargetType & pixel, const ComponentType & v)
  {
    pixel[i] = v;
  }
  static ComponentType
  GetNthComponent(int i, const TargetType & pixel)
  {
    return pixel[i];
  }
  static ComponentType
  GetScalarValue(const TargetType & pixel)
  {
    return pixel.GetNorm();
  }
};


//
//  Default traits for pixel types deriving from VariableSizeMatrix<>
//
template <typename VComponent>
class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<VariableSizeMatrix<VComponent>>
{
public:
  using TargetType = VariableSizeMatrix<VComponent>;
  using ComponentType = VComponent;
  static unsigned int
  GetNumberOfComponents()
  {
    return 0;
  }
  static unsigned int
  GetNumberOfComponents(const TargetType pixel)
  {
    return pixel.Cols() * pixel.Rows();
  }
  static void
  SetNthComponent(int i, TargetType & pixel, const ComponentType & v)
  {
    const unsigned int row = i / pixel.Cols();
    const unsigned int col = i % pixel.Cols();
    pixel(row, col) = v;
  }
  static ComponentType
  GetNthComponent(int i, const TargetType & pixel)
  {
    const unsigned int row = i / pixel.Cols();
    const unsigned int col = i % pixel.Cols();
    return pixel(row, col);
  }
  static ComponentType
  GetScalarValue(const TargetType &)
  {
    return 0.0;
  }
};


//
//  End of Traits for the classes deriving from FixedArray.
//
//

//
//  Default traits for the pixel types deriving from Matrix<>
//

template <typename VComponent, unsigned VRows, unsigned VCols>
class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<Matrix<VComponent, VRows, VCols>>
{
public:
  using TargetType = Matrix<VComponent, VRows, VCols>;
  using ComponentType = VComponent;
  static unsigned int
  GetNumberOfComponents()
  {
    return VRows * VCols;
  }
  static void
  SetNthComponent(int i, TargetType & pixel, const ComponentType & v)
  {
    const unsigned int row = i / VCols;
    const unsigned int col = i % VCols;
    pixel[row][col] = v;
  }
  static ComponentType
  GetNthComponent(int i, const TargetType & pixel)
  {
    const unsigned int row = i / VCols;
    const unsigned int col = i % VCols;
    return pixel[row][col];
  }
  static ComponentType
  GetScalarValue(const TargetType & pixel)
  {
    return pixel[0][0];
  }
};

//
//  Default traits for the pixel types deriving from std::complex<>
//

template <typename TComponent>
class ITK_TEMPLATE_EXPORT DefaultConvertPixelTraits<::std::complex<TComponent>>
{
public:
  using TargetType = ::std::complex<TComponent>;
  using ComponentType = TComponent;
  static unsigned int
  GetNumberOfComponents()
  {
    return 2;
  }
  static void
  SetNthComponent(int i, TargetType & pixel, const ComponentType & v)
  {
    if (i == 0)
    {
      pixel = TargetType(v, pixel.imag());
    }
    else
    {
      pixel = TargetType(pixel.real(), v);
    }
  }
  static ComponentType
  GetScalarValue(const TargetType & pixel)
  {
    return std::norm(pixel);
  }
};


//
//  End of Traits for the classes deriving from std::complex.
//
//
} // end namespace itk
#endif
