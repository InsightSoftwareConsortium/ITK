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

#ifndef itkMeshConvertPixelTraits_h
#define itkMeshConvertPixelTraits_h

#include "itkArray.h"
#include "itkMatrix.h"
#include "itkOffset.h"
#include "itkCovariantVector.h"
#include "itkVariableLengthVector.h"
#include "itkVector.h"
#include "itkPoint.h"

namespace itk
{

/** \class MeshConvertPixelTraits
 *  \brief Traits class used to by ConvertPixels to convert blocks of pixels.
 *
 *  TOutputPixelType is the destination type. The input type is inferred
 *  by the templated static function Convert.
 *
 *  This implementaion, does a simple assignment operator, so if you are
 *  going from from a higher bit representation to a lower bit one (int to
 *  char), you may want to specialize and add some sort of transfer function.
 *  \ingroup ITKIOMesh
 */
template<typename PixelType>
class MeshConvertPixelTraits
{
public:
  /** Determine the pixel data type. */
  typedef typename PixelType::ComponentType ComponentType;

  /** Return the number of components per pixel. */
  static unsigned int GetNumberOfComponents()
  { return PixelType::GetNumberOfComponents();}

  static unsigned int GetNumberOfComponents(const PixelType& )
  { return PixelType::GetNumberOfComponents();}

  /** Return the nth component of the pixel. */
  static ComponentType GetNthComponent(int c, const PixelType& pixel)
  { return pixel.GetNthComponent(c); }

  /** Set the nth component of the pixel. */
  static void SetNthComponent(int c, PixelType& pixel, const ComponentType& v)
  { pixel.SetNthComponent(c, v); }

  /** Return a single scalar value from this pixel. */
  static ComponentType GetScalarValue(const PixelType& pixel)
  { return pixel.GetScalarValue(); }

};

#define ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(type)                     \
template<>                                                                \
  class MeshConvertPixelTraits<type>                                      \
  {                                                                       \
  public:                                                                 \
  typedef type ComponentType;                                             \
  static unsigned int GetNumberOfComponents()                             \
  {                                                                       \
  return 1;                                                               \
  }                                                                       \
  static unsigned int GetNumberOfComponents(const type& itkNotUsed(pixel))\
  {                                                                       \
  return 1;                                                               \
  }                                                                       \
  static ComponentType GetNthComponent(int itkNotUsed(c), const type& pixel)\
  {                                                                       \
  return pixel;                                                           \
  }                                                                       \
  static void SetNthComponent(int , type& pixel, const ComponentType& v)  \
  {                                                                       \
  pixel = v;                                                              \
  }                                                                       \
  static type GetScalarValue(const type& pixel)                           \
  {                                                                       \
  return pixel;                                                           \
  }                                                                       \
  };

  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(float)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(double)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(int)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(char)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(short)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned int)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(signed char)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned char)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned short)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(long)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(unsigned long)
  ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL(bool)

#undef ITK_DEFAULTCONVERTTRAITS_NATIVE_SPECIAL

//
//  Default traits for the Offset<> pixel type
//

#define ITK_MESH_DEFAULTCONVERTTRAITS_OFFSET_TYPE(dimension)                       \
  template<>                                                                       \
  class MeshConvertPixelTraits< Offset<dimension> >                                \
  {                                                                                \
  public:                                                                          \
  typedef Offset<dimension>  TargetType;                                           \
  typedef TargetType::OffsetValueType  ComponentType;                              \
  static unsigned int GetNumberOfComponents()                                      \
  {                                                                                \
  return dimension;                                                                \
  }                                                                                \
  static unsigned int GetNumberOfComponents(const TargetType& itkNotUsed(pixel))   \
  {                                                                                \
  return dimension;                                                                \
  }                                                                                \
  static ComponentType GetNthComponent(int c, const TargetType& pixel)             \
  {                                                                                \
  return pixel[c];                                                                 \
  }                                                                                \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
  {                                                                                \
  pixel[i] = v;                                                                    \
  }                                                                                \
  static ComponentType GetScalarValue(const TargetType& pixel)                     \
  {                                                                                \
  return pixel[0];                                                                 \
  }                                                                                \
  };                                                                               \


// Define traits for Offset<> from dimensions 1 to 5
  ITK_MESH_DEFAULTCONVERTTRAITS_OFFSET_TYPE(1)
  ITK_MESH_DEFAULTCONVERTTRAITS_OFFSET_TYPE(2)
  ITK_MESH_DEFAULTCONVERTTRAITS_OFFSET_TYPE(3)
  ITK_MESH_DEFAULTCONVERTTRAITS_OFFSET_TYPE(4)
  ITK_MESH_DEFAULTCONVERTTRAITS_OFFSET_TYPE(5)

//
//  Default traits for the pixel types deriving from FixedArray<>
//

#define ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(type,componenttype, dimension) \
  template<>                                                                         \
  class MeshConvertPixelTraits< type< componenttype, dimension> >                    \
  {                                                                                  \
  public:                                                                            \
  typedef type< componenttype, dimension >  TargetType;                              \
  typedef componenttype                     ComponentType;                           \
  static unsigned int GetNumberOfComponents()                                        \
  {                                                                                  \
  return dimension;                                                                  \
  }                                                                                  \
  static unsigned int GetNumberOfComponents(const TargetType& itkNotUsed(pixel))     \
  {                                                                                  \
  return dimension;                                                                  \
  }                                                                                  \
  static ComponentType GetNthComponent(int c, const TargetType& pixel)               \
  {                                                                                  \
  return pixel[c];                                                                   \
  }                                                                                  \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)     \
  {                                                                                  \
  pixel[i] = v;                                                                      \
  }                                                                                  \
  static ComponentType GetScalarValue(const TargetType& pixel)                       \
  {                                                                                  \
  return pixel[0];                                                                   \
  }                                                                                  \
  };                                                                                 \

//
//
// Define traits for Classed deriving from FixedArray from dimensions 1 to 6
// These classes include: Vector, CovariantVector and Point.
//
//
#define ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, Type)      \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,1)                     \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,2)                     \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,3)                     \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,4)                     \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,5)                     \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE(ArrayType,Type,6)

#define ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(ArrayType)      \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, char);           \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, signed char);    \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned char);  \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, short);          \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned short); \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, int);            \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned int);   \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, long);           \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, unsigned long);  \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, float);          \
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_MACRO(ArrayType, double);

  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(Vector);
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(CovariantVector);
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(Point);
  ITK_MESH_DEFAULTCONVERTTRAITS_FIXEDARRAY_TYPE_ALL_TYPES_MACRO(FixedArray);

//
//  End of Traits for the classes deriving from FixedArray.
//
//


//
//  Default traits for the pixel types deriving from Matrix<>
//

#define ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(type,componenttype,rows,cols)    \
template<>                                                                         \
  class MeshConvertPixelTraits< type< componenttype, rows, cols > >                \
  {                                                                                \
  public:                                                                          \
  typedef type< componenttype, rows, cols >  TargetType;                           \
  typedef componenttype                     ComponentType;                         \
  static unsigned int GetNumberOfComponents()                                      \
  {                                                                                \
  return rows * cols;                                                              \
  }                                                                                \
  static unsigned int GetNumberOfComponents(const TargetType& itkNotUsed(pixel))   \
  {                                                                                \
  return rows * cols;                                                              \
  }                                                                                \
  static ComponentType GetNthComponent(int c, const TargetType& pixel)             \
  {                                                                                \
  const unsigned int row = c / cols;                                               \
  const unsigned int col = c % cols;                                               \
  return pixel[row][col];                                                          \
  }                                                                                \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
  {                                                                                \
  const unsigned int row = i / cols;                                               \
  const unsigned int col = i % cols;                                               \
  pixel[row][col] = v;                                                             \
  }                                                                                \
  static ComponentType GetScalarValue(const TargetType& pixel)                     \
  {                                                                                \
  return pixel[0][0];                                                              \
  }                                                                                \
  };                                                                               \

//
//
// Define traits for Classed deriving from Matrix from dimensions 1 to 6
//
//
#define ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, Type)      \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(ArrayType,Type,1,1)                   \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(ArrayType,Type,2,2)                   \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(ArrayType,Type,3,3)                   \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(ArrayType,Type,4,4)                   \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(ArrayType,Type,5,5)                   \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE(ArrayType,Type,6,6)

#define ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_TYPES_MACRO(ArrayType)      \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, char);           \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, signed char);    \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, unsigned char);  \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, short);          \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, unsigned short); \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, int);            \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, unsigned int);   \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, long);           \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, unsigned long);  \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, float);          \
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_MACRO(ArrayType, double);

//
// Add here other classes that derive from Matrix or that have the same API
//
  ITK_MESH_DEFAULTCONVERTTRAITS_MATRIX_TYPE_ALL_TYPES_MACRO(Matrix);

//
//  End of Traits for the classes deriving from Matrix.
//
//


//
//  Default traits for the pixel types deriving from std::complex<>
//

#define ITK_MESH_DEFAULTCONVERTTRAITS_COMPLEX_TYPE( componenttype )                \
template<>                                                                         \
  class MeshConvertPixelTraits< ::std::complex< componenttype > >                  \
  {                                                                                \
  public:                                                                          \
  typedef ::std::complex< componenttype>  TargetType;                              \
  typedef componenttype                     ComponentType;                         \
  static unsigned int GetNumberOfComponents()                                      \
  {                                                                                \
  return 2;                                                                        \
  }                                                                                \
  static unsigned int GetNumberOfComponents(const TargetType & itkNotUsed(pixel))  \
  {                                                                                \
  return 2;                                                                        \
  }                                                                                \
  static ComponentType GetNthComponent(int i, TargetType & pixel)                  \
  {                                                                                \
  if( i == 0 )                                                                     \
    {                                                                              \
    return pixel.imag();                                                           \
    }                                                                              \
  else                                                                             \
    {                                                                              \
    return pixel.real();                                                           \
    }                                                                              \
  }                                                                                \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
  {                                                                                \
  if( i == 0 )                                                                     \
    {                                                                              \
    pixel = TargetType( v, pixel.imag() );                                         \
    }                                                                              \
  else                                                                             \
    {                                                                              \
    pixel = TargetType( pixel.real(), v );                                         \
    }                                                                              \
  }                                                                                \
  static ComponentType GetScalarValue(const TargetType& pixel)                     \
  {                                                                                \
  return std::norm(pixel);                                                         \
  }                                                                                \
  };                                                                               \

  ITK_MESH_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(float);
  ITK_MESH_DEFAULTCONVERTTRAITS_COMPLEX_TYPE(double);

#define ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(type,componenttype)               \
template<>                                                                         \
  class MeshConvertPixelTraits< type< componenttype> >                             \
  {                                                                                \
  public:                                                                          \
  typedef type< componenttype >             TargetType;                            \
  typedef componenttype                     ComponentType;                         \
  static unsigned int GetNumberOfComponents()                                      \
  {                                                                                \
  return 0;                                                                        \
  }                                                                                \
  static unsigned int GetNumberOfComponents(const TargetType& pixel)               \
  {                                                                                \
  return pixel.Size();                                                             \
  }                                                                                \
  static ComponentType GetNthComponent(int c, const TargetType& pixel)             \
  {                                                                                \
  return pixel[c];                                                                 \
  }                                                                                \
  static void SetNthComponent(int i, TargetType & pixel, const ComponentType& v)   \
  {                                                                                \
  pixel[i] = v;                                                                    \
  }                                                                                \
  static ComponentType GetScalarValue(const TargetType& pixel)                     \
  {                                                                                \
  return pixel[0];                                                                 \
  }                                                                                \
  };                                                                               \

#define ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE_ALL_TYPES_MACRO(ArrayType)        \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, char);                       \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, signed char);                \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, unsigned char);              \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, short);                      \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, unsigned short);             \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, int);                        \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, unsigned int);               \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, long);                       \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, unsigned long);              \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, float);                      \
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE(ArrayType, double);

  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE_ALL_TYPES_MACRO(Array);
  ITK_MESH_DEFAULTCONVERTTRAITS_ARRAY_TYPE_ALL_TYPES_MACRO(VariableLengthVector);
//
//  End of Traits for the classes deriving from std::complex.
//
//

} // end namespace itk
#endif
