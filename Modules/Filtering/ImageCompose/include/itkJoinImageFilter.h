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
#ifndef itkJoinImageFilter_h
#define itkJoinImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkPixelTraits.h"

namespace itk
{
namespace Functor
{
/**
 * \class JoinFunctor
 * \brief Join the components of two pixel types into a single pixel type.
 *
 * JoinFunctor combines the components of two pixel types into a
 * single pixel type.  The components of one pixel are appended to
 * the components of the other pixel. The output pixel type is an
 * itk::Vector. The ValueType of the vector is the smallest scalar
 * type that can represent the dynamic range of the both the input
 * pixel value types.  Hence, joining a char and unsigned char
 * results in a short since that is the smallest datatype with a
 * large enough dynamic range.  To define a consistent behavior
 * across different architectures, the join of an int and an
 * unsigned int is float. On a 64 bit architecture, this join could
 * be represented in a long. But on 32 bit architectures, the only
 * safe join value type is a float. For this and similar ambiguous
 * cases, the join value type is promoted to a float.
 * \ingroup ITKImageCompose
 */
template< typename TPixel1, typename TPixel2 >
class JoinFunctor
{
public:
  JoinFunctor() {}
  ~JoinFunctor() {}

  /** Standard typedefs */
  typedef JoinFunctor Self;

  /** Typedefs for the output join type. */
  typedef typename PixelTraits< TPixel1 >::ValueType               ValueType1;
  typedef typename PixelTraits< TPixel2 >::ValueType               ValueType2;
  typedef typename JoinTraits< ValueType1, ValueType2 >::ValueType JoinValueType;

  /** Capture the dimensions of the image. */
  itkStaticConstMacro(Dimension1, unsigned int,
                      PixelTraits< TPixel1 >::Dimension);
  itkStaticConstMacro(Dimension2, unsigned int,
                      PixelTraits< TPixel2 >::Dimension);
  itkStaticConstMacro(JoinDimension, unsigned int,
                      Dimension1 + Dimension2);

  /** A vector of the join dimension. */
  typedef Vector< JoinValueType, itkGetStaticConstMacro(JoinDimension) > JoinType;

  bool operator!=(const JoinFunctor &) const
  {
    return false;
  }

  bool operator==(const JoinFunctor & other) const
  {
    return !( *this != other );
  }

  /** operator().  This is the "call" method of the functor. */
  inline JoinType operator()(const TPixel1 & A, const TPixel2 & B) const
  {
    JoinType out;

    // Copy A into the output, casting as necessary
    this->FirstCopier(out, 0, A);

    // Copy B into the output, starting where A left off,casting as necessary
    this->SecondCopier(out, Dimension1, B);

    return out;
  }

private:
  /** Helper class to choose the right Copier method for each pixel
   * type.  This helps us pick the right Copier for the "dimension"
   * or number of components in each pixel type. The following is a
   * work around for the lack of partial specialization (VC++) and lack of
   * templated member functions that are templated over an integer
   * value. */
  struct CopierDispatchBase {};
  template< unsigned int VDimension >
  struct CopierDispatch:public CopierDispatchBase {};

  /** Copier function to copy the first pixel to the output pixel
   * casting as necessary. The contents of the source pixel are
   * placed in the output pixel at position idx.  This method simply
   * delegates to one of two overloaded implementations based on the
   * dimension (or number of components) of the first
   * pixel. FirstCopier() and SecondCopier() are defined as distinct
   * functions (as opposed to being a single Copier() function
   * overloaded on the last parameter type or pixel type) to avoid
   * "already declared/defined" errors for the case where TPixel1
   * and TPixel2 are the same types. */
  void FirstCopier(JoinType & out, unsigned int idx, const TPixel1 & A) const
  {
    FirstCopier(CopierDispatch< Dimension1 >(), out, idx, A);
  }

  /** Copier function specific to a vector type first pixel. */
  void FirstCopier(CopierDispatchBase,
                   JoinType & out, unsigned int idx, const TPixel1 & A) const
  {
    for ( unsigned int i = 0; i < Dimension1; i++, idx++ )
      {
      out[idx] = static_cast< JoinValueType >( A[i] );
      }
  }

  /** Copier function specific to a scalar first pixel. */
  void FirstCopier(CopierDispatch< 1 >,
                   JoinType & out, unsigned int idx, const TPixel1 & A) const
  { out[idx] = static_cast< JoinValueType >( A ); }

  /** Copier function to copy the second pixel to the output pixel casting
   * as necessary. The contents of the source pixel are placed in the
   * output pixel at position idx.  This method simply delegates to
   * one of two overloaded implementations based on the dimension
   * (or number of components) of the second pixel. */
  void SecondCopier(JoinType & out, unsigned int idx, const TPixel2 & B) const
  {
    SecondCopier(CopierDispatch< Dimension2 >(), out, idx, B);
  }

  /** Copier function specific to a vector type second pixel. */
  void SecondCopier(CopierDispatchBase,
                    JoinType & out, unsigned int idx, const TPixel2 & B) const
  {
    for ( unsigned int i = 0; i < Dimension2; i++, idx++ )
      {
      out[idx] = static_cast< JoinValueType >( B[i] );
      }
  }

  /** Copier function specific to a scalar second pixel. */
  void SecondCopier(CopierDispatch< 1 >,
                    JoinType & out, unsigned int idx, const TPixel2 & B) const
  {
    out[idx] = static_cast< JoinValueType >( B );
  }
};  //class JoinFunction

template< typename TImage1, typename TImage2 >
struct MakeJoin {
  typedef JoinFunctor< typename TImage1::PixelType,
                       typename TImage2::PixelType > FunctorType;
  typedef Image< typename FunctorType::JoinType,
                  TImage1 ::ImageDimension > ImageType;
};
} //namespace functor

/** \class JoinImageFilter
 * \brief Join two images, resulting in an image where each pixel has the components of the first image followed by the components of the second image.
 *
 * JoinImageFilter combines two images by appending the components of one
 * image to the components of another image. The output image type is always
 * a itk::Vector image and the vector value type will the smallest type
 * that can represent the dynamic range of both the input value types.
 * Hence, joining an image of char and unsigned char results in an image
 * of shorts since that is the smallest datatype with a large enough
 * dynamic range.  To define a consistent behavior across different
 * architectures, the join of an int and an unsigned int is float. On a
 * 64 bit architecture, this join could be represented in a long. But on
 * 32 bit architectures, the only safe join value type is a float. For
 * this and similar ambiguous cases, the join value type is promoted to a
 * float.
 *
 * Note that this filter is not templated over its output image type.
 * Rather the filter determines what its output image type is based on
 * the input data types. To determine the output type, use
 * JoinImageFilter<Image1, Image2>::OutputImageType
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageCompose
 *
 * \wiki
 * \wikiexample{VectorImages/JoinImageFilter,Join images\, stacking their components}
 * \endwiki
 */
template< typename TInputImage1, typename TInputImage2 >
class JoinImageFilter:
  public BinaryFunctorImageFilter< TInputImage1,
                                   TInputImage2,
                                   typename
                                   Functor::MakeJoin< TInputImage1,
                                                      TInputImage2 >::ImageType,
                                   typename
                                   Functor::MakeJoin< TInputImage1,
                                                      TInputImage2 >::FunctorType >
{
public:
  /** Capture the output image dimension. */
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage1::ImageDimension);

  /** Standard class typedefs. */
  typedef JoinImageFilter Self;

  /** Output typedefs. */
  typedef typename Functor::MakeJoin< TInputImage1,
                                      TInputImage2 >::FunctorType FunctorType;
  typedef typename Functor::MakeJoin< TInputImage1,
                                      TInputImage2 >::ImageType OutputImageType;
  typedef typename FunctorType::JoinType OutputImagePixelType;

  /** Standard class typedefs. */
  typedef BinaryFunctorImageFilter< TInputImage1, TInputImage2, OutputImageType,
                                    FunctorType > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JoinImageFilter, BinaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( Input1HasPixelTraitsCheck,
                   ( Concept::HasPixelTraits< typename TInputImage1::PixelType > ) );
  itkConceptMacro( Input2HasPixelTraitsCheck,
                   ( Concept::HasPixelTraits< typename TInputImage2::PixelType > ) );
  itkConceptMacro( Input1Input2HasJoinTraitsCheck,
                   ( Concept::HasJoinTraits< typename PixelTraits< typename TInputImage1::PixelType >::ValueType,
                                             typename PixelTraits< typename TInputImage2::PixelType >::ValueType > ) );
  // End concept checking
#endif

protected:
  JoinImageFilter() {}
  virtual ~JoinImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(JoinImageFilter);
};
} // end namespace itk

#endif
