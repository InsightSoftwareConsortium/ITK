/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJoinImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkJoinImageFilter_h
#define __itkJoinImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkPixelTraits.h"

namespace itk
{
  
namespace Functor {  
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
   */
  template <class TPixel1, class TPixel2>
  class JoinFunctor
  {
  public:
    JoinFunctor() {};
    ~JoinFunctor() {};

    /**
     * Typedefs for the output join type
     */
    typedef typename PixelTraits<TPixel1>::ValueType ValueType1;
    typedef typename PixelTraits<TPixel2>::ValueType ValueType2;
    enum {Dimension1 = PixelTraits<TPixel1>::Dimension};
    enum {Dimension2 = PixelTraits<TPixel2>::Dimension};
    
    typedef typename JoinTraits<ValueType1, ValueType2>::ValueType JoinValueType;
    enum {JoinDimension = Dimension1 + Dimension2};

    typedef Vector<JoinValueType, JoinDimension> JoinType;

    
    /**
     * operator().  This is the "call" method of the functor.
     */
    inline JoinType operator()( const TPixel1 & A, const TPixel2 & B)
    {
      JoinType out;

      // Copy A into the output, casting as necessary
      this->FirstCopier(out, 0, A);

      // Copy B into the output, starting where A left off,casting as necessary
      this->SecondCopier(out, Dimension1, B);

      return out;
    }

   private:
    /**
     * Helper class to choose the right Copier method for each pixel
     * type.  This helps us pick the right Copier for the "dimension"
     * or number of components in each pixel type. The following is a
     * work around for the lack of partial specialization (VC++) and lack of
     * templated member functions that are templated over an integer
     * value.
     */
    struct CopierDispatchBase {};
    template<unsigned int VDimension>
    struct CopierDispatch : public CopierDispatchBase {};

    /**
     * Copier function to copy the first pixel to the output pixel
     * casting as necessary. The contents of the source pixel are
     * placed in the output pixel at position idx.  This method simply
     * delegates to one of two overloaded implementations based on the
     * dimension (or number of components) of the first
     * pixel. FirstCopier() and SecondCopier() are defined as distinct
     * functions (as opposed to being a single Copier() function
     * overloaded on the last parameter type or pixel type) to avoid
     * "already declared/defined" errors for the case where TPixel1
     * and TPixel2 are the same types.
     */
    void FirstCopier(JoinType& out, unsigned int idx, const TPixel1& A)
    {
      FirstCopier(CopierDispatch<Dimension1>(), out, idx, A);
    }

    /**
     * Copier function specific to a vector type first pixel.
     */
    void FirstCopier(CopierDispatchBase,
                     JoinType& out, unsigned int idx, const TPixel1& A)
    {
      for (unsigned int i=0; i < Dimension1; i++, idx++)
        {
        out[idx] = static_cast<JoinValueType>(A[i]);
        }
    }

    /**
     * Copier function specific to a scalar first pixel.
     */
    void FirstCopier(CopierDispatch<1>,
                     JoinType& out, unsigned int idx, const TPixel1& A)
    {
      out[idx] = static_cast<JoinValueType>(A);
    }

    /**
     * Copier function to copy the second pixel to the output pixel casting
     * as necessary. The contents of the source pixel are placed in the
     * output pixel at position idx.  This method simply delegates to
     * one of two overloaded implementations based on the dimension
     * (or number of components) of the second pixel.
     */
    void SecondCopier(JoinType& out, unsigned int idx, const TPixel2& B)
    {
      SecondCopier(CopierDispatch<Dimension2>(), out, idx, B);
    }

    /**
     * Copier function specific to a vector type second pixel.
     */
    void SecondCopier(CopierDispatchBase,
                      JoinType& out, unsigned int idx, const TPixel2& B)
    {
      for (unsigned int i=0; i < Dimension2; i++, idx++)
        {
        out[idx] = static_cast<JoinValueType>(B[i]);
        }
    }

    /**
     * Copier function specific to a scalar second pixel.
     */
    void SecondCopier(CopierDispatch<1>,
                      JoinType& out, unsigned int idx, const TPixel2& B)
    {
      out[idx] = static_cast<JoinValueType>(B);
    }
  }; 
}


  
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
 * \ingroup IntensityImageFilters  Multithreaded
 */
template <class TInputImage1, class TInputImage2>
class ITK_EXPORT JoinImageFilter:
    public BinaryFunctorImageFilter<TInputImage1,
                                    TInputImage2,
                                    Image< ITK_TYPENAME Functor::JoinFunctor<ITK_TYPENAME TInputImage1::PixelType,  ITK_TYPENAME TInputImage2::PixelType>::JoinType, TInputImage1::ImageDimension>,
                                    Functor::JoinFunctor< ITK_TYPENAME TInputImage1::PixelType, ITK_TYPENAME TInputImage2::PixelType> >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef JoinImageFilter  Self;

  /**
   * Output typedefs
   */
  typedef typename Functor::JoinFunctor< typename TInputImage1::PixelType,  typename TInputImage2::PixelType>::JoinType OutputImagePixelType;
  enum {OutputImageDimension = TInputImage1::ImageDimension};
  typedef Image<OutputImagePixelType, OutputImageDimension> OutputImageType;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef BinaryFunctorImageFilter<TInputImage1,TInputImage2, OutputImageType,
      Functor::JoinFunctor< typename TInputImage1::PixelType,
                            typename TInputImage2::PixelType> > Superclass; 

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(JoinImageFilter, BinaryFunctorImageFilter);

protected:
  JoinImageFilter() {}
  virtual ~JoinImageFilter() {}
  JoinImageFilter(const Self&) {}
  void operator=(const Self&) {}

};

  
} // end namespace itk
  
#endif
