/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptLinearInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOptLinearInterpolateImageFunction_h
#define __itkOptLinearInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"

namespace itk
{

/** \class LinearInterpolateImageFunction
 * \brief Linearly interpolate an image at specified positions.
 *
 * LinearInterpolateImageFunction linearly interpolates image intensity at
 * a non-integer pixel position. This class is templated
 * over the input image type and the coordinate representation type 
 * (e.g. float or double).
 *
 * This function works for N-dimensional images.
 *
 * \warning This function work only for images with scalar pixel
 * types. For vector images use VectorLinearInterpolateImageFunction.
 *
 * \sa VectorLinearInterpolateImageFunction
 *
 * \ingroup ImageFunctions ImageInterpolators 
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT LinearInterpolateImageFunction : 
  public InterpolateImageFunction<TInputImage,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef LinearInterpolateImageFunction                 Self;
  typedef InterpolateImageFunction<TInputImage,TCoordRep>   Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(LinearInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the linearly interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual inline OutputType EvaluateAtContinuousIndex( const 
                                                         ContinuousIndexType &
                                                            index ) const
    {
    return this->EvaluateOptimized( Dispatch< ImageDimension >(), index );
    }

protected:
  LinearInterpolateImageFunction();
  ~LinearInterpolateImageFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  LinearInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /** Number of neighbors used in the interpolation */
  static const unsigned long  m_Neighbors;  

  struct DispatchBase {};
  template< unsigned int > struct Dispatch : DispatchBase {};

  inline OutputType EvaluateOptimized( const Dispatch<0> &,
                                       const ContinuousIndexType & index) const
    {
    return 0;
    }

  inline OutputType EvaluateOptimized( const Dispatch<1>&,
                                       const ContinuousIndexType & index) const
    {
    IndexType basei;

    double i = index[0];
    basei[0] = (long)i;
    if( i < 0.0 && double(basei[0]) != i)
      {
      basei[0]--;
      }

    double distance = i - double(basei[0]);

    double val0 = this->GetInputImage()->GetPixel( basei );
    double val1 = val0;
    ++basei[0];
    val1 = this->GetInputImage()->GetPixel( basei );

    return( static_cast<OutputType>( val0 + distance * ( val1 - val0 ) ) );
    }

  inline OutputType EvaluateOptimized( const Dispatch<2>&,
                                       const ContinuousIndexType & index) const
    {
    IndexType basei;

    double i = index[0];
    basei[0] = (long)i;
    if( i < 0.0 && double(basei[0]) != i)
      {
      basei[0]--;
      }
    double distance0 = i - double(basei[0]);

    i = index[1];
    basei[1] = (long)i;
    if( i < 0.0 && double(basei[1]) != i)
      {
      basei[1]--;
      }
    double distance1 = i - double(basei[1]);


    double val00 = this->GetInputImage()->GetPixel( basei );
    if(distance0+distance1 == 0)
      {
      return( static_cast<OutputType>( val00 ) );
      }
    else if(distance1 == 0) // if they have the same "y"
      {
      ++basei[0];  // then interpolate across "x"
      if(basei[0]>this->m_EndIndex[0])
        {
        return( static_cast<OutputType>( val00 ) );
        }
      double val10 = this->GetInputImage()->GetPixel( basei );
      return( static_cast<OutputType>(val00 + distance0 * (val10 - val00)) );
      }
    else if(distance0 == 0) // if they have the same "x"
      {
      ++basei[1];  // then interpolate across "y"
      if(basei[1]>this->m_EndIndex[1])
        {
        return( static_cast<OutputType>( val00 ) );
        }
      double val01 = this->GetInputImage()->GetPixel( basei );
      return( static_cast<OutputType>(val00 + distance1 * (val01 - val00)) );
      }
    else
      {
      ++basei[0];
      double val10 = this->GetInputImage()->GetPixel( basei );
      ++basei[1];
      double val11 = this->GetInputImage()->GetPixel( basei );
      --basei[0];
      double val01 = this->GetInputImage()->GetPixel( basei );
    
      double val0 = val00 + distance1 * ( val01 - val00 ); // interpolate across "y"
      double val1 = val10 + distance1 * ( val11 - val10 ); // interpolate across "y"

      return( static_cast<OutputType>( val0 + distance0 * (val1-val0) ) ); // interpolate across "X"
      }
    }

  inline OutputType EvaluateOptimized( const Dispatch<3>&,
                                       const ContinuousIndexType & index) const
    {
    IndexType basei;
    double val[8];

    unsigned long min0;
    unsigned long max0;
    unsigned long min1;
    unsigned long max1;
    unsigned long min2;
    unsigned long max2;

    double i = index[0];
    basei[0] = (long)i;
    if( i < 0.0 && double(basei[0]) != i)
      {
      basei[0]--;
      }
    double distance0 = i - double(basei[0]);
 
    i = index[1];
    basei[1] = (long)i;
    if( i < 0.0 && double(basei[1]) != i)
      {
      basei[1]--;
      }
    double distance1 = i - double(basei[1]);
 
    i = index[2];
    basei[2] = (long)i;
    if( i < 0.0 && double(basei[2]) != i)
      {
      basei[2]--;
      }
    double distance2 = i - double(basei[2]);

    val[0] = this->GetInputImage()->GetPixel( basei );
    if(distance0+distance1+distance2 == 0)
      {
      return( static_cast<OutputType>( val[0] ) );
      }
    if(distance0 > 0.0)
      {
      min0 = basei[0];
      max0 = basei[0]+1;
      if(max0>this->m_EndIndex[0])
        {
        max0 = this->m_EndIndex[0];
        }
      }
    if(distance1 > 0.0)
      {
      min1 = basei[1];
      max1 = basei[1]+1;
      if(max1>this->m_EndIndex[1])
        {
        max1 = this->m_EndIndex[1];
        }
      }
    if(distance2 > 0.0)
      {
      min2 = basei[2];
      max2 = basei[2]+1;
      if(max2>this->m_EndIndex[2])
        {
        max2 = this->m_EndIndex[2];
        }
      }
    if(distance2 == 0)
      {
      if(distance1 == 0)
        {
        basei[0] = max0;
        val[1] = this->GetInputImage()->GetPixel( basei );
 
        double val0 = val[0] + distance0 * (val[1]-val[0]);
 
        return( static_cast<OutputType>( val0 ) );
        }
      else if(distance0 == 0)
        {
        basei[1] = max1;
        val[2] = this->GetInputImage()->GetPixel( basei );
 
        double val0 = val[0] + distance1 * (val[2]-val[0]);
 
        return( static_cast<OutputType>( val0 ) );
        }
      else
        {
        basei[0] = max0;
        val[1] = this->GetInputImage()->GetPixel( basei );
        basei[1] = max1;
        val[3] = this->GetInputImage()->GetPixel( basei );
        basei[0] = min0;
        val[2] = this->GetInputImage()->GetPixel( basei );
 
        double val0 = val[0] + distance0 * (val[1]-val[0]);
        double val1 = val[2] + distance0 * (val[3]-val[2]);

        return( static_cast<OutputType>( val0 + distance1 * (val1 - val0) ) );
        }
      }
    else
      {
      basei[2] = max2;
      val[4] = this->GetInputImage()->GetPixel( basei );
      if(distance1 == 0)
        {
        if(distance0 == 0)
          {
          return( static_cast<OutputType>( val[0] + distance2 
                                                    * (val[4] - val[0]) ) );
          }
        else
          {
          basei[0] = max0;
          val[5] = this->GetInputImage()->GetPixel( basei );
          basei[2] = min2;
          val[1] = this->GetInputImage()->GetPixel( basei );
  
          double val0 = val[0] + distance0 * (val[1]-val[0]);
          double val1 = val[4] + distance0 * (val[5]-val[4]);
 
          return( static_cast<OutputType>( val0 + distance2 * (val1 - val0) ) );
          }
        }
      else if(distance0 == 0)
        {
        basei[1] = max1;
        val[6] = this->GetInputImage()->GetPixel( basei );
        basei[2] = min2;
        val[2] = this->GetInputImage()->GetPixel( basei );
 
        double val0 = val[0] + distance1 * (val[2]-val[0]);
        double val1 = val[4] + distance1 * (val[6]-val[4]);
 
        return( static_cast<OutputType>( val0 + distance2 * (val1 - val0) ) );
        }
      else 
        {
        basei[0] = max0;
        val[5] = this->GetInputImage()->GetPixel( basei );
        basei[1] = max1;
        val[7] = this->GetInputImage()->GetPixel( basei );
        basei[0] = min0;
        val[6] = this->GetInputImage()->GetPixel( basei );
        basei[2] = min2;
        val[2] = this->GetInputImage()->GetPixel( basei );
        basei[0] = max0;
        val[3] = this->GetInputImage()->GetPixel( basei );
        basei[1] = min1;
        val[1] = this->GetInputImage()->GetPixel( basei );
 
        double val00 = val[0] + distance0 * (val[1]-val[0]);
        double val01 = val[2] + distance0 * (val[3]-val[2]);
        double val0 = val00 + distance1 * (val01-val00);
  
        double val10 = val[4] + distance0 * (val[5]-val[4]);
        double val11 = val[6] + distance0 * (val[7]-val[6]);
        double val1 = val10 + distance1 * (val11-val10);
  
        return( static_cast<OutputType>( val0 + distance2 * (val1-val0) ) );
        }
      }
    }

  inline OutputType EvaluateOptimized( const DispatchBase &,
                                       const ContinuousIndexType & index) const
    {
    return this->EvaluateUnoptimized( index );
    }
                                       
  virtual inline OutputType EvaluateUnoptimized( 
                                       const ContinuousIndexType & index) const;
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_LinearInterpolateImageFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT LinearInterpolateImageFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef LinearInterpolateImageFunction< ITK_TEMPLATE_2 x > \
                                                  LinearInterpolateImageFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkLinearInterpolateImageFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkLinearInterpolateImageFunction.txx"
#endif

#endif
