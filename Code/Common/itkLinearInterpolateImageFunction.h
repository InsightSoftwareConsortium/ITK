/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkLinearInterpolateImageFunction_h
#define _itkLinearInterpolateImageFunction_h

#include "itkContinuousImageFunction.h"
#include "itkPoint.h"

namespace itk
{

/** 
 * \class LinearInterpolateImageFunction
 * \brief Linearly interpolate an image.
 *
 * LinearInterpolateImageFunction linearly interpolates image intensity at
 * a non-integer pixel position. This class is templated
 * over the input image type.
 *
 * This function works for N-dimensional images.
 * */
template <class TInputImage >
class ITK_EXPORT LinearInterpolateImageFunction : 
  public ContinuousImageFunction<TInputImage,double> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LinearInterpolateImageFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ContinuousImageFunction<TInputImage,double> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * Dimension underlying input image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Index typedef support.
   */
  typedef typename InputImageType::IndexType IndexType;

  /**
   * Point typedef support.
   */
  typedef typename Superclass::PointType  PointType;

  /**
   * Evaluate the function at a Point position
   *
   * Returns the linearly interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * Superclass::IsInsideBuffer() can be used to check bounds before
   * calling the method.
   */
  virtual double Evaluate( const PointType & point ) const;

protected:
  LinearInterpolateImageFunction();
  LinearInterpolateImageFunction( const Self& ){};

  ~LinearInterpolateImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:

  unsigned long          m_Neighbors;  // Size of interpolation neighborhood

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearInterpolateImageFunction.txx"
#endif

#endif
