/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.h
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
#ifndef __itkRandomImageSource_h
#define __itkRandomImageSource_h

#include "itkImageSource.h"

namespace itk
{

/** \class RandomImageSource
 * \brief Generate an n-dimensional image of random image values.
 *
 * RandomImageSource generates an image of random scalar values.
 * The output image may be of any dimension. The scalar values are
 * inserted into the image via a scalar iterator (i.e., the pixel type
 * must support GetScalar()/SetScalar()).
 */
template <typename TOutputImage>
class ITK_EXPORT RandomImageSource : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RandomImageSource   Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Typedef for the output image PixelType
   */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /**
   * Typedef for the output image ScalarValueType.
   */
  typedef typename TOutputImage::ScalarValueType OutputImageScalarValueType;

  /**
   * Typedef to describe the output image region type.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RandomImageSource,ImageSource);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Specify the size of the output image.
   */
  itkSetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);

  /** 
   * Get the size of the output image.
   */
  itkGetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);
  
  /** 
   * Specify the spacing of the output image.
   */
  itkSetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** 
   * Get the spacing of the output image.
   */
  itkGetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** 
   * Specify the origin of the output image.
   */
  itkSetVectorMacro(Origin,float,TOutputImage::ImageDimension);

  /** 
   * Get the origin of the output image.
   */
  itkGetVectorMacro(Origin,float,TOutputImage::ImageDimension);
  
  /** 
   * Set the minimum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::ScalarValueType>::min().
   */
  itkSetClampMacro(Min, OutputImageScalarValueType,
                   NumericTraits<OutputImageScalarValueType>::min(),
                   NumericTraits<OutputImageScalarValueType>::max());
  
  /** 
   * Get the minimum possible pixel value.
   */
  itkGetMacro(Min,typename TOutputImage::ScalarValueType);

  /** 
   * Set the maximum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::ScalarValueType>::max().
   */
  itkSetClampMacro(Max, OutputImageScalarValueType,
                   NumericTraits<OutputImageScalarValueType>::min(),
                   NumericTraits<OutputImageScalarValueType>::max());
  
  /** 
   * Get the maximum possible pixel value.
   */
  itkGetMacro(Max, OutputImageScalarValueType);

protected:
  RandomImageSource();
  ~RandomImageSource();
  RandomImageSource(const RandomImageSource&) {};
  void operator=(const RandomImageSource&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
  virtual void 
  ThreadedGenerateData(const OutputImageRegionType& 
		       outputRegionForThread, int threadId );
  virtual void GenerateOutputInformation();

private:
  unsigned long *m_Size;    //size of the output image
  float         *m_Spacing; //spacing
  float         *m_Origin;  //origin

  typename TOutputImage::ScalarValueType m_Min; //minimum possible value
  typename TOutputImage::ScalarValueType m_Max; //maximum possible value
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.txx"
#endif

#endif
