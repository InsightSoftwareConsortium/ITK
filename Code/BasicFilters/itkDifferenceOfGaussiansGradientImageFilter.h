/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceOfGaussiansGradientImageFilter.h
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
#ifndef __itkDifferenceOfGaussiansGradientImageFilter_h
#define __itkDifferenceOfGaussiansGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class DifferenceOfGaussiansGradientImageFilter
 * \brief Performs difference-of-gaussians gradient detection
 *
 * \ingroup ImageFeatureExtraction 
 *
 * */

template<typename TInputImage, typename TDataType>
class ITK_EXPORT DifferenceOfGaussiansGradientImageFilter :
public ImageToImageFilter<TInputImage,
  Image<CovariantVector<TDataType>, TInputImage::ImageDimension> >
{
public:
   
  /**
   * Standard "Self" typedef.
   */
  typedef DifferenceOfGaussiansGradientImageFilter Self;

  /**
   * Output image typedef. The output image is always an n-dimensional
   * image of n-dimensional vectors of doubles.
   */
  typedef Image<CovariantVector<TDataType>, TInputImage::ImageDimension>
    TOutputImage;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage, TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Number of dimensions
   */
  enum {NDimensions = TInputImage::ImageDimension};

  /**
   * Image size typedef
   */
  typedef Size<TInputImage::ImageDimension> SizeType;

  /**
   * Image index typedef
   */
  typedef typename TInputImage::IndexType IndexType;

  /**
   * Image pixel value typedef
   */
  typedef typename TInputImage::PixelType PixelType;

  /**
   * Typedef to describe the output image region type.
   */
  typedef typename TInputImage::RegionType OutputImageRegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( DifferenceOfGaussiansGradientImageFilter, ImageToImageFilter );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Method for evaluating the implicit function over the image.
   */
  void GenerateData();

  /**
   * Gets and sets for member variables
   */
  itkGetMacro(Width, unsigned int);
  itkSetMacro(Width, unsigned int);

protected:

  DifferenceOfGaussiansGradientImageFilter();
  virtual ~DifferenceOfGaussiansGradientImageFilter() {};

  DifferenceOfGaussiansGradientImageFilter(const Self&) {}
  void operator=(const Self&) {}

private:

  unsigned int m_Width;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDifferenceOfGaussiansGradientImageFilter.txx"
#endif

#endif
