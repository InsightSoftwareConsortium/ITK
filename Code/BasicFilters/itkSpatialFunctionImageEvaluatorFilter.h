/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunctionImageEvaluatorFilter.h
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
#ifndef __itkSpatialFunctionImageEvaluatorFilter_h
#define __itkSpatialFunctionImageEvaluatorFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"
#include "itkSpatialFunction.h"

namespace itk
{

/**
 * \class SpatialFunctionImageEvaluatorFilter
 * \brief Evaluates an SpatialFunction onto a source image
 *
 * SpatialFunctionImageEvaluatorFilter walks an input image and evaluates
 * the function at every pixel location. Since implicit functions exist
 * in "real" space, rather than index space, the source image must be a
 * PhysicalImage.
 *
 * Like its parent ImageToImageFilter, this class functions in the filtering
 * pipeline and produces a unique output image.
 * */

template<class TSpatialFunction, class TInputImage, class TOutputImage>
class ITK_EXPORT SpatialFunctionImageEvaluatorFilter :
   public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
   
  /**
   * Standard "Self" typedef.
   */
  typedef SpatialFunctionImageEvaluatorFilter Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;

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
  typedef Size<TOutputImage::ImageDimension> SizeType;

  /**
   * Image index typedef
   */
  typedef typename TOutputImage::IndexType IndexType;

  /**
   * Image pixel value typedef
   */
  typedef typename TOutputImage::PixelType PixelType;

  /**
   * Typedef to describe the output image region type.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /**
   * Type of function
   */
  typedef TSpatialFunction TFunctionType;

  /**
   * Return type of function
   */
  typedef typename TFunctionType::TFunctionValueType TFunctionValueType;

  /**
  * Typedef describing vector info
  */
  typedef typename TFunctionType::TVectorType TVectorType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( SpatialFunctionImageEvaluatorFilter, ImageToImageFilter );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Set the internal implicit function
   */
  void SetFunction( TFunctionType* pFunction )
    {m_pFunction = pFunction;};

  /**
   * Method for evaluating the implicit function over the image.
   */
  void GenerateData();

  /**
   * Gets and sets for member variables
   */

  itkSetMacro( InteriorValue, PixelType );
  itkGetMacro( InteriorValue, PixelType );

  itkSetMacro( ExteriorValue, PixelType );
  itkGetMacro( ExteriorValue, PixelType );

protected:

  SpatialFunctionImageEvaluatorFilter();
  virtual ~SpatialFunctionImageEvaluatorFilter() {};

  SpatialFunctionImageEvaluatorFilter(const Self&) {}
  void operator=(const Self&) {}

private:

  /**
   * The function that will be evaluated over the image
   */
  TFunctionType* m_pFunction;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialFunctionImageEvaluatorFilter.txx"
#endif

#endif
