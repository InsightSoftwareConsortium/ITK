
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingBasedEdgeDetectionImageFilter.h
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

#ifndef __itkZeroCrossingBasedEdgeDetectionImageFilter_h
#define __itkZeroCrossingBasedEdgeDetectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"


namespace itk
{

/** \class ZeroCrossingBasedEdgeDetectionImageFilter
 * \brief Provides a zero-crossing based edge detecor. 
 *
 * The input image is first * smoothed with a gauussian filter, then the 
 * LaplacianImageFilter is applied. Finally the zero-crossing of the 
 * laplaican of the image is  calculated. 
 *
 * To use this filter, the parameters--variance and maximum error parameter
 * needed by the DiscreteGaussianImageFilter.

 * \sa DiscreteGaussianImageFilter
 * \sa LaplacianImageFilter
 * \sa ZeroCrossingImageFilter
 * 
 * \ingroup ImageFeatureExtraction
 */

template<class TInputImage, class TOutputImage>
  class ITK_EXPORT ZeroCrossingBasedEdgeDetectionImageFilter: public ImageToImageFilter<TInputImage, TOutputImage>
  {

  public:
    /**
     * standard "Self" & Superclass typedef.
     */
    typedef ZeroCrossingBasedEdgeDetectionImageFilter    Self;
    typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
 

    /**
      * Image typedef support
      */
    typedef TInputImage  InputImageType;
    typedef TOutputImage OutputImageType;

       
    /** 
     * SmartPointer typedef support 
     */    

    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /**
     * Define pixel type
     */
    typedef typename TInputImage::PixelType  InputImagePixelType;
    typedef typename TOutputImage::PixelType  OutputImagePixelType;
    
    /**
     * Method for creation through the object factory.
     */
    itkNewMacro(Self);  
    
    /**
     * Typedef to describe the output image region type.
     */
    typedef typename TOutputImage::RegionType OutputImageRegionType;
    
    
    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro(ZeroCrossingBasedEdgeDetectionImageFilter, ImageToImageFilter);
    
    /**
     * ImageDimension enumeration
     */
    enum { ImageDimension = TInputImage::ImageDimension };

 /**
   * Standard get/set macros for filter parameters.
   */
  itkSetVectorMacro(Variance, float, ImageDimension);
  itkGetVectorMacro(Variance, const float, ImageDimension);
  itkSetVectorMacro(MaximumError, float, ImageDimension);
  itkGetVectorMacro(MaximumError, const float, ImageDimension);
  void SetVariance(const float v)
    {
      float vArray[ImageDimension];
      for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
      this->SetVariance(vArray);
    }
  void SetMaximumError(const float v)
    {
      float vArray[ImageDimension];
      for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
      this->SetMaximumError(vArray);
    }

    //    virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  protected:
    ZeroCrossingBasedEdgeDetectionImageFilter()
      {
        this->SetVariance(0.0f);
        this->SetMaximumError(0.01f);
      }
    ~ZeroCrossingBasedEdgeDetectionImageFilter(){}
    ZeroCrossingBasedEdgeDetectionImageFilter(const Self&) {}

  /**
   * Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to the pipeline of a DiscreteGaussianImageFilter, 
   * a LaplacianImageFilter and a ZeroCrossingImageFilter.  Since these
   * filters are multithreaded, this filter is multithreaded by default.
   */
  void GenerateData();

private:
   /**
   * The variance of the Gaussian Filter used in this filter
   */
  float m_Variance[ImageDimension];
  /**
   * The maximum error of the gaussian blurring kernel in each dimensional
   * direction.
   */

  float m_MaximumError[ImageDimension];  

  };

} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.txx"
#endif
  
#endif

