
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingImageFilter.h
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

#ifndef __itkZeroCrossingImageFilter_h
#define __itkZeroCrossingImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"


namespace itk
{

/** \class ZeroCrossingImageFilter
 * \brief Detect the zero-crossing of the input image, which is usually 
 *  preprocessed by the LaplacianImageFilter.
 *  
 *  If the value of a pixel and its neighbor has different sign, then there
 *  must be zero-crossing between these two pixels. In this implemention, only
 *  the nearest neighbors are compared.(e.g. for 2D image, a pixel has 4 nearesst 
 *  neighbors). The pixel nearest to the zero-crossing position is set to 
 *  represent the place of zero-crossing. 
 *  
 *  \sa Image
 *  \sa Neighborhood
 *  \sa NeighborhoodOperator
 *  \sa NeighborhoodIterator
 *  
 *  \ingroup ImageFeatureExtraction
 */ 

template<class TInputImage, class TOutputImage>
  class ITK_EXPORT ZeroCrossingImageFilter: public ImageToImageFilter<TInputImage, TOutputImage>
  {

  public:
    /**
     * standard "Self" & Superclass typedef.
     */
    typedef ZeroCrossingImageFilter    Self;
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
    itkTypeMacro(ZeroCrossingImageFilter, ImageToImageFilter);
    
    /**
     * ImageDimension enumeration
     */
    enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * ZeroCrossingImageFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to do comparisons between the central pixel and ite neighbors).
   * Thus ZeroCrossingImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()
   */
    
    virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  protected:
    ZeroCrossingImageFilter(){}
    ~ZeroCrossingImageFilter(){}
    ZeroCrossingImageFilter(const Self&) {}

  /**
   * ZeroCrossingImageFilter can be implemented as a multithreaded filter.  
   * Therefore,this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  };

} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroCrossingImageFilter.txx"
#endif
  
#endif

