/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSobelEdgeDetectionImageFilter.h
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
#ifndef __itkSobelEdgeDetectionImageFilter_h
#define __itkSobelEdgeDetectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class SobelEdgeDetectionImageFilter
 * \brief Implements the Sobel Edge Detection.
 *  
 *  In this filter, the Sobel operator is first applied to each direction just
 *  the derivative image filters. then the the gradient magnitude is 
 *  calculated, which indicates where the edges are.

 * \sa ImageToImageFilter
 * \sa SobelOperator
 * 
 * \ingroup ImageFeatureExtraction 
 *
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT SobelEdgeDetectionImageFilter : 
           public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef SobelEdgeDetectionImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(SobelEdgeDetectionImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * SobelEdgeDetectionImageFilter needs a larger input requested region than
   * the output requested region (larger in the direction of the
   * derivative).  As such, SobelEdgeDetectionImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  SobelEdgeDetectionImageFilter() {}
  virtual ~SobelEdgeDetectionImageFilter() {}
  SobelEdgeDetectionImageFilter(const Self&) {}
  //void operator=(const Self&) {}

  /**
   * Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default.
   */
  void GenerateData();

private:

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSobelEdgeDetectionImageFilter.txx"
#endif

#endif
