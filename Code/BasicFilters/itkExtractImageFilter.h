/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractImageFilter.h
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
#ifndef __itkExtractImageFilter_h
#define __itkExtractImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{

/** \class ExtractImageFilter
 * \brief Decrease the image size by cropping the image to the selected 
 * region bounds.
 *
 * ExtractImageFilter changes the image boundary of an image by removing  
 * pixels outside the target region.  The target region must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ExtractImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ExtractImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtractImageFilter, ImageToImageFilter);

  /** Typedef to describe the output and input image region types. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TOutputImage::IndexType OutputImageIndexType;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TInputImage::SizeType InputImageSizeType;

  /** ImageDimension enumeration */
  enum { ImageDimension = TInputImage::ImageDimension };

  /** Set/Get the output image region. */
  itkSetMacro(ExtractionRegion, OutputImageRegionType);
  itkGetMacro(ExtractionRegion, OutputImageRegionType);
                 
  /** ExtractImageFilter produces an image which is a different resolution
   * than its input image.  As such, ExtractImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in order
   * to inform the pipeline execution model.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation();

protected:
  ExtractImageFilter();
  ~ExtractImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ExtractImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  ExtractImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OutputImageRegionType m_ExtractionRegion;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtractImageFilter.txx"
#endif
  
#endif
