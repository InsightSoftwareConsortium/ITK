/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWrapPadImageFilter.h
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
#ifndef __itkWrapPadImageFilter_h
#define __itkWrapPadImageFilter_h

#include "itkPadImageFilter.h"
#include <vector>

namespace itk
{

/** \class WrapPadImageFilter
 * \brief Increase the image size by padding with replicants of the 
 * input image value.
 *
 * WrapPadImageFilter changes the image bounds of an image.  Added pixels are 
 * filled in with a wrapped replica of the input image.  The image bounds 
 * of the output must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT WrapPadImageFilter:
    public PadImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef WrapPadImageFilter         Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef PadImageFilter<TInputImage,TOutputImage>  Superclass;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self); 
  
  /**
   * Typedef to describe the output image region type.
   */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;
  
  /**
   * Typedef to describe the type of pixel.
   */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType InputImagePixelType;
  
  /**
   * Typedef to describe the output and input image index and size types.
   */
  typedef typename Superclass::OutputImageIndexType OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType InputImageSizeType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(WrapPadImageFilter, PadImageFilter);
  
  /**
   * ImageDimension enumeration
   */
  enum { ImageDimension = TInputImage::ImageDimension };
  
protected:
  WrapPadImageFilter() {};
  ~WrapPadImageFilter() {};
  WrapPadImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** 
   * WrapPadImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() 
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );
/**
 * Given an n dimensional list of input region breakpoints in indices
 * and size (where the current region and maximum region for each dimension
 * is encoded in regIndices and regLimit), choose the next input region.
 */ 
  int GenerateNextInputRegion(int *regIndices, int *regLimit, 
			      std::vector<int> indices[], 
			      std::vector<int> sizes[], 
			      InputImageRegionType& outputRegion);
/**
 * Given an n dimensional list of output region breakpoints in indices
 * and size (where the current region and maximum region for each dimension
 * is encoded in regIndices and regLimit), choose the next output region.
 */ 
  int GenerateNextOutputRegion(int *regIndices, int *regLimit, 
			       std::vector<int> indices[], 
			       std::vector<int> sizes[], 
			       OutputImageRegionType& outputRegion);

/**
 * Given the start and end indices of a region, determine how many
 * instances of size fit within the region.  The variable offset provides
 * a way to adjust width of the area while forcing alignment to the
 * start or end location.
 */
  int FindRegionsInArea(int start, int end, int size, int offset);

/**
 * Generate region 0 (inter-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the 
 * starting points and the lengths of the output region directly 
 * corresponding to the input region.  Padding will be on either 
 * side of this region.  The algorithmic complications are necessary
 * to support the streaming interface and multithreading.
 */
  int BuildInterRegions(std::vector<int>& inputRegionStart, 
                        std::vector<int>& outputRegionStart,
                        std::vector<int>& inputRegionSizes, 
                        std::vector<int>& outputRegionSizes,
                        int inputIndex, int outputIndex,
                        int inputSize, int outputSize, int numRegs, 
                        int & regCtr);

/**
 * Generate region 1 (pre-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the 
 * starting points and the lengths of the output region directly 
 * preceding the input region in this dimension.  This may require
 * more than one region be defined if the padding is larger than the
 * size of the input image in this dimension.  Other algorithmic 
 * complications are necessary to support the streaming interface 
 * and multithreading.
 */
  int BuildPreRegions(std::vector<int>& inputRegionStart, 
                      std::vector<int>& outputRegionStart,
                      std::vector<int>& inputRegionSizes, 
                      std::vector<int>& outputRegionSizes,
                      int inputIndex, int outputIndex,
                      int inputSize, int outputSize, int numRegs, 
                      int & regCtr);

/**
 * Generate region 2 (post-region) information.  Based on the indices
 * of the input and the output for this dimension, decide what are the 
 * starting points and the lengths of the output region directly 
 * succeeding the input region in this dimension.  This may require
 * more than one region be defined if the padding is larger than the
 * size of the input image in this dimension.  Other algorithmic 
 * complications are necessary to support the streaming interface 
 * and multithreading.
 */
int BuildPostRegions(std::vector<int>& inputRegionStart, 
                     std::vector<int>& outputRegionStart,
                     std::vector<int>& inputRegionSizes, 
                     std::vector<int>& outputRegionSizes,
                     int inputIndex, int outputIndex,
                     int inputSize, int outputSize, 
                     int numRegs, int & regCtr);

};
  
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWrapPadImageFilter.txx"
#endif

#endif
