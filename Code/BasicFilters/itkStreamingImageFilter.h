/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStreamingImageFilter.h
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
#ifndef __itkStreamingImageFilter_h
#define __itkStreamingImageFilter_h

#include "itkImageSource.h"
#include "itkImageRegionSplitter.h"

namespace itk
{

/** \class StreamingImageFilter
 * \brief Pipeline object to control data streaming for large data processing.
 *
 * StreamingImageFilter is a pipeline object that allows the user to control
 * how data is pulled through the pipeline.  To generate its
 * OutputRequestedRegion, this filter will divide the output into several
 * pieces (controlled by SetNumberOfStreamDivisions), and call the upstream
 * pipeline for each piece, tiling the individual outputs into one large
 * output. This reduces the memory footprint for the application since
 * each filter does not have to process the entire dataset at once.
 * This filter will produce the entire output as one image, but the upstream
 * filters will do their processing in pieces.
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT StreamingImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef StreamingImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage, TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(StreamingImageFilter,ImageToImageFilter);

  /** 
   * Some typedefs for the input and output.
   */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType; 
  typedef typename OutputImageType::PixelType OutputImagePixelType; 

  /**
   * SmartPointer to a region splitting object
   */
  enum { InputImageDimension = InputImageType::ImageDimension };
  typedef ImageRegionSplitter<InputImageDimension> SplitterType;
  typedef typename SplitterType::Pointer RegionSplitterPointer;
  
  /**
   * Set the number of pieces to divide the input.  The upstream pipeline
   * will be executed this many times.
   */
  itkSetMacro(NumberOfStreamDivisions,unsigned int);

  /**
   * Get the number of pieces to divide the input. The upstream pipeline
   * will be executed this many times.
   */
  itkGetConstReferenceMacro(NumberOfStreamDivisions,unsigned int);

  /**
   * Set the helper class for dividing the input into chunks.
   */
  itkSetObjectMacro(RegionSplitter, SplitterType);

  /**
   * Get the helper class for dividing the input into chunks.
   */
  itkGetObjectMacro(RegionSplitter, SplitterType);

  /**
   * Override UpdateOutputData() from ProcessObject to divide upstream
   * updates into pieces. This filter does not have a GenerateData()
   * or ThreadedGenerateData() method.  Instead, all the work is done
   * in UpdateOutputData() since it must update a little, execute a little,
   * update some more, execute some more, etc.
   */
  virtual void UpdateOutputData(DataObject *output);
  
protected:
  StreamingImageFilter();
  ~StreamingImageFilter();
  StreamingImageFilter(const StreamingImageFilter&) {};
  void operator=(const StreamingImageFilter&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  unsigned int m_NumberOfStreamDivisions;

  RegionSplitterPointer m_RegionSplitter;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStreamingImageFilter.txx"
#endif

#endif
