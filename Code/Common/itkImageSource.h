/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSource.h
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
#ifndef __itkImageSource_h
#define __itkImageSource_h

#include "itkProcessObject.h"

namespace itk
{

/** \class ImageSource
 *  \brief Base class for all process objects that output image data.
 *
 * ImageSource is the base class for all process objects that output
 * image data. Specifically, this class defines the GetOutput() method
 * that returns a pointer to the output image. The class also defines
 * some internal private data memebers that are used to manage streaming
 * of data.
 *
 * \ingroup DataSources
 */
template <class TOutputImage>
class ITK_EXPORT ImageSource : public ProcessObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageSource         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ProcessObject  Superclass;

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
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageSource,ProcessObject);

  /** 
   * Some typedefs.
   */
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  
  /** 
   * Get the image output of this process object. 
   */
  OutputImagePointer GetOutput();
  OutputImagePointer GetOutput(unsigned int idx);

  /** 
   * Set the image output of this process object. 
   */
  void SetOutput(OutputImageType *output);

protected:
  ImageSource();
  virtual ~ImageSource() {}
  ImageSource(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  
  /**
   * A version of GenerateData specific for image processing filters.  This
   * implementation will split the processing across multiple threads. Each
   * thread will call the method ThreadedGenerateData().  If an image
   * processing filter cannot be threaded, the filter should provide
   * an implementation of GenerateData().  If a filter provides GenerateData,
   * then that version GenerateData is responsible for allocating the output
   * data.  If a filter provides a ThreadedGenerateData() instead, then
   * then the output data will be allocated automatically prior to calling
   * ThreadedGenerateData().
   *
   * \sa ThreadedGenerateData()
   */
  virtual void GenerateData();

  /**
   * If an imaging filter can be implemented as a multithreaded algorithm,
   * the filter will provide an implementation of ThreadedGenerateData().
   * This superclass will automatically split the output image into a number
   * of pieces, spawn multiple threads, and call ThreadedGenerateData()
   * in each thread. If an image processing filter cannot support threading,
   * that filter should provide an implementation of the GenerateData()
   * method instead of providing an implementation of ThreadedGenerateData().
   * If a filter provides a GenerateData() method as its implementation, then
   * the filter is responsible for allocating the output data.  If a filter
   * provides a ThreadedGenerateData() method as its implementation, then
   * the output memory will allocated automatically by this superclass.
   * The ThreadedGenerateData() method should only produce the output specified
   * by "outputThreadRegion" parameter. ThreadedGenerateData() cannot write
   * to any other portion of the output image (as this is responsibility of
   * a different thread).
   *
   * \sa GenerateData(), SplitRequestedRegion()
   */
  virtual
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  /**
   * Split the output's RequestedRegion into "num" pieces, returning
   * region "i" as "splitRegion". This method is called "num" times. The
   * regions must not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the output RequestedRegion,
   * i.e. return value is less than or equal to "num".
   */
  virtual
  int SplitRequestedRegion(int i, int num, OutputImageRegionType& splitRegion);

  /**
   * Static function used as a "callback" by the MultiThreader.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to ThreadedGenerateData().
   */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback( void *arg );

  /**
   * Internal structure used for passing image data into the threading library
   */
  struct ThreadStruct
  {
    ImageSource *Filter;
  };
  
private:

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSource.txx"
#endif

#endif
  
