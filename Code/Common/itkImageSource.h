/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
  typedef TOutputImage OutputImage;
  typedef typename OutputImage::Pointer OutputImagePointer;
  typedef typename OutputImage::Region OutputImageRegion;
  typedef typename OutputImage::PixelType OutputImagePixelType;
  
  /** 
   * Get the image output of this process object. 
   */
  OutputImagePointer GetOutput();
  OutputImagePointer GetOutput(unsigned int idx);

  /** 
   * Set the image output of this process object. 
   */
  void SetOutput(OutputImage *output);

protected:
  ImageSource();
  virtual ~ImageSource() {}
  ImageSource(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
  /** 
   * What is the input requested region that is required to produce the
   * output requested region? By default, the largest possible region is
   * always required but this is overridden in many subclasses. For instance,
   * for an image processing filter where an output pixel is a simple function
   * of an input pixel, the input requested region will be set to the output
   * requested region.  For an image processing filter where an output pixel
   * is a function of the pixels in a neighborhood of an input pixel, then
   * the input requested region will need to be larger than the output
   * requested region (to avoid introducing artificial boundary conditions).
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion();
  
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
  void ThreadedGenerateData(const OutputImageRegion& outputRegionForThread,
                            int threadId );

  /**
   * Split the output's RequestedRegion into "num" pieces, returning
   * region "i" as "splitRegion". This method is called "num" times. The
   * regions must not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the output RequestedRegion,
   * i.e. return value is less than or equal to "num".
   */
  virtual
  int SplitRequestedRegion(int i, int num, OutputImageRegion& splitRegion);

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
  
