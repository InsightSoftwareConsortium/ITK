/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageToImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageToImage_h
#define __itkFilterImageToImage_h

#include "itkImageSource.h"
#include "itkMultiThreader.h"

namespace itk
{

/** \class FilterImageToImage
 * \brief 
 *
 * FilterImageToImage is the base class for all process objects that output
 * image data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * This class provides the infrastructure for supporting multithreaded
 * processing of images.  If a filter provides an implementation of
 * GenerateData(), the image processing will run in a single thread and the
 * implementation is responsible for allocating its output data.  If a filter
 * provides an implementation of ThreadedGenerateData() instead, the image
 * will be divided into a number of pieces, a number of threads will be
 * spawned, and ThreadedGenerateData() will be called in each thread.  Here,
 * the output memory will be allocated by this superclass prior to calling
 * ThreadedGenerateData().
 *
 * FilterImageToImage provides an implementation of
 * GenerateInputRequestedRegion().  The base assumption to this point in the
 * heirarchy is that a process object would ask for the largest possible
 * region on input in order to produce any output.  Imaging filters,
 * however, can usually answer this question more precisely.  The default
 * implementation of GenerateInputRequestedRegion() in this class is to
 * request an input that matches the size of the requested output.  If a
 * filter requires more input (say a filter that uses neighborhood
 * information) or less input (for instance a magnify filter), then these
 * filters will have to provide another implmentation of this method. By
 * convention, such implementations should call the Superclass' method
 * first.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT FilterImageToImage : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageToImage  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FilterImageToImage,ImageSource);

  /** 
   * Some typedefs.
   */
  typedef TInputImage InputImage;
  typedef typename InputImage::Pointer InputImagePointer;
  typedef typename InputImage::Region InputImageRegion; 
  typedef typename InputImage::PixelType InputImagePixelType; 

  /** 
   * Set the image input of this process object. 
   */
  void SetInput(InputImage *input);

  /** 
   * Get the image input of this process object. 
   */
  InputImagePointer GetInput();
  InputImagePointer GetInput(unsigned int idx);

  /**
   * Get/Set the number of threads to create when executing.
   */
  itkSetClampMacro( NumberOfThreads, int, 1, ITK_MAX_THREADS );
  itkGetMacro( NumberOfThreads, int );

  

protected:
  FilterImageToImage();
  ~FilterImageToImage();
  FilterImageToImage(const FilterImageToImage&) {};
  void operator=(const FilterImageToImage&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  /** 
   * What is the input requested region that is required to produce the
   * output requested region? The base assumption for image processing
   * filters is that the input requested region can be set to match the
   * output requested region.  If a filter requires more input (for instance
   * a filter that uses neighborhoods needs more input than output to avoid
   * introducing artificial boundary conditions) or less input (for instance 
   * a magnify filter) will have to override this method.  In doing so, it
   * should call its superclass' implementation as its first step. Note that
   * this imaging filters operate differently than the classes to this
   * point in the class hierachy.  Up till now, the base assumption has been
   * that the largest possible region will be requested of the input.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion(),
   *     ImageSource::GenerateInputRequestedRegion()
   */
  void GenerateInputRequestedRegion();
  
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
  void GenerateData();

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
  
  // ivars for processing images in multiple threads
  MultiThreader::Pointer m_Threader;
  int m_NumberOfThreads;

 private:
  /**
   * Internal structure used for passing data into the threading library
   */
  struct ThreadStruct
  {
    FilterImageToImage *Filter;
  };
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageToImage.txx"
#endif

#endif
