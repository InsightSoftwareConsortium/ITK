/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageSource_h
#define itkImageSource_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkImageRegionSplitterBase.h"
#include "itkImageSourceCommon.h"

namespace itk
{

/** \class ImageSource
 *  \brief Base class for all process objects that output image data.
 *
 * ImageSource is the base class for all process objects that output
 * image data. Specifically, this class defines the GetOutput() method
 * that returns a pointer to the output image. The class also defines
 * some internal private data members that are used to manage streaming
 * of data.
 *
 * Memory management in an ImageSource is slightly different than a
 * standard ProcessObject.  ProcessObject's always release the bulk
 * data associated with their output prior to GenerateData() being
 * called. ImageSources default to not releasing the bulk data incase
 * that particular memory block is large enough to hold the new output
 * values.  This avoids unnecessary deallocation/allocation
 * sequences. ImageSource's can be forced to use a memory management
 * model similar to the default ProcessObject behaviour by calling
 * ProcessObject::ReleaseDataBeforeUpdateFlagOn().  A user may want to
 * set this flag to limit peak memory usage during a pipeline update.
 *
 * \ingroup DataSources
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Developer/ImageSource,Produce an image programmatically.}
 * \endwiki
 */
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT ImageSource
  : public ProcessObject, private ImageSourceCommon
{
public:
  /** Standard class typedefs. */
  typedef ImageSource                Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  typedef Superclass::DataObjectIdentifierType       DataObjectIdentifierType;
  typedef Superclass::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSource, ProcessObject);

  /** Some convenient typedefs. */
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType  OutputImagePixelType;

  /** ImageDimension constant */
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Get the output data of this process object.  The output of this
   * function is not valid until an appropriate Update() method has
   * been called, either explicitly or implicitly.  Both the filter
   * itself and the data object have Update() methods, and both
   * methods update the data.  Here are three ways to use
   * GetOutput() and make sure the data is valid.  In these
   * examples, \a image is a pointer to some Image object, and the
   * particular ProcessObjects involved are filters.  The same
   * examples apply to non-image (e.g. Mesh) data as well.
   *
   * \code
   *   anotherFilter->SetInput( someFilter->GetOutput() );
   *   anotherFilter->Update();
   * \endcode
   *
   * In this situation, \a someFilter and \a anotherFilter are said
   * to constitute a \b pipeline.
   *
   * \code
   *   image = someFilter->GetOutput();
   *   image->Update();
   * \endcode
   *
   * \code
   *   someFilter->Update();
   *   image = someFilter->GetOutput();
   * \endcode
   * (In the above example, the two lines of code can be in
   * either order.)
   *
   * Note that Update() is not called automatically except within a
   * pipeline as in the first example.  When \b streaming (using a
   * StreamingImageFilter) is activated, it may be more efficient to
   * use a pipeline than to call Update() once for each filter in
   * turn.
   *
   * For an image, the data generated is for the requested
   * Region, which can be set using ImageBase::SetRequestedRegion().
   * By default, the largest possible region is requested.
   *
   * For Filters which have multiple outputs of different types, the
   * GetOutput() method assumes the output is of OutputImageType. For
   * the GetOutput(unsigned int) method, a dynamic_cast is performed
   * incase the filter has outputs of different types or image
   * types. Derived classes should have names get methods for these
   * outputs.
   */
  OutputImageType * GetOutput();
  const OutputImageType * GetOutput() const;

  OutputImageType * GetOutput(unsigned int idx);

  /** Graft the specified DataObject onto this ProcessObject's output.
   * This method grabs a handle to the specified DataObject's bulk
   * data to use as its output's own bulk data. It also copies the
   * region ivars (RequestedRegion, BufferedRegion, LargestPossibleRegion)
   * and meta-data (Spacing, Origin, Direction) from the
   * specified data object into this filter's output data object. Most
   * importantly, however, it leaves the Source ivar untouched so the
   * original pipeline routing is intact. This method is used when a
   * process object is implemented using a mini-pipeline which is
   * defined in its GenerateData() method.  The usage is:
   *
   * \code
   *    // Setup the mini-pipeline to process the input to this filter
   *    // The input is not connected to the pipeline.
   *    typename InputImageType::Pointer input = InputImageType::New();
   *    input->Graft( const_cast< InputImageType * >( this->GetInput() );
   *    firstFilterInMiniPipeline->SetInput( input );
   *
   *    // setup the mini-pipeline to calculate the correct regions
   *    // and write to the appropriate bulk data block
   *    lastFilterInMiniPipeline->GraftOutput( this->GetOutput() );
   *
   *    // execute the mini-pipeline
   *    lastFilterInMiniPipeline->Update();
   *
   *    // graft the mini-pipeline output back onto this filter's output.
   *    // this is needed to get the appropriate regions passed back.
   *    this->GraftOutput( lastFilterInMiniPipeline->GetOutput() );
   * \endcode
   *
   * For proper pipeline execution, a filter using a mini-pipeline
   * must implement the GenerateInputRequestedRegion(),
   * GenerateOutputRequestedRegion(), GenerateOutputInformation() and
   * EnlargeOutputRequestedRegion() methods as necessary to reflect
   * how the mini-pipeline will execute (in other words, the outer
   * filter's pipeline mechanism must be consistent with what the
   * mini-pipeline will do).
   *  */
  virtual void GraftOutput(DataObject *output);

  /** Graft the specified data object onto this ProcessObject's named
   * output. This is similar to the GraftOutput method except it
   * allows you to specify which output is affected.
   * See the GraftOutput for general usage information.
   */
  virtual void GraftOutput(const DataObjectIdentifierType & key, DataObject *output);

  /** Graft the specified data object onto this ProcessObject's idx'th
   * output. This is similar to the GraftOutput method except it
   * allows you to specify which output is affected. The specified index
   * must be a valid output number (less than
   * ProcessObject::GetNumberOfIndexedOutputs()). See the GraftOutput for
   * general usage information. */
  virtual void GraftNthOutput(unsigned int idx, DataObject *output);

  /** Make a DataObject of the correct type to used as the specified
   * output.  Every ProcessObject subclass must be able to create a
   * DataObject that can be used as a specified output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.  DataObject::DisconnectPipeline, disconnects a data object
   * from being an output of its current source.  When the data object
   * is disconnected, the ProcessObject needs to construct a replacement
   * output data object so that the ProcessObject is in a valid state.
   * So DataObject::DisconnectPipeline eventually calls
   * ProcessObject::MakeOutput. Note that MakeOutput always returns a
   * SmartPointer to a DataObject. If a subclass of ImageSource has
   * multiple outputs of different types, then that class must provide
   * an implementation of MakeOutput(). */
  virtual ProcessObject::DataObjectPointer MakeOutput(ProcessObject::DataObjectPointerArraySizeType idx) ITK_OVERRIDE;
  virtual ProcessObject::DataObjectPointer MakeOutput(const ProcessObject::DataObjectIdentifierType &) ITK_OVERRIDE;

protected:
  ImageSource();
  virtual ~ImageSource() ITK_OVERRIDE {}

  /** A version of GenerateData() specific for image processing
   * filters.  This implementation will split the processing across
   * multiple threads. The buffer is allocated by this method. Then
   * the BeforeThreadedGenerateData() method is called (if
   * provided). Then, a series of threads are spawned each calling
   * ThreadedGenerateData(). After all the threads have completed
   * processing, the AfterThreadedGenerateData() method is called (if
   * provided). If an image processing filter cannot be threaded, the
   * filter should provide an implementation of GenerateData(). That
   * implementation is responsible for allocating the output buffer.
   * If a filter an be threaded, it should NOT provide a
   * GenerateData() method but should provide a ThreadedGenerateData()
   * instead.
   *
   * \sa ThreadedGenerateData() */
  virtual void GenerateData() ITK_OVERRIDE;

  /** If an imaging filter can be implemented as a multithreaded
   * algorithm, the filter will provide an implementation of
   * ThreadedGenerateData(). This superclass will automatically split
   * the output image into a number of pieces, spawn multiple threads,
   * and call ThreadedGenerateData() in each thread. Prior to spawning
   * threads, the BeforeThreadedGenerateData() method is called. After
   * all the threads have completed, the AfterThreadedGenerateData()
   * method is called. If an image processing filter cannot support
   * threading, that filter should provide an implementation of the
   * GenerateData() method instead of providing an implementation of
   * ThreadedGenerateData().  If a filter provides a GenerateData()
   * method as its implementation, then the filter is responsible for
   * allocating the output data.  If a filter provides a
   * ThreadedGenerateData() method as its implementation, then the
   * output memory will allocated automatically by this superclass.
   * The ThreadedGenerateData() method should only produce the output
   * specified by "outputThreadRegion"
   * parameter. ThreadedGenerateData() cannot write to any other
   * portion of the output image (as this is responsibility of a
   * different thread).
   *
   * \sa GenerateData(), SplitRequestedRegion() */
  virtual void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId);

  /** The GenerateData method normally allocates the buffers for all of the
   * outputs of a filter. Some filters may want to override this default
   * behavior. For example, a filter may have multiple outputs with
   * varying resolution. Or a filter may want to process data in place by
   * grafting its input to its output. */
  virtual void AllocateOutputs();

  /** If an imaging filter needs to perform processing after the buffer
   * has been allocated but before threads are spawned, the filter can
   * can provide an implementation for BeforeThreadedGenerateData(). The
   * execution flow in the default GenerateData() method will be:
   *      1) Allocate the output buffer
   *      2) Call BeforeThreadedGenerateData()
   *      3) Spawn threads, calling ThreadedGenerateData() in each thread.
   *      4) Call AfterThreadedGenerateData()
   * Note that this flow of control is only available if a filter provides
   * a ThreadedGenerateData() method and NOT a GenerateData() method. */
  virtual void BeforeThreadedGenerateData() {}

  /** If an imaging filter needs to perform processing after all
   * processing threads have completed, the filter can can provide an
   * implementation for AfterThreadedGenerateData(). The execution
   * flow in the default GenerateData() method will be:
   *      1) Allocate the output buffer
   *      2) Call BeforeThreadedGenerateData()
   *      3) Spawn threads, calling ThreadedGenerateData() in each thread.
   *      4) Call AfterThreadedGenerateData()
   * Note that this flow of control is only available if a filter provides
   * a ThreadedGenerateData() method and NOT a GenerateData() method. */
  virtual void AfterThreadedGenerateData() {}

  /** \brief Returns the default image region splitter
   *
   * This is an adapter function from the private common base class to
   * the interface of this class.
   */
  static const ImageRegionSplitterBase* GetGlobalDefaultSplitter()
  {
    return ImageSourceCommon::GetGlobalDefaultSplitter();
  }

  /** \brief Get the image splitter to split the image for multi-threading.
   *
   * The Splitter object divides the image into regions for threading
   * or streaming. The algorithms on how to split an images are
   * separated into class so that they can be easily be reused. When
   * deriving from this class to write a filter consideration to the
   * algorithm used to divide the image should be made. If a change is
   * desired this method should be overridden to return the
   * appropriate object.
   */
  virtual const ImageRegionSplitterBase* GetImageRegionSplitter() const;

  /** Split the output's RequestedRegion into "pieces" pieces, returning
   * region "i" as "splitRegion". This method is called concurrently
   * "pieces" times. The  regions must not overlap. The method returns the number
   * of pieces that the routine is capable of splitting the output RequestedRegion,
   * i.e. return value is less than or equal to "pieces".
   *
   * To override the algorithm used split the image this method should
   * no longer be overridden. Instead, the algorithm should be
   * implemented in a ImageRegionSplitterBase class, and the
   * GetImageRegionSplitter should overridden to return the splitter
   * object with the desired algorithm.
   *
   * \sa GetImageRegionSplitter
   **/
  virtual
  unsigned int SplitRequestedRegion(unsigned int i, unsigned int pieces, OutputImageRegionType & splitRegion);

  /** Static function used as a "callback" by the MultiThreader.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to ThreadedGenerateData(). */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback(void *arg);

  /** Internal structure used for passing image data into the threading library
    */
  struct ThreadStruct {
    Pointer Filter;
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageSource);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSource.hxx"
#endif

#endif
