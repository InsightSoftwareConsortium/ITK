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
#ifndef itkImageTransformer_h
#define itkImageTransformer_h

#include "itkProcessObject.h"
#include "itkImage.h"

namespace itk
{
/** \class ImageTransformer
 *  \brief Base class for all process objects that transform an image into something else.
 *
 * ImageTransformer is the base class for all process objects that transform an
 * image data. Specifically, this class defines the SetInput() method
 * that takes a pointer to the input image. The class also defines
 * some internal private data members that are used to manage streaming
 * of data.
 *
 * Memory management in an ImageTransformer is slightly different than a
 * standard ProcessObject.  ProcessObject's always release the bulk
 * data associated with their output prior to GenerateData() being
 * called. ImageTransformers default to not releasing the bulk data incase
 * that particular memory block is large enough to hold the new output
 * values.  This avoids unnecessary deallocation/allocation
 * sequences. ImageTransformer's can be forced to use a memory management
 * model similar to the default ProcessObject behaviour by calling
 * ProcessObject::ReleaseDataBeforeUpdateFlagOn().  A user may want to
 * set this flag to limit peak memory usage during a pipeline update.
 *
 * \ingroup ITKCommon
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT ImageTransformer:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageTransformer           Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageTransformer, ProcessObject);

  /** Some convenient typedefs. */
  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::Pointer    InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType;
  typedef typename InputImageType::PixelType  InputImagePixelType;

  /** ImageDimension constant */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  virtual void SetInput(const InputImageType *image);
  virtual void SetInput(unsigned int, const TInputImage *image);
  const InputImageType * GetInput() const;
  InputImageType * GetInput();
  const InputImageType * GetInput(unsigned int idx) const;

  /** Push/Pop the input of this process object. These methods allow a
   * filter to model its input vector as a queue or stack.  These
   * routines may not be appropriate for all filters, especially
   * filters with different types of inputs.  These routines follow
   * the semantics of STL.
   *
   * The routines are useful for applications that need to process
   * "rolling" sets of images.  For instance, if an application has 10
   * images and they need to run a filter on images 1, 2, 3, 4, then
   * run the filter on images 2, 3, 4, 5, then run the filter on
   * images 3, 4, 5, 6, the application can accomplish this by popping
   * an input off the front of the input list and push a new image
   * onto the back of input list.  Again, this only makes sense for
   * filters that single type of input.
   *
   * Other uses are also possible. For a single input filter, pushing
   * and popping inputs allow the application to temporarily replace
   * an input to a filter.
   */
  virtual void PushBackInput(const InputImageType *image);
  virtual void PopBackInput() ITK_OVERRIDE;
  virtual void PushFrontInput(const InputImageType *image);
  virtual void PopFrontInput() ITK_OVERRIDE;

protected:
  ImageTransformer();
  virtual ~ImageTransformer() ITK_OVERRIDE {}

  /** The image transformer is assumed to need the whole input.
   *
   * This implementation of GenerateInputRequestedRegion() only
   * processes the inputs that are a subclass of the
   * ImageBase<InputImageDimension>.  If an input is another type of
   * DataObject (including an Image of a different dimension), they
   * are skipped by this method. The subclasses of ImageToImageFilter
   * are responsible for providing an implementation of
   * GenerateInputRequestedRegion() when there are multiple inputs of
   * different types.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion(),
   *     ImageSource::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

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
   * ThreadedGenerateData().  This superclass will automatically split
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
   * specified by "inputThreadRegion"
   * parameter. ThreadedGenerateData() cannot write to any other
   * portion of the output image (as this is responsibility of a
   * different thread).
   *
   * \sa GenerateData(), SplitRequestedRegion() */
  virtual
  void ThreadedGenerateData(const InputImageRegionType & inputRegionForThread,
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

  /** Split the input's RequestedRegion into "num" pieces, returning
   * region "i" as "splitRegion". This method is called "num" times. The
   * regions must not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the input RequestedRegion,
   * i.e. return value is less than or equal to "num". */
  virtual
  unsigned int SplitRequestedRegion(unsigned int i, unsigned int num, InputImageRegionType & splitRegion);

  /** Static function used as a "callback" by the MultiThreader.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to ThreadedGenerateData(). */
  static ITK_THREAD_RETURN_TYPE ThreaderCallback(void *arg);

  /**
   * PushBackInput(), PushFrontInput() in the public section force the
   * input to be the type expected by an ImageTransformer. However,
   * these methods end up "hiding" the versions from the superclass
   * (ProcessObject) whose arguments are DataObjects. Here, we re-expose
   * the versions from ProcessObject to avoid warnings about hiding
   * methods from the superclass.
   * NOTE: The same code resides in ImageToImageFilter
   */
  virtual void PushBackInput(const DataObject *input) ITK_OVERRIDE
  { Superclass::PushBackInput(input); }
  virtual void PushFrontInput(const DataObject *input) ITK_OVERRIDE
  { Superclass::PushFrontInput(input); }

  /** Internal structure used for passing image data into the threading library
    */
  struct ThreadStruct {
    Pointer Filter;
  };

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageTransformer);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageTransformer.hxx"
#endif

#endif
