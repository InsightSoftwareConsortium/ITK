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
#ifndef __itkRingBufferImageSet_h
#define __itkRingBufferImageSet_h

#include "itkImage.h"
#include "itkImageSource.h"
#include "itkDefaultConvertPixelTraits.h"

/** For now, use itk::VideoIOBase internally. This may change if further
 * hierarchy is added in the form of itk::ImageSetSource */
#include "itkVideoIOBase.h"
 
namespace itk
{
/** \class RingBufferImageSet
 *  \brief Templated ring buffer for streaming n-dimensional images
 *
 * This class holds a set of buffers, each able to hold the data for a single
 * itk::Image. It is templated over the parameters for the images it will
 * contain and the number of buffers it will hold.
 *
 * The "Ring" nature of the buffer is such that the buffers are ordered
 * relative to a Head pointer. Head can be advanced or reversed and will loop
 * forward from the end of the block of buffers back to the beginning and
 * backward from beginning to end.
 *
 * NOTE: For now, we don't implement spacial streaming (Regions). This will be
 * addressed in the final version.
 */

template< class TPixel,
          unsigned int VImageDimension = 2,
          unsigned int VNumberOfBuffers = 3 >
class ITK_EXPORT RingBufferImageSet:
  public ImageSource< Image< TPixel, VImageDimension > >
{

public:

  /**-TYPEDEFS---------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef RingBufferImageSet                  Self;
  typedef TPixel                              PixelType;
  typedef Image< PixelType, VImageDimension > ImageType;
  typedef ImageSource< ImageType >            Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;
  typedef WeakPointer< const Self >           ConstWeakPointer;

  /** Pixel conversion typedefs */
  typedef DefaultConvertPixelTraits<PixelType> ConvertPixelTraits;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RingBufferImageSet, ImageSource);

  /** Dimension of the internal images */
  itkStaticConstMacro(ImageDimension, unsigned int, VImageDimension);

  /** Number of image buffers */
  itkStaticConstMacro(NumberOfBuffers, unsigned int, VNumberOfBuffers);

  /** Typedefs for returning image parameters */
  typedef typename ImageType::IndexType     IndexType;
  typedef typename ImageType::OffsetType    OffsetType;
  typedef typename ImageType::SizeType      SizeType;
  typedef typename ImageType::SpacingType   SpacingType;
  typedef typename ImageType::PointType     PointType;
  typedef typename ImageType::DirectionType DirectionType;

  /** Container used to store pixels for each image */
  typedef ImportImageContainer< itk::SizeValueType,
                                PixelType >         PixelContainer;
  typedef typename PixelContainer::Pointer          PixelContainerPointer;
  typedef typename PixelContainer::ConstPointer     PixelContainerConstPointer;



  /**-PUBLIC METHODS---------------------------------------------------------*/

  /** Empty buffers and reset HEAD to first buffer */
  void Initialize();

  /** Allocate memory for the image buffers */
  void Allocate();

  /** Deallocate memory from the image buffers */
  void Deallocate();

  /** Image dimension. The dimension of an image is fixed at construction. */
  static unsigned int GetImageDimension() { return VImageDimension; }

  /** Get the number of buffers */
  static unsigned int GetNumberOfBuffers() { return VNumberOfBuffers; }

  /** Move the Head pointer along the ring using the given offset */
  void MoveHead(int offset);

  /** Convenience methods for moving Head +/- 1 */
  void MoveHeadForward();
  void MoveHeadBackward();

  /** Fetch the next image into the current Head buffer */
  void BufferNextImage();

  /** Set the internal VideoIOBase pointer. This will generally be called by
   * the object that creates the RingBuffer (e.g. itk::VideoFileReader) */
  void SetVideoIO(VideoIOBase* videoIO);

  /** Report whether or not the indicated buffer is valid */
  bool BufferIsValid(int offset);


  /** Return an itk::Image created using the data in the specified buffer.
   * The buffer is specified using the provided offset forward or backward. The
   * offset will be taken modulo the number of buffers
   *
   * This method is designed primarily to be used inside of a
   * VideoToVideoFilter. The idea is that at each step of the temporal
   * streaming process, the filter should be able to access any of the buffered
   * frames without having to call Update. This method creates a copy of the
   * buffered data (converting to the output pixel type if necessary) and
   * returns an image containing that new copy.
   */
  typename ImageType::Pointer GetBufferedImage(int offset);
  void GetBufferedImage(int offset, ImageType*);


  /** Set up the output information */
  virtual void GenerateOutputInformation();

  /** Overwrite the default UpdateOutputData to always be out of date */
  virtual void UpdateOutputData( DataObject* );

  /** The Source API is borrowed from DataObject. This allows the ImageSet to
   * behave both like a process object and like a data object so that it can be
   * generated using a VideoFileReader */
  bool ConnectSource(ProcessObject *s, unsigned long idx) const;
  bool DisconnectSource(ProcessObject *s, unsigned long idx) const;


protected:

  /**-PROTECTED METHODS------------------------------------------------------*/
  RingBufferImageSet();
  virtual ~RingBufferImageSet();
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Get the proper buffer index from an offset */
  unsigned int GetOffsetBufferIndex(int offset);

  /** Convert buffer for output */
  void DoConvertBuffer(unsigned int bufferIndex, ImageType* output);

  /** Prepare an image for output, but don't fill it */
  void PrepareOutputImage(ImageType* output);

  /** Advance to the next image in the stream and set the output to a
   * (converted) copy of the data in the current Head buffer */
  virtual void GenerateData();

  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** Array of buffers for holding the image data */
  std::vector<PixelContainerPointer> m_BufferArray;

  /** Array of flags to indicate when given buffers contain valid data */
  std::vector<bool> m_BufferValidFlags;

  /** Pointer to the current active buffer */
  PixelContainer* m_Head;
  unsigned int    m_HeadIndex;

  /** VideoIOBase used to retrieve images. This may be changed if more
   * hierarchy is added to support general ImageSet sources */
  VideoIOBase::Pointer m_VideoIO;

  /** Flag to store whether or not memory has been allocated */
  bool m_Allocated;

  /** Flag to store whether or not the pixel type needs to be converted */
  bool m_PixelConversionNeeded;

  /** Who generated this data? - This is borrowed from DataObject and is
   * used to force generation if this is the output of a VideoFileReader */
  mutable WeakPointer< ProcessObject > m_Source;
  mutable unsigned int                 m_SourceOutputIndex;

private:
  RingBufferImageSet(const Self &); // purposely not implemented
  void operator=(const Self &);     // purposely not implemented


};  // end RingBufferImageSet class

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkRingBufferImageSet.txx"
#endif

#endif
