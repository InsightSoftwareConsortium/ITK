/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkRingBuffer_h
#define itkRingBuffer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkIntTypes.h"

namespace itk
{
/**
 *\class RingBuffer
 *  \brief Templated ring buffer for holding anything
 *
 * This ring buffer can hold any type of itk class that supports smart
 * pointers. A HEAD pointer is maintained and the buffer pointers can be
 * accessed in order using either positive or negative offsets. The HEAD
 * pointer can also be moved forward or backward in the ring.
 *
 * \ingroup ITKVideoCore
 */

template <typename TElement>
class ITK_TEMPLATE_EXPORT RingBuffer : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RingBuffer);

  /**-TYPEDEFS---------------------------------------------------------------*/

  /** Standard class type aliases */
  using Self = RingBuffer;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Contained type */
  using ElementType = TElement;
  using ElementPointer = typename ElementType::Pointer;

  using SizeValueType = ::itk::SizeValueType;
  using OffsetValueType = ::itk::OffsetValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RingBuffer, Object);

  /** Set the number of buffers
   * WARNING: If the size is set smaller than the current buffer size, the tail
   * of the buffer will be chopped off */
  void
  SetNumberOfBuffers(SizeValueType n);

  /** Get the buffer size */
  SizeValueType
  GetNumberOfBuffers();

  /** Move the Head pointer along the ring using the given offset */
  void
  MoveHead(OffsetValueType offset);

  /** Convenience methods for moving Head +/- 1 */
  void
  MoveHeadForward();
  void
  MoveHeadBackward();

  /** Report whether or not the indicated buffer is full */
  bool
  BufferIsFull(OffsetValueType offset);

  /** Report the current position of Head (mostly used for testing) */
  SizeValueType
  GetHeadIndex()
  {
    return this->m_HeadIndex;
  }

  /** Access the data from the indicated buffer */
  typename ElementType::Pointer
  GetBufferContents(OffsetValueType offset);

  /** Set the buffer contents of a buffer */
  void
  SetBufferContents(OffsetValueType offset, ElementPointer element);

protected:
  /**-PROTECTED METHODS------------------------------------------------------*/
  RingBuffer();
  ~RingBuffer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Get the proper buffer index from an offset */
  OffsetValueType
  GetOffsetBufferIndex(OffsetValueType offset);

  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** Pointer to the current active buffer */
  SizeValueType m_HeadIndex{ 0 };

  /** Vector of pointers to elements */
  std::vector<ElementPointer> m_PointerVector;
}; // end RingBuffer class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRingBuffer.hxx"
#endif

#endif
