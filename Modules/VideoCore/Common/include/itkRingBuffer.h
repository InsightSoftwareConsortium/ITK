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
#ifndef __itkRingBuffer_h
#define __itkRingBuffer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/** \class RingBuffer
 *  \brief Templated ring buffer for holding anything
 *
 * This ring buffer can hold any type of class
 */

template< class TElementType >
class ITK_EXPORT RingBuffer:public Object
{

public:

  /**-TYPEDEFS---------------------------------------------------------------*/

  /** Standard class typedefs */
  typedef RingBuffer                  Self;
  typedef Object                      Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Contained type */
  typedef TElementType ElementType;

  /** Type for returning number of buffers */
  typedef unsigned int SizeValueType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RingBuffer, Object);

  /** Set the number of buffers
   * WARNING: If the size is set smaller than the current buffer size, the tail
   * of the buffer will be chopped off */
  void SetNumberOfBuffers(SizeValueType sz);

  /** Get the buffer size */
  SizeValueType GetNumberOfBuffers();

  /** Move the Head pointer along the ring using the given offset */
  void MoveHead(int offset);

  /** Convenience methods for moving Head +/- 1 */
  void MoveHeadForward();
  void MoveHeadBackward();

  /** Report whether or not the indicated buffer is full */
  bool BufferIsFull(int offset);

  /** Report the current position of Head (mostly used for testing) */
  unsigned int GetHeadIndex() { return this->m_HeadIndex; }

  /** Access the data from the indicated buffer */
  typename ElementType::Pointer GetBufferContents(int offset);

  /** Set the buffer contents of a buffer */
  void SetBufferContents(int offset, ElementType* element);

protected:

  /**-PROTECTED METHODS------------------------------------------------------*/
  RingBuffer();
  virtual ~RingBuffer(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Get the proper buffer index from an offset */
  unsigned int GetOffsetBufferIndex(int offset);

  /**-PROTECTED MEMBERS------------------------------------------------------*/

  /** Vector of pointers to elements */
  std::vector< typename ElementType::Pointer > m_PointerVector;

  /** Pointer to the current active buffer */
  unsigned int m_HeadIndex;

private:
  RingBuffer(const Self &); // purposely not implemented
  void operator=(const Self &);     // purposely not implemented


};  // end RingBuffer class

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkRingBuffer.txx"
#endif

#endif
