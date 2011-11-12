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
#include <iostream>

#include "itkRingBuffer.h"

/**
 * This test is basically a duplicate of RingBufferImageSetTest with a few
 * additions to test the video specific methods
 */
int itkRingBufferTest( int , char* [] )
{

  //////
  // Test instantiation
  //////
  typedef itk::RingBuffer<itk::Object> RingBufferType;
  RingBufferType::Pointer ringBuffer = RingBufferType::New();

  // Check that the default number of buffers (3) was properly set
  if (ringBuffer->GetNumberOfBuffers() != 3)
    {
    std::cerr << "Failed to allocate 3 buffers for initialization" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Test setting number of buffers and moving Head
  //////

  // Try adding buffers
  ringBuffer->SetNumberOfBuffers(5);
  if (ringBuffer->GetNumberOfBuffers() != 5 || ringBuffer->GetHeadIndex() != 4)
    {
    std::cerr << "Failed to allocate 5 buffers from 3 buffers" << std::endl;
    return EXIT_FAILURE;
    }

  // Try removing buffers
  ringBuffer->SetNumberOfBuffers(2);
  if (ringBuffer->GetNumberOfBuffers() != 2 || ringBuffer->GetHeadIndex() != 1)
    {
    std::cerr << "Failed to allocate 2 buffers from 5 buffers" << std::endl;
    return EXIT_FAILURE;
    }

  // Try moving the head forward (should wrap back to 0)
  ringBuffer->MoveHeadForward();
  if (ringBuffer->GetHeadIndex() != 0)
    {
    std::cerr << "Failed to wrap Head pointer back to index 0" << std::endl;
    return EXIT_FAILURE;
    }

  // Add buffers back up to 4 and verify the head location is 2 (2 new buffers
  // added at tail)
  ringBuffer->SetNumberOfBuffers(4);
  if (ringBuffer->GetNumberOfBuffers() != 4 || ringBuffer->GetHeadIndex() != 2)
    {
    std::cerr << "Failed to add buffers correctly when Head starts at index 0" << std::endl;
    return EXIT_FAILURE;
    }

  // Move head "forward" 2 which should put it at 0
  ringBuffer->MoveHead(2);
  if (ringBuffer->GetHeadIndex() != 0)
    {
    std::cerr << "Failed to move Head properly back to index 0" << std::endl;
    return EXIT_FAILURE;
    }

  // Remove 2 buffers. Verify that head stays at 0 and length goes back down to
  // 2
  ringBuffer->SetNumberOfBuffers(2);
  if (ringBuffer->GetHeadIndex() != 0 || ringBuffer->GetNumberOfBuffers() != 2)
    {
    std::cerr << "Failed to keep Head at 0 index when removing from tail" << std::endl;
    return EXIT_FAILURE;
    }

  // Test looping buffer offset forward
  unsigned int oldHeadIndex = ringBuffer->GetHeadIndex();
  ringBuffer->MoveHead(2*ringBuffer->GetNumberOfBuffers() );
  if (ringBuffer->GetHeadIndex() != oldHeadIndex)
    {
    std::cerr << "Failed to properly loop Head index when moving forward" << std::endl;
    return EXIT_FAILURE;
    }

  // Test looping buffer offset backward
  ringBuffer->MoveHead(-2 * static_cast<int>(ringBuffer->GetNumberOfBuffers()) );
  if (ringBuffer->GetHeadIndex() != oldHeadIndex)
    {
    //DEBUG
    std::cout << "oldHeadIndex = " << oldHeadIndex << ", HeadIndex = " << ringBuffer->GetHeadIndex() << std::endl;
    std::cout << "NumberOfBuffers = " << ringBuffer->GetNumberOfBuffers() << std::endl;

    std::cerr << "Failed to properly loop Head index when moving backward" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Test Setting buffers and full checking
  //////

  // Make sure all buffers are currently invalid
  for (unsigned int i = 0; i < ringBuffer->GetNumberOfBuffers(); ++i)
    {
    if (ringBuffer->BufferIsFull(i) )
      {
      std::cerr << "Incorrectly reported a full buffer for offset " << i << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Create a new Object, add it at Head and make sure it is reported full
  itk::Object::Pointer obj1 = itk::Object::New();
  ringBuffer->SetBufferContents(0, obj1);
  if (!ringBuffer->BufferIsFull(0) )
    {
    std::cerr << "Did not report Head as full after setting contents" << std::endl;
    return EXIT_FAILURE;
    }

  // Try retreiving the object and compare time stamps
  itk::Object::Pointer objOut = ringBuffer->GetBufferContents(0);
  if (obj1->GetTimeStamp() != objOut->GetTimeStamp() )
    {
    std::cerr << "Returned object doesn't match input object" << std::endl;
    return EXIT_FAILURE;
    }

  // Add another object and then check fullness of all buffers
  itk::Object::Pointer obj2 = itk::Object::New();
  ringBuffer->SetBufferContents(-1, obj2);

  // Everything except Head and Head-1 should be empty
  for (unsigned int i = 1; i < ringBuffer->GetNumberOfBuffers()-1; ++i)
    {
    if (ringBuffer->BufferIsFull(i) )
      {
      std::cerr << "Incorrectly reported a full buffer for offset " << i
                << " after filling Head and Head-1" << std::endl;
      return EXIT_FAILURE;
      }
    }
  if (!ringBuffer->BufferIsFull(0) || !ringBuffer->BufferIsFull(-1) )
    {
    std::cerr << "Did not report Head or Head-1 full after setting" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
