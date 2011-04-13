#include <iostream>

#include "itkRingBuffer.h"

/**
 * This test is basically a duplicate of RingBufferImageSetTest with a few
 * additions to test the video specific methods
 */
int itkRingBufferTest ( int argc, char *argv[] )
{

  //
  // Create a new ring buffer
  //
  typedef itk::RingBuffer<itk::Object> RingBufferType;
  RingBufferType::Pointer ringBuffer = RingBufferType::New();

  // Check that the default number of buffers (3) was properly set
  if (ringBuffer->GetNumberOfBuffers() != 3)
    {
    std::cerr << "Failed to allocate 3 buffers for initialization" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Try adding buffers
  //
  ringBuffer->SetNumberOfBuffers(5);
  if (ringBuffer->GetNumberOfBuffers() != 5 || ringBuffer->GetHeadIndex() != 4)
    {
    std::cerr << "Failed to allocate 5 buffers from 3 buffers" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Try removing buffers
  //
  ringBuffer->SetNumberOfBuffers(2);
  if (ringBuffer->GetNumberOfBuffers() != 2 || ringBuffer->GetHeadIndex() != 1)
    {
    std::cerr << "Failed to allocate 2 buffers from 5 buffers" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Try moving the head forward (should wrap back to 0)
  //
  ringBuffer->MoveHeadForward();
  if (ringBuffer->GetHeadIndex() != 0)
    {
    std::cerr << "Failed to wrap Head pointer back to index 0" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Add buffers back up to 4 and verify the head location is 2 (2 new buffers added at tail)
  //
  ringBuffer->SetNumberOfBuffers(4);
  if (ringBuffer->GetNumberOfBuffers() != 4 || ringBuffer->GetHeadIndex() != 2)
    {
    std::cerr << "Failed to add buffers correctly when Head starts at index 0" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Move head "forward" 2 which should put it at 0
  //
  ringBuffer->MoveHead(2);
  if (ringBuffer->GetHeadIndex() != 0)
    {
    std::cerr << "Failed to move Head properly back to index 0" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Remove 2 buffers. Verify that head stays at 0 and length goes back down to 2
  //
  ringBuffer->SetNumberOfBuffers(2);
  if (ringBuffer->GetHeadIndex() != 0 || ringBuffer->GetNumberOfBuffers() != 2)
    {
    std::cerr << "Failed to keep Head at 0 index when removing from tail" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
