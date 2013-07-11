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
#include "itkThreadedImageRegionPartitioner.h"

/*
 * Main test entry function
 */
int itkThreadedImageRegionPartitionerTest(int , char* [])
{
  const unsigned int Dimension = 2;

  typedef itk::ThreadedImageRegionPartitioner< Dimension > ThreadedImageRegionPartitionerType;
  ThreadedImageRegionPartitionerType::Pointer threadedImageRegionPartitioner = ThreadedImageRegionPartitionerType::New();

  typedef ThreadedImageRegionPartitionerType::DomainType ImageRegionType;

  typedef ImageRegionType::SizeType   SizeType;
  typedef ImageRegionType::IndexType  IndexType;

  SizeType size;
  IndexType index;

  size.Fill(97);
  index.Fill(4);

  ImageRegionType completeRegion;

  completeRegion.SetSize( size );
  completeRegion.SetIndex( index );

  // Define the expected results
  ImageRegionType expectedRegion;
  std::vector< ImageRegionType > expectedSubRegions;
  size[1] = 25;
  expectedRegion.SetIndex( index );
  expectedRegion.SetSize( size );
  expectedSubRegions.push_back( expectedRegion );

  index[1] = 29;
  expectedRegion.SetIndex( index );
  expectedSubRegions.push_back( expectedRegion );

  index[1] = 54;
  expectedRegion.SetIndex( index );
  expectedSubRegions.push_back( expectedRegion );

  index[1] = 79;
  expectedRegion.SetIndex( index );
  size[1]  = 22;
  expectedRegion.SetSize( size );
  expectedSubRegions.push_back( expectedRegion );

  const itk::ThreadIdType totalThreads = 4;
  ImageRegionType subRegion;
  for( itk::ThreadIdType i = 0; i < totalThreads; ++i )
    {
    threadedImageRegionPartitioner->PartitionDomain( i,
                                                     totalThreads,
                                                     completeRegion,
                                                     subRegion );
    std::cout << "The resulting subregion for thread: " << i
              << " is : " << subRegion << std::endl;

    if( expectedSubRegions[i] != subRegion )
      {
      std::cerr << "The calculated sub-region, " << subRegion
                << " did not match the expected region: " << expectedSubRegions[i]
                << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
