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
#include "itkTimeStamp.h"
#include "itkMultiThreader.h"


// A helper struct for the test, the idea is to have one timestamp per thread.
// To ease the writing of the test, we use  MultiThreader::SingleMethodExecute
// with an array of timestamps in the shared data
typedef struct {
  std::vector<itk::TimeStamp> timestamps;
  std::vector<unsigned long> counters;
} TimeStampTestHelper;

ITK_THREAD_RETURN_TYPE modified_function( void *ptr )
{
  typedef itk::MultiThreader::ThreadInfoStruct  ThreadInfoType;

  ThreadInfoType * infoStruct = static_cast< ThreadInfoType * >( ptr );

  const itk::ThreadIdType threadId = infoStruct->ThreadID;

  TimeStampTestHelper * helper =
    static_cast< TimeStampTestHelper * >( infoStruct->UserData );

  helper->timestamps[threadId].Modified();
  helper->counters[threadId]++;

  return ITK_THREAD_RETURN_VALUE;
}

int itkTimeStampTest(int, char*[])
{
  bool success = true;

  try
    {
    TimeStampTestHelper helper;

    // Set up the multithreader
    itk::MultiThreader::Pointer multithreader = itk::MultiThreader::New();
    multithreader->SetNumberOfThreads( ITK_MAX_THREADS+10 );// this will be clamped
    multithreader->SetSingleMethod( modified_function, &helper);

    // Test that the number of threads has actually been clamped
    const itk::ThreadIdType numberOfThreads = multithreader->GetNumberOfThreads();

    if( numberOfThreads > ITK_MAX_THREADS )
      {
      std::cerr << "[TEST FAILED]" << std::endl;
      std::cerr << "numberOfThreads > ITK_MAX_THREADS" << std::endl;
      return EXIT_FAILURE;
      }

    // Set up the helper class
    helper.counters.resize( numberOfThreads );
    helper.timestamps.resize( numberOfThreads );
    for(itk::ThreadIdType k=0; k < numberOfThreads; k++)
    {
       helper.counters[k] = 0;
    }

    // Declare an array to test whether the all modified times have
    // been used
    std::vector<bool> istimestamped( numberOfThreads );

    // Call Modified once  on any object to make it up-to-date
    multithreader->Modified();

    const itk::ModifiedTimeType init_mtime = multithreader->GetMTime();
    std::cout << "init_mtime: " << init_mtime << std::endl;

    itk::ModifiedTimeType prev_mtime = init_mtime;

    const unsigned int num_exp = 500;

    for( unsigned int i = 0; i < num_exp; i++ )
      {
      multithreader->SingleMethodExecute();

      itk::ModifiedTimeType min_mtime = helper.timestamps[0].GetMTime();
      itk::ModifiedTimeType max_mtime = helper.timestamps[0].GetMTime();
      for(itk::ThreadIdType k=0; k < numberOfThreads; k++)
        {
        const itk::ModifiedTimeType & mtime = helper.timestamps[k].GetMTime();
        if ( mtime > max_mtime )
          {
          max_mtime = mtime;
          }
        else if ( mtime < min_mtime )
          {
          min_mtime = mtime;
          }

        // initialiaze the array to false
        istimestamped[k]=false;
        }

      bool iter_success =
             ( ((max_mtime-prev_mtime ) == numberOfThreads) &&
               (min_mtime==prev_mtime+1) );

      if ( iter_success )
        {
        for(itk::ThreadIdType k=0; k < numberOfThreads; k++)
          {
          // Test whether the all modified times have
          // been used
          const itk::ModifiedTimeType index = helper.timestamps[k].GetMTime()-min_mtime;

          if ( istimestamped[index] == true )
            {
            iter_success = false;
            std::cerr<<helper.timestamps[k].GetMTime()<<" was used twice as a timestamp!"<<std::endl;
            }
          else
            {
            istimestamped[index] = true;
            }

          // Test the counters
          if( helper.counters[k] != i+1 )
            {
            iter_success = false;
            std::cerr << "counter[" << k << "] = " << helper.counters[k];
            std::cerr << " at iteration " << i << std::endl;
            }
          }
      }

      if( !iter_success )
        {
        std::cerr << "[Iteration " << i << " FAILED]" << std::endl;
        std::cerr << "max_mtime       : " << max_mtime << std::endl;
        std::cerr << "min_mtime       : " << min_mtime << std::endl;
        std::cerr << "prev_mtime      : " << prev_mtime << std::endl;
        std::cerr << "num_threads     : " << numberOfThreads << std::endl;
        std::cerr << "max - prev mtime: " << max_mtime - prev_mtime << std::endl;
        std::cerr << std::endl;
        success = false;

        // Note that in a more general setting,  (max_mtime-prev_mtime)>numberOfThreads
        // might be a normal case since the modified time of a time stamp
        // is global. If a new itk object is created this will also increment
        // the time. In our specific test, there's no reason for another ITK object to be
        // modified though
        }

      prev_mtime = max_mtime;
      }
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << "[TEST FAILED]" << std::endl;
    std::cerr << "Exception caught: "<< e << std::endl;
    return EXIT_FAILURE;
    }

  if (!success)
    {
    std::cerr << "[TEST FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[TEST PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
