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

// I suspect that Purify is not working correctly.  It should report a memory
// leak for this program.

int itkMemoryLeakTest(int, char* [] )
{
  // Leak 10000 bytes of memory, a little at a time.
  for(unsigned int i=0; i < 100; ++i)
    {
    char* leaker = new char[100];
    *leaker = 0; // Prevent unused variable warning.
    }

  return 0;
}
