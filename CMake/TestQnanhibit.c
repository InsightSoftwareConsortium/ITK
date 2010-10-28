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
#include <stdio.h>
#include <float.h>

int
main(int argc, char *argv[])
{
   const char * const me=argv[0];
   const float zero=0.0F;
   union {
     float flt32bit;
     int   int32bit;
   } qnan;

   if (sizeof(float) != sizeof(int))
     {
     fprintf(stderr, "%s: MADNESS:  sizeof(float)=%d != sizeof(int)=%d\n",
           me, (int)sizeof(float), (int)sizeof(int));
     return -1;
     }
   qnan.flt32bit=zero/zero;
   printf("-DTEEM_QNANHIBIT=%d\n", (qnan.int32bit >> 22) & 1);
   return (int)((qnan.int32bit >> 22) & 1);
}
