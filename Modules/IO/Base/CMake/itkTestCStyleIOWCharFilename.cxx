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
/*
   This file tests whether we have _wopen and the like
*/

#include <io.h> // for _wopen
#include <fcntl.h> // for _O_RDONLY
#include <stdio.h> // for _wfopen

int main()
{
  _wopen( L"tmptest.txt", _O_RDONLY );
  _wfopen( L"tmptest.txt", L"r" );
  _wunlink( L"tmptest.txt" );
  return 0;
}
