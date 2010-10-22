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
#include "JavaCWD.h"

#if defined(_WIN32) && (defined(_MSC_VER) || defined(__MINGW32__))
#include <direct.h>
#include <stdio.h>
#include <stdlib.h>

void JavaCWD::SetCWD(const char* dir)
{
  _chdir(dir);
}

const char* JavaCWD::GetCWD()
{
  static char buffer[4096];
  _getcwd(buffer, 4096);
  return buffer;
}
#else
#include <unistd.h>
void JavaCWD::SetCWD(const char* dir)
{
  chdir(dir);
}

const char* JavaCWD::GetCWD()
{
  static char buffer[4096];
  getcwd(buffer, 4096);
  return buffer;
}
#endif

#if !defined(_WIN32)
# include <dlfcn.h>

int JavaCWD::Load(const char* lib)
{
  return dlopen(lib, RTLD_GLOBAL|RTLD_NOW)? 1:0;
}
#else
int JavaCWD::Load(const char* lib)
{
  return 0;
}
#endif
