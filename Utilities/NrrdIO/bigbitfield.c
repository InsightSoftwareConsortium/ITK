/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.
 
  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:
 
  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.
 
  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.
 
  3. This notice may not be removed or altered from any source distribution.
*/

#include <stdio.h>

#ifdef _WIN32
typedef unsigned __int64 nio_ullong;
#else
typedef unsigned long long nio_ullong;
#endif

typedef union {
  double d;
  struct {
    nio_ullong bogus0 : 60;
    unsigned int bogus1 : 2;
    unsigned int bogus2 : 2;
  } c;
} nio_test;

int
main(int argc, char *argv[]) {
  
  printf("hello, world.\n");
  return 0;
}
