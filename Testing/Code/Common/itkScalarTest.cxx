/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkScalarTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include <iostream>
#include "itkScalar.h"

int main()
{
  bool passed = true;

  typedef itk::Scalar<short> ShortScalar;

  ShortScalar s;
  ShortScalar t;
  short i = 2;
  
  t = 7;
  s = 2.5;
  float fs;
  fs = s;
  fs = 3.2;
  s = (short) fs; // must explicitly cast to short, implicit casts are a defect
  s = i;

  s = t;
  s = 2;
  s = t.GetScalar();
  s.SetScalar(5);

  if (s == t)
    {
    passed = false;
    }
  if (! (s == s.GetScalar()) )
    {
    passed = false;
    }
  if (s == 7)
    {
    passed = false;
    }


  if (! (s != t) )
    {
    passed = false;
    }
  if (s != s.GetScalar())
    {
    passed = false;
    }
  if (s != 5)
    {
    passed = false;
    }

  if (s >= t)
    {
    passed = false;
    }
  if (! (s >= s.GetScalar()) )
    {
    passed = false;
    }
  if (s >= 7)
    {
    passed = false;
    }

  if (s > t)
    {
    passed = false;
    }
  if (s > s.GetScalar())
    {
    passed = false;
    }
  if (s > 7)
    {
    passed = false;
    }
  
  if (! (s <= t))
    {
    passed = false;
    }
  if (! (s <= s.GetScalar()))
    {
    passed = false;
    }
  if (! (s <= 7))
    {
    passed = false;
    }

  if (! (s < t))
    {
    passed = false;
    }
  if (s < s.GetScalar())
    {
    passed = false;
    }
  if (! (s < 7))
    {
    passed = false;
    }

  s = 2;
  s += 4;
  if (s != 6)
    {
    passed = false;
    }
  s = s + 4;
  if (s != 10)
    {
    passed = false;
    }
  
  
  if (passed)
    {
    std::cout << "Scalar test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Scalar test failed." << std::endl;
    return EXIT_FAILURE;
    }
}



