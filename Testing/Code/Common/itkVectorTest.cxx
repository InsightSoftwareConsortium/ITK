/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkVectorTest.cxx
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
#include "itkVector.h"

// Define floating point type to use for the test.
typedef double Real;

bool different(Real a, Real b)
{
  return fabs(a-b) > 1e-6;
}

int main()
{
  bool passed = true;

  typedef itk::Vector<Real, 2> RealVector;

  RealVector s;
  RealVector t;
  RealVector r;

  int i = 4;
  Real f = 2.1;
  
  s.Fill(3.0);
  if (different(s[0], 3.0) || different(s[1], 3.0))
    {
    passed = false;
    }

  s.Fill(2);
  if (different(s[0], 2.0) || different(s[1], 2.0))
    {
    passed = false;
    }
  
  s.Fill(i);
  if (different(s[0], i) || different(s[1], i))
    {
    passed = false;
    }

  s.Fill(f);
  if (different(s[0], f) || different(s[1], f))
    {
    passed = false;
    }


  t = s;
  if (different(t[0], s[0]) || different(t[1], s[1]))
    {
    passed = false;
    }
  
  s = -t;
  if (different(s[0], -t[0]) || different(s[1], -t[1]))
    {
    passed = false; 
    }
  
  s.Fill(3.0);
  s *= 2.5;
  if (different(s[0], 7.5) || different(s[1], 7.5))
    {
    passed = false;
    }

  s /= 2.0;
  if (different(s[0], 3.75) || different(s[1], 3.75))
    {
    passed = false;
    }
 

  s.Fill(3.8);
  s *= f;
  if (different(s[0], 7.98) || different(s[1], 7.98))
    {
    passed = false;
    }  

  s /= f;
  if (different(s[0], 3.8) || different(s[1], 3.8))
    {
    passed = false;
    }

  s += t;
  if (different(s[0], 5.9) || different(s[1], 5.9))
    {
    passed = false;
    }
  

  s -= t;
  if (different(s[0], 3.8) || different(s[1], 3.8))
    {
    passed = false;
    }


  r = s + t;
  if (different(r[0], 5.9) || different(r[1], 5.9))
    {
    passed = false;
    }
  
  r = s - t;
  if (different(r[0], 1.7) || different(r[1], 1.7))
    {
    passed = false;
    }


  r = s * 10.0;
  if (different(r[0], 38.0) || different(r[1], 38.0))
    {
    passed = false;
    }

  r = s / 10.0;
  if (different(r[0], .38) || different(r[1], .38))
    {
    passed = false;
    }

  r = s * f;
  if (different(r[0], 7.98) || different(r[1], 7.98))
    {
    passed = false;
    }

  f = 2.0;
  r = s / f;
  if (different(r[0], 1.9) || different(r[1], 1.9))
    {
    passed = false;
    }

  r[1] = 7.0;
  if (different(r[0], 1.9) || different(r[1], 7.0))
    {
    passed = false;
    }

  if (passed)
    {
    std::cout << "Vector test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else
    {
    std::cout << "Vector test failed." << std::endl;
    return EXIT_FAILURE;
    }
  
}
