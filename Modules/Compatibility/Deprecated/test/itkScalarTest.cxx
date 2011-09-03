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
#include "itkScalar.h"

int itkScalarTest(int, char* [] )
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
