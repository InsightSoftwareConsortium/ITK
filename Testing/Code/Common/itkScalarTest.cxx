/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkScalarTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

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
    exit(EXIT_SUCCESS);
    }
  else
    {
    std::cout << "Scalar test failed." << std::endl;
    exit(EXIT_FAILURE);
    }
}



