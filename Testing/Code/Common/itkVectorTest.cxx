/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkVectorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkVector.h"

bool different(float a, float b)
{
  return fabs(a-b) > 1e-6;
}

int main()
{
  bool passed = true;

  typedef itk::Vector<float, 2> FloatVector;

  FloatVector s;
  FloatVector t;
  FloatVector r;

  int i = 4;
  float f = 2.1;
  
  s = 3.0;
  if (different(s[0], 3.0) || different(s[1], 3.0))
    {
    passed = false;
    }

  s = 2;
  if (different(s[0], 2.0) || different(s[1], 2.0))
    {
    passed = false;
    }
  
  s = i;
  if (different(s[0], i) || different(s[1], i))
    {
    passed = false;
    }

  s = f;
  if (different(s[0], f) || different(s[1], f))
    {
    passed = false;
    }


  t = s;
  if (different(t[0], s[0]) || different(t[1], s[1]))
    {
    passed = false;
    }
  
  s = (-t);
  if (different(s[0], -t[0]) || different(s[1], -t[1]))
    {
    passed = false; 
    }
  
  s = 3.0;
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
  
  s += 0.5;
  if (different(s[0], 4.25) || different(s[1], 4.25))
    {
    passed = false;
    }

  s -= 0.2;
  if (different(s[0], 4.05) || different(s[1], 4.05))
    {
    passed = false;
    }

  s = 3.8;
  s *= f;
  if (different(s[0], 7.98) || different(s[1], 7.98))
    {
    passed = false;
    }
  
  s += f;
  if (different(s[0], 10.08) || different(s[1], 10.08))
    {
    passed = false;
    }
  
  s -= f;
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
  
  s += t.GetVector();
  if (different(s[0], 8.0) || different(s[1], 8.0))
    {
    passed = false;
    }

  s -= t;
  if (different(s[0], 5.9) || different(s[1], 5.9))
    {
    passed = false;
    }

  s -= t.GetVector();
  if (different(s[0], 3.8) || different(s[1], 3.8))
    {
    passed = false;
    }

  r = s + t;
  if (different(r[0], 5.9) || different(r[1], 5.9))
    {
    passed = false;
    }
  r = s + t.GetVector();
  if (different(r[0], 5.9) || different(r[1], 5.9))
    {
    passed = false;
    }
  
  r = s - t;
  if (different(r[0], 1.7) || different(r[1], 1.7))
    {
    passed = false;
    }
  r = s - t.GetVector();
  if (different(r[0], 1.7) || different(r[1], 1.7))
    {
    passed = false;
    }

  r = s + 0.1111;
  if (different(r[0], 3.9111) || different(r[1], 3.9111))
    {
    passed = false;
    }
  r = s - 0.1;
  if (different(r[0], 3.7) || different(r[1], 3.7))
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

  r = s + f;
  if (different(r[0], 5.9) || different(r[1], 5.9))
    {
    passed = false;
    }

  r = s - f;
  if (different(r[0], 1.7) || different(r[1], 1.7))
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
    exit(EXIT_SUCCESS);
    }
  else
    {
    std::cout << "Vector test failed." << std::endl;
    exit(EXIT_FAILURE);
    }
  
}



