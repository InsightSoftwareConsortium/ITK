/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestTemplatePartialSpecialization.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/*
   This file tests if the compiler supports partial specialization or not
*/

template <class T>
class victor
{
  T data[256];
public:
  victor() {}
  T &operator[](unsigned i) { return data[i]; }
};

template <class T>
class victor<T *>
{
  T *data[256];
 public:
  T * &operator[](unsigned i) { return data[i]; }
  void slarf() { data[0] += (data[2] - data[1]); }
};

template <class A, class R>
struct foo {
  typedef A a;
  typedef R r;
};

template <class T> struct foo<T *, T *> { void bar() {} };
template <class T> struct foo<int *, T> { void baz() {} };

int main() 
{ 
  return 0;
}

