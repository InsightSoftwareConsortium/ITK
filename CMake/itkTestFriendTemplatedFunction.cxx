/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTestFriendTemplatedFunction.cxx
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
   This file tests the syntax that the compiler supports for declaring 
   templated functions as friends of a templated class.
*/

//
// Here is our templated function
//
template <class T>
T compose( const T & a, const T & b )
{ return a + b; }


//
// Here is our templated class
//
template <class T>
class WantToHaveFriend
{
public:

  typedef WantToHaveFriend Self;

  Self operator+(const Self & other) const
    {
    Self result;
    result.x = this->x + other.x;
    return result;
    }

  WantToHaveFriend()
    {
    x = 0;
    }
 
  void DoNothing() const
    {
    // of course... do nothing.
    }

//
//  Here are the variants that some compilers use
//

#ifdef TRY_COMPILE_FRIEND_WITH_NULL_TEMPLATE_STRING
  friend Self compose(const Self &, const Self &);
#endif

#ifdef TRY_COMPILE_FRIEND_WITH_EMPTY_TEMPLATE_BRACKETS
  friend Self compose<>(const Self &, const Self &);
#endif

#ifdef TRY_COMPILE_FRIEND_WITH_TEMPLATE_ARGUMENTS
  friend Self compose<Self>(const Self &, const Self &);
#endif

private:
   int x;
};

int main() 
{ 
  typedef WantToHaveFriend<int>  FriendlyType;

  FriendlyType foo1;
  FriendlyType foo2;
  FriendlyType foo3;

  foo1 = compose( foo2, foo3 );

  foo1.DoNothing();

  return 0;
}

