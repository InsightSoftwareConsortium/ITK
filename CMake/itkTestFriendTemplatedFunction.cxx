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
// Here is our templated function, forward declared
//
template <class T> class WantToHaveFriend;

template <class T>
bool operator==( const WantToHaveFriend<T> & a, const  WantToHaveFriend<T> & b );


//
// Here is our templated class
//
template <class T>
class WantToHaveFriend
{
public:

  typedef WantToHaveFriend Self;

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
  friend bool operator==(const Self &, const Self &);
#endif

#ifdef TRY_COMPILE_FRIEND_WITH_EMPTY_TEMPLATE_BRACKETS
  friend bool operator==<>(const Self &, const Self &);
#endif

#ifdef TRY_COMPILE_FRIEND_WITH_TEMPLATE_ARGUMENTS
  friend bool operator==<Self>(const Self &, const Self &);
#endif

private:
   int x;
};

template <class T>
bool operator==( const WantToHaveFriend<T> & a, const  WantToHaveFriend<T> & b )
{ return a.x == b.x; }


int main() 
{ 
  typedef WantToHaveFriend<int>  FriendlyType;

  FriendlyType foo1;
  FriendlyType foo2;

  bool result = ( foo1 == foo2 );

  if( result )
    {
    return 1;
    }

  return 0;
}

