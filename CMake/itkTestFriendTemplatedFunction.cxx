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

