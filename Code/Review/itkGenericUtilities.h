/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGenericUtilities.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGenericUtilities_h
#define __itkGenericUtilities_h

namespace itk
{
///////////////////////////////////////////////////

///////////////////////////////////////////////////
// Generic Programming Algorithms
///////////////////////////////////////////////////

/// \brief returns the min and max of a sequence defined by two iterators
///
/// Uses the < operator to determin ordering
/// If first == last then return is pair(first,first);
/// otherwise is it pair(min, max)
template< class TInputIter >
std::pair< TInputIter, TInputIter > min_max_element(TInputIter first, TInputIter last)
{
  std::pair< TInputIter, TInputIter > result(first, first);

  if ( first == last )
    {
    return result;
    }

  while ( ++first != last )
    {
    TInputIter prev = first;
    if ( ++first == last )
      {
      if ( *prev < *( result.first ) )
        {
        result.first = prev;
        }
      if ( *( result.second ) < *prev )
        {
        result.second = prev;
        }
      break;
      }
    else if ( *first < *prev )
      {
      if ( *first < *( result.first ) )
        {
        result.first = first;
        }
      if ( *( result.second ) < *prev )
        {
        result.second = prev;
        }
      }
    else
      {
      if ( *prev < *( result.first ) )
        {
        result.first = prev;
        }
      if ( *( result.second ) < *first )
        {
        result.second = first;
        }
      }
    }
  return result;
}

/// \brief returns the min and max of a
///
/// Uses the provided binary functor
/// If first == last then return is pair(first,first);
/// otherwise is it pair(min, max)
template< class TInputIter, class TCompare >
std::pair< TInputIter, TInputIter > min_max_element(TInputIter first, TInputIter last, TCompare comp)
{
  std::pair< TInputIter, TInputIter > result(first, first);

  if ( first == last )
    {
    return result;
    }

  while ( ++first != last )
    {
    TInputIter prev = first;
    if ( ++first == last )
      {
      if ( comp( *prev, *( result.first ) ) )
        {
        result.first = prev;
        }
      if ( comp(*( result.second ), *prev) )
        {
        result.second = prev;
        }
      break;
      }
    else if ( comp(*first, *prev) )
      {
      if ( comp( *first, *( result.first ) ) )
        {
        result.first = first;
        }
      if ( comp(*( result.second ), *prev) )
        {
        result.second = prev;
        }
      }
    else
      {
      if ( comp( *prev, *( result.first ) ) )
        {
        result.first = prev;
        }
      if ( comp(*( result.second ), *first) )
        {
        result.second = first;
        }
      }
    }
  return result;
}
} // end itk namespace

#endif //__itkGenericUtilities_h
