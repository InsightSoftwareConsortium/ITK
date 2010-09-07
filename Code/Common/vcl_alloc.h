/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    vcl_alloc.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef vcl_alloc_h
#define vcl_alloc_h

// That is an adaptor for working with any alloc provided below
template< class T, class Alloc >
class vcl_simple_alloc
{
  typedef Alloc alloc_type;
public:
  typedef typename Alloc::value_type alloc_value_type; // awf
  typedef T                          value_type;

#if !__STL_EAGER_TYPECHECK
  enum {
    chunk = sizeof( value_type ) / sizeof( alloc_value_type ) + ( sizeof( value_type ) % sizeof( alloc_value_type ) > 0 )
    };
#else
  // note: any out-of-line template definitions will not see this.
#define chunk ( sizeof( value_type ) / sizeof( alloc_value_type ) \
                + ( sizeof( value_type ) % sizeof( alloc_value_type ) > 0 ) )
#endif

  static value_type * allocate(size_t n)
  { return 0 == n ? 0 : (value_type *)alloc_type().allocate(n * chunk, 0); }
  static value_type * allocate(void)
  { return (value_type *)alloc_type().allocate(chunk, 0); }
  static void deallocate(value_type *p, size_t n)
  { if ( 0 != n ) { alloc_type().deallocate(p, n * chunk); } }
  static void deallocate(value_type *p)
  { alloc_type().deallocate( (char *)p, chunk ); }

#undef chunk
};
#endif
