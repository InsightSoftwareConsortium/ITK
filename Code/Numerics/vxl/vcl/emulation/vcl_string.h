// Main header for the -*- C++ -*- vcl_string classes.
#ifndef vcl_emulation_string_h_
#define vcl_emulation_string_h_

// Main templates for the -*- C++ -*- vcl_string classes.
// Copyright (C) 1994, 1995 Free Software Foundation

// This file is part of the GNU ANSI C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

// As a special exception, if you link this library with files
// compiled with a GNU compiler to produce an executable, this does not cause
// the resulting executable to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

// Written by Jason Merrill based upon the specification by Takanori Adachi
// in ANSI X3J16/94-0013R2.

#ifdef __GNUG__
#pragma interface
#endif

#include <vcl_cstddef.h>   // For ptrdiff_t
#include <vcl_iosfwd.h>
#include <vcl_rel_ops.h>   // For operator!= from operator==

#include "vcl_stlconf.h"
#include "vcl_straits.h"

#if _G_USE_EXCEPTIONS

#include <stdexcept>
#define OUTOFRANGE(cond) \
  do { if (!(cond)) throw out_of_range (#cond); } while (0)
#define LENGTHERROR(cond) \
  do { if (!(cond)) throw length_error (#cond); } while (0)

#else

#include <vcl_cassert.h>
#define OUTOFRANGE(cond) assert (!(cond))
#define LENGTHERROR(cond) assert (!(cond))

#endif

// Make shorter name for vcl_basic_string if STL is using short names
# if defined ( __STL_USE_ABBREVS )
#  define vcl_basic_string vcl_bS
# endif

#define EOS 0

extern "C++" {

template <class charT, VCL_DFL_TYPE_PARAM_STLDECL( traits, vcl_char_traits<charT> ) >
class vcl_basic_string
{
private:
  charT *data_;
  size_t strlen_;
  size_t current_size_;

public:
// types:
  typedef traits traits_type;
  typedef charT value_type;
  typedef size_t size_type;
  typedef ptrdiff_t difference_type;
  typedef charT& reference;
  typedef const charT& const_reference;
  typedef charT* pointer;
  typedef const charT* const_pointer;
  typedef pointer iterator;
  typedef const_pointer const_iterator;
#if 0
  typedef reverse_iterator<iterator, value_type,
                           reference, difference_type> reverse_iterator;
  typedef reverse_iterator<const_iterator, value_type, const_reference,
                           difference_type> const_reverse_iterator;
#endif
  static const size_type npos
#ifdef __GNUG__
  = static_cast<size_type>(-1)
#endif
       ;
//: Return pointer to internal data
  const charT* data () const
  { return data_; }

//: Return length in chars
  size_type length () const
  { return strlen_; }

//: Return length in chars
  size_type size () const
  { return strlen_; }

//: Return current amount of memory allocated
  size_type capacity () const
  { return current_size_; }
  
//: Return maximum possible size
  size_type max_size () const
    { return (npos - 1)/sizeof (charT); }  // XXX

//: Return true iff length is 0
  bool empty () const
    { return strlen_ == 0; }

// _lib.string.cons_ construct/copy/destroy:
// - Assignment
  vcl_basic_string& operator= (const vcl_basic_string& str);

// @{ CONSTRUCTORS @}

//: Default ctor
  explicit vcl_basic_string (): data_(0),strlen_(0),current_size_(0) { }

//: Copy ctor
  vcl_basic_string (const vcl_basic_string& str):
    data_(0),strlen_(0),current_size_(0)
  {
    *this = str;
  }

//: Assign str[pos .. pos+n]
  vcl_basic_string (const vcl_basic_string& str, size_type pos, size_type n = npos)
    : data_(0),strlen_(0),current_size_(0) { assign (str, pos, n); }

//: Assign s[0..n]
  vcl_basic_string (const charT* s, size_type n)
    : data_(0),strlen_(0),current_size_(0) { assign (s, n); }

//: Assign s
  vcl_basic_string (const charT* s)
    : data_(0),strlen_(0),current_size_(0) { assign (s); }

//: Fill with n copies of 'c'
  vcl_basic_string (size_type n, charT c)
    : data_(0),strlen_(0),current_size_(0) { assign (n, c); }

#if 0
  template<class InputIterator>
    vcl_basic_string(InputIterator begin, InputIterator end,
                 Allocator& = Allocator());
#else
  vcl_basic_string (const charT* s, const charT* e)
    : data_(0),strlen_(0),current_size_(0) { assign(s, e-s); }
#endif

  ~vcl_basic_string ()
    {
      delete [] data_;
    }

// @{ APPENDING, ASSIGNMENT @}

//: append str[pos..pos+n] etc.
  vcl_basic_string& append (const vcl_basic_string& str, size_type pos = 0,
                        size_type n = npos)
    { return replace (length (), 0, str, pos, n); }
  vcl_basic_string& append (const charT* s, size_type n)
    { return replace (length (), 0, s, n); }
  vcl_basic_string& append (const charT* s)
    { return append (s, traits::length (s)); }

  vcl_basic_string& append (size_type n, charT c)
    { return replace (length (), 0, n, c); }
#if 0
  template<class InputIterator>
    vcl_basic_string& append(InputIterator first, InputIterator last);
#endif

//: assign str[pos..pos+n] etc.
  vcl_basic_string& assign (const vcl_basic_string& str, size_type pos = 0,
                        size_type n = npos)
    { return replace (0, npos, str, pos, n); }
  vcl_basic_string& assign (const charT* s, size_type n)
    { return replace (0, npos, s, n); }
  vcl_basic_string& assign (const charT* s)
    { return assign (s, traits::length (s)); }
  vcl_basic_string& assign (size_type n, charT c)
    { return replace (0, npos, n, c); }
#if 0
  template<class InputIterator>
    vcl_basic_string& assign(InputIterator first, InputIterator last);
#endif

  vcl_basic_string& operator= (const charT* s)
    { return assign (s); }
  vcl_basic_string& operator= (charT c)
    { return assign (1, c); }

//: append rhs
  vcl_basic_string& operator+= (const vcl_basic_string& rhs)
    { return append (rhs); }
  vcl_basic_string& operator+= (const charT* s)
    { return append (s); }
  vcl_basic_string& operator+= (charT c)
    { return append (1, c); }

// @{ INSERTION @}

//: insert str[pos2..pos2+n] after position pos1.
  vcl_basic_string& insert (size_type pos1, const vcl_basic_string& str,
                        size_type pos2 = 0, size_type n = npos)
    { return replace (pos1, 0, str, pos2, n); }

//: insert s[0..n] after position pos.
  vcl_basic_string& insert (size_type pos, const charT* s, size_type n)
    { return replace (pos, 0, s, n); }

//: insert s after position pos.
  vcl_basic_string& insert (size_type pos, const charT* s)
    { return insert (pos, s, traits::length (s)); }

//: insert n copies of c after position pos.
  vcl_basic_string& insert (size_type pos, size_type n, charT c)
    { return replace (pos, 0, n, c); }

//: insert c after position indicated by iterator p.
  iterator insert(iterator p, charT c)
    { size_type pos = p - begin (); insert (pos, 1, c); return pos +begin (); }

//: insert n copies of c after position indicated by  iterator p.
  iterator insert(iterator p, size_type n, charT c)
    { size_type pos = p - begin (); insert (pos, n, c); return pos +begin (); }
#if 0
  template<class InputIterator>
    void insert(iterator p, InputIterator first, InputIterator last);
#endif

// @{ ERASING @}

//: erase [pos..pos+n]
  vcl_basic_string& erase (size_type pos = 0, size_type n = npos)
    { return replace (pos, n, (size_type)0, (charT)0); }

//: erase 1 character at pos.
  iterator erase (iterator p)
    { size_type __o = p - begin();
      replace (__o, 1, (size_type)0, (charT)0);
      return begin() + __o; }

//: erase from first to last.
  iterator erase (iterator f, iterator l)
    { size_type __o = f - begin();
      replace (__o, l-f, (size_type)0, (charT)0);
      return begin() + __o; }


// @{ REPLACING @}

//: replace this[pos1..pos1+n1] with str[pos2..pos2+n2].
  vcl_basic_string& replace (size_type pos1, size_type n1, const vcl_basic_string& str,
                         size_type pos2 = 0, size_type n2 = npos);

//: replace this[pos1..pos1+n1] with s[0..n2].
  vcl_basic_string& replace (size_type pos1, size_type n1, const charT* s,
                         size_type n2);

//: replace this[pos1..pos1+n1] with s.
  vcl_basic_string& replace (size_type pos1, size_type n1, const charT* s)
    { return replace (pos1, n1, s, traits::length (s)); }

//: replace this[pos1..pos1+n1] with n2 copies of c.
  vcl_basic_string& replace (size_type pos, size_type n1, size_type n2, charT c);

//: replace this[pos..pos+n] with c.
  vcl_basic_string& replace (size_type pos, size_type n, charT c)
    { return replace (pos, n, 1, c); }

//: replace this[i1..i2] with str. etc.
  vcl_basic_string& replace (iterator i1, iterator i2, const vcl_basic_string& str)
    { return replace (i1 - begin (), i2 - i1, str); }
  vcl_basic_string& replace (iterator i1, iterator i2, const charT* s, size_type n)
    { return replace (i1 - begin (), i2 - i1, s, n); }
  vcl_basic_string& replace (iterator i1, iterator i2, const charT* s)
    { return replace (i1 - begin (), i2 - i1, s); }
  vcl_basic_string& replace (iterator i1, iterator i2, size_type n, charT c)
    { return replace (i1 - begin (), i2 - i1, n, c); }
#if 0
  template<class InputIterator>
  vcl_basic_string& replace(iterator i1, iterator i2, InputIterator j1, InputIterator j2);
#else
  // no member templates:
  vcl_basic_string& replace(charT *i1, charT *i2, charT const *j1, charT const *j2)
    { return replace(i1 - begin(), i2-i1, j1, j2-j1); }
#endif

// @{ MISCELLANEOUS @}

//: swap with that.
  void swap (vcl_basic_string &that) {
#define vcl_string_swap_aux(T,m) {T d = that.m; that.m = this->m; this->m = d;}
    vcl_string_swap_aux(charT*, data_);
    vcl_string_swap_aux(size_t, strlen_);
    vcl_string_swap_aux(size_t, current_size_);
  }

//: Return [reference to] char at pos
  charT operator[] (size_type pos) const
    {
      return data_[pos];
    }

  reference operator[] (size_type pos)
    { return data_[pos]; }

//: Return [reference to] char at pos
  reference at (size_type pos)
    {
      OUTOFRANGE (pos >= length ());
      return data_[pos];
    }
  const_reference at (size_type pos) const
    {
      OUTOFRANGE (pos >= length ());
      return data ()[pos];
    }

//: Return pointer to C-style string
  const charT* c_str () const
    { return data_; }

//: Resize to n, filling with c.
  void resize (size_type n, charT c);

//: Resize to n, filling with 0.
  void resize (size_type n)
    { resize (n, EOS); }

//: Pre-expand size.  [unimplemented]
  void reserve (size_type) { }

//: Take a copy of s[0..n], placing after pos.
  size_type copy (charT* s, size_type n, size_type pos = 0);

// @{ SEARCHING @}

//: Return index of first occurrence of str, starting at pos.
  size_type find (const vcl_basic_string& str, size_type pos = 0) const
    { return find (str.data(), pos, str.length()); }
//: Find first occurrence of s[0..n], starting at pos.
  size_type find (const charT* s, size_type pos, size_type n) const;
//: Find first occurrence of s, starting at pos.
  size_type find (const charT* s, size_type pos = 0) const
    { return find (s, pos, traits::length (s)); }
  size_type find (charT c, size_type pos = 0) const;

//: Find, starting from end. etc.
  size_type rfind (const vcl_basic_string& str, size_type pos = npos) const
    { return rfind (str.data(), pos, str.length()); }
  size_type rfind (const charT* s, size_type pos, size_type n) const;
  size_type rfind (const charT* s, size_type pos = npos) const
    { return rfind (s, pos, traits::length (s)); }
  size_type rfind (charT c, size_type pos = npos) const;

//: Return index of the first occurrence of any character from str.
  size_type find_first_of (const vcl_basic_string& str, size_type pos = 0) const
    { return find_first_of (str.data(), pos, str.length()); }
  size_type find_first_of (const charT* s, size_type pos, size_type n) const;
  size_type find_first_of (const charT* s, size_type pos = 0) const
    { return find_first_of (s, pos, traits::length (s)); }
  size_type find_first_of (charT c, size_type pos = 0) const
    { return find (c, pos); }

//: Return index of the last occurrence of any character from str.
  size_type find_last_of (const vcl_basic_string& str, size_type pos = npos) const
    { return find_last_of (str.data(), pos, str.length()); }
  size_type find_last_of (const charT* s, size_type pos, size_type n) const;
  size_type find_last_of (const charT* s, size_type pos = npos) const
    { return find_last_of (s, pos, traits::length (s)); }
  size_type find_last_of (charT c, size_type pos = npos) const
    { return rfind (c, pos); }

//: Return index of the first occurrence of any character not in str.
  size_type find_first_not_of (const vcl_basic_string& str, size_type pos = 0) const
    { return find_first_not_of (str.data(), pos, str.length()); }
  size_type find_first_not_of (const charT* s, size_type pos, size_type n) const;
  size_type find_first_not_of (const charT* s, size_type pos = 0) const
    { return find_first_not_of (s, pos, traits::length (s)); }
  size_type find_first_not_of (charT c, size_type pos = 0) const;

//: Return index of the last occurrence of any character not in str.
  size_type find_last_not_of (const vcl_basic_string& str, size_type pos = npos) const
    { return find_last_not_of (str.data(), pos, str.length()); }
  size_type find_last_not_of (const charT* s, size_type pos, size_type n) const;
  size_type find_last_not_of (const charT* s, size_type pos = npos) const
    { return find_last_not_of (s, pos, traits::length (s)); }
  size_type find_last_not_of (charT c, size_type pos = npos) const;

//: Extract substring of length n chars staring at pos (written "this[pos..pos+n]")
  vcl_basic_string substr (size_type pos = 0, size_type n = npos) const
    { return vcl_basic_string (*this, pos, n); }

// @{ COMPARISON @}

//: Return -,0,+ comparing str to this[pos,pos+n].
  int compare (const vcl_basic_string& str, size_type pos = 0, size_type n = npos) const;
  // There is no 'strncmp' equivalent for charT pointers.
  int compare (const charT* s, size_type pos, size_type n) const;
  int compare (const charT* s, size_type pos = 0) const
    { return compare (s, pos, traits::length (s)); }

// @{ STL ITERATORS @}

  iterator begin () { return data_; }

  iterator end () { return data_ + strlen_; }

  const_iterator begin () const { return data_; }

  const_iterator end () const { return data_ + strlen_; }

#if 0
  reverse_iterator       rbegin() { return reverse_iterator (end ()); }
  const_reverse_iterator rbegin() const
    { return const_reverse_iterator (end ()); }
  reverse_iterator       rend() { return reverse_iterator (begin ()); }
  const_reverse_iterator rend() const
    { return const reverse_iterator (begin ()); }
#endif

private:
  static size_type _find (const charT* ptr, charT c, size_type xpos, size_type len);

  void realloc(int new_capacity);
};

// Provide template defn for const.
// Currently gcc doesn't want it and doesn't need it.
// Win32 needs it.
// ecgs doesn't care

#ifndef __GNUG__
#ifndef __SUNPRO_CC
template <class charT, class traits>
const size_t vcl_basic_string <charT, traits>::npos = (size_t)(-1);
#endif
#endif

template <class charT, class traits>
inline vcl_basic_string <charT, traits>
operator+ (const vcl_basic_string <charT, traits>& lhs,
           const vcl_basic_string <charT, traits>& rhs)
{
  vcl_basic_string <charT, traits> str (lhs);
  str.append (rhs);
  return str;
}

template <class charT, class traits>
inline vcl_basic_string <charT, traits>
operator+ (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  vcl_basic_string <charT, traits> str (lhs);
  str.append (rhs);
  return str;
}

template <class charT, class traits>
inline vcl_basic_string <charT, traits>
operator+ (charT lhs, const vcl_basic_string <charT, traits>& rhs)
{
  vcl_basic_string <charT, traits> str (1, lhs);
  str.append (rhs);
  return str;
}

template <class charT, class traits>
inline vcl_basic_string <charT, traits>
operator+ (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  vcl_basic_string <charT, traits> str (lhs);
  str.append (rhs);
  return str;
}

template <class charT, class traits>
inline vcl_basic_string <charT, traits>
operator+ (const vcl_basic_string <charT, traits>& lhs, charT rhs)
{
  vcl_basic_string <charT, traits> str (lhs);
  str.append (1, rhs);
  return str;
}

template <class charT, class traits>
inline bool
operator== (const vcl_basic_string <charT, traits>& lhs,
            const vcl_basic_string <charT, traits>& rhs)
{
  return (lhs.compare (rhs) == 0);
}

template <class charT, class traits>
inline bool
operator== (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  return (rhs.compare (lhs) == 0);
}

template <class charT, class traits>
inline bool
operator== (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  return (lhs.compare (rhs) == 0);
}

template <class charT, class traits>
inline bool
operator!= (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  return (rhs.compare (lhs) != 0);
}

template <class charT, class traits>
inline bool
operator!= (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  return (lhs.compare (rhs) != 0);
}

template <class charT, class traits>
inline bool
operator< (const vcl_basic_string <charT, traits>& lhs,
            const vcl_basic_string <charT, traits>& rhs)
{
  return (lhs.compare (rhs) < 0);
}

template <class charT, class traits>
inline bool
operator< (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  return (rhs.compare (lhs) > 0);
}

template <class charT, class traits>
inline bool
operator< (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  return (lhs.compare (rhs) < 0);
}

template <class charT, class traits>
inline bool
operator> (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  return (rhs.compare (lhs) < 0);
}

template <class charT, class traits>
inline bool
operator> (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  return (lhs.compare (rhs) > 0);
}

template <class charT, class traits>
inline bool
operator<= (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  return (rhs.compare (lhs) >= 0);
}

template <class charT, class traits>
inline bool
operator<= (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  return (lhs.compare (rhs) <= 0);
}

template <class charT, class traits>
inline bool
operator>= (const charT* lhs, const vcl_basic_string <charT, traits>& rhs)
{
  return (rhs.compare (lhs) <= 0);
}

template <class charT, class traits>
inline bool
operator>= (const vcl_basic_string <charT, traits>& lhs, const charT* rhs)
{
  return (lhs.compare (rhs) >= 0);
}

// Kludge this until g++ supports the new template overloading semantics.
// NOTE: vcl_string.h should be included after standard STL headers, if those
// headers are included, so __SGI_STL_FUNCTION_H is defined before this text
// is included. -- tpr
// #ifndef __IGNORE_STRING_COMPARISOM_OPERATORS
// #if !defined(__SGI_STL_FUNCTION_H)
#if !defined(FUNCTION_H)
template <class charT, class traits>
inline bool
operator!= (const vcl_basic_string <charT, traits>& lhs,
            const vcl_basic_string <charT, traits>& rhs)
{
  return (lhs.compare (rhs) != 0);
}

template <class charT, class traits>
inline bool
operator> (const vcl_basic_string <charT, traits>& lhs,
           const vcl_basic_string <charT, traits>& rhs)
{
  return (lhs.compare (rhs) > 0);
}

template <class charT, class traits>
inline bool
operator<= (const vcl_basic_string <charT, traits>& lhs,
            const vcl_basic_string <charT, traits>& rhs)
{
  return (lhs.compare (rhs) <= 0);
}

template <class charT, class traits>
inline bool
operator>= (const vcl_basic_string <charT, traits>& lhs,
            const vcl_basic_string <charT, traits>& rhs)
{
  return (lhs.compare (rhs) >= 0);
}
#endif
// #endif

template <class charT, class traits> vcl_istream&
operator>> (vcl_istream&, vcl_basic_string <charT, traits>&);
template <class charT, class traits>
inline vcl_ostream&
operator<< (vcl_ostream &o, const vcl_basic_string <charT, traits>& s)
{
  return o.write (s.data (), s.length ());
}

template <class charT, class traits> vcl_istream&
#ifdef __SUNPRO_CC
getline (vcl_istream&, vcl_basic_string <charT, traits>&, charT delim);
#else
getline (vcl_istream&, vcl_basic_string <charT, traits>&, charT delim ='\n');
#endif
} // extern "C++"

#undef EOS

#define __SUBCLASS_STRING_ABBREV
#if defined(__GNUC__) || defined(__SGI_CC)
//rick# if (__GNUC__ > 2) || (__GNUC_MINOR__ <= 7)
#  undef __SUBCLASS_STRING_ABBREV
//rick# endif
#endif

#if defined(__STL_USE_ABBREVS) && defined(__SUBCLASS_STRING_ABBREV)

#define VCL_STRING_IS_TYPEDEF 0
// For short typenames:
struct vcl_string : public vcl_basic_string <char, vcl_char_traits <char> > {
  typedef vcl_basic_string <char, vcl_char_traits <char> > super;

  explicit vcl_string () { }
  vcl_string (const vcl_string& str): super(str) {}
  vcl_string (const super& str): super(str) {}
  vcl_string (const super& str, size_type pos, size_type n = npos): super(str,pos,n) {}
  vcl_string (const char* s, size_type n): super(s,n) {}
  vcl_string (const char* s): super(s) {}
  vcl_string (size_type n, char c): super(n,c) {}
#if 0
  template<class InputIterator>
  vcl_string(InputIterator begin, InputIterator end): super(begin,end) {}
#endif
};
#else

#define VCL_STRING_IS_TYPEDEF 1
typedef vcl_basic_string <char, vcl_char_traits <char> > vcl_string;
#endif

#endif // vcl_emulation_string_h
