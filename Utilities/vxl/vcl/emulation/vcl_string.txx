// Member templates for the -*- C++ -*- string classes.
// Copyright (C) 1994 Free Software Foundation

#ifndef vcl_emulation_string_txx_
#define vcl_emulation_string_txx_

//:
// \file
//
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
// Rewritten by awf@robots.ox.ac.uk to use non-refcounting implementation.

#include <vcl_cassert.h>
#include <vcl_iostream.h>

extern "C++" {

template <class charT, class traits>
vcl_basic_string <charT, traits>& vcl_basic_string <charT, traits>::
operator= (const vcl_basic_string& str)
{
  if (&str != this) {
    if (str.data_) {
      realloc(str.strlen_ + 1); // decisions...
      this->strlen_ = str.strlen_;
      traits::copy(this->data_, str.data_, this->strlen_ + 1);
    } else {
      this->data_ = 0;
      this->strlen_ = 0;
      this->current_size_ = 0;
    }
  }
  return *this;
}

//: Private, ensures string is large enough to hold `new_capacity' characters.
template <class charT, class traits>
void vcl_basic_string <charT, traits>::
realloc(int new_capacity)
{
  if ((unsigned) new_capacity < this->current_size_)
    return;
  if (!this->data_) {
    this->current_size_ = ((unsigned)new_capacity + 15U) & ~15U; // round up to 16
    this->data_ = new charT[this->current_size_];
  } else {
#ifdef __GNUG__
    char tmp[this->strlen_ + 1];
#else
    // FIXME should feature-check and use alloca here.  As every schoolboy
    // knows, it's a Bad Thing to use the heap for realloc copy buffers.
    char *tmp = new char[this->strlen_ + 1];
#endif
    traits::copy(tmp, this->data_, this->strlen_ + 1);

    // assert basically that strlen < current_size_
    assert(this->strlen_ < (vcl_size_t) new_capacity); // need space for null terminator.

    while (this->current_size_ <= (vcl_size_t) new_capacity)
      this->current_size_ = this->current_size_ * 2;
    this->current_size_ = ((unsigned)this->current_size_ + 15U) & ~15U; // round up to 16

    delete [] this->data_;
    this->data_ = new charT[this->current_size_];
    traits::copy(this->data_, tmp, this->strlen_ + 1);
#ifdef __GNUG__
    /* no cleanup */
#else
    delete [] tmp;
#endif
  }
}

//: Replace this[pos1..pos1+n1) with str[pos2..pos2+n2).
template <class charT, class traits>
vcl_basic_string <charT, traits>& vcl_basic_string <charT, traits>::
replace (vcl_size_t pos1, vcl_size_t n1,
         const vcl_basic_string& str, vcl_size_t pos2, vcl_size_t n2)
{
  const vcl_size_t len2 = str.length ();

  if (pos1 == 0 && n1 >= length () && pos2 == 0 && n2 >= len2)
    return operator= (str);

  OUTOFRANGE (pos2 > len2);

  if (n2 > len2 - pos2)
    n2 = len2 - pos2;

  return replace (pos1, n1, str.data () + pos2, n2);
}

//: Replace this[pos..pos+n1) with str[0..n2).
template <class charT, class traits>
vcl_basic_string <charT, traits>& vcl_basic_string <charT, traits>::
replace (vcl_size_t pos, vcl_size_t n1, const charT* s, vcl_size_t n2)
{
  const vcl_size_t len = length ();
  OUTOFRANGE (pos > len);
  if (n1 > len - pos)
    n1 = len - pos;
  LENGTHERROR (len - n1 > max_size () - n2);
  vcl_size_t newlen = len - n1 + n2;

  realloc (newlen + 1);
  int p = len - (pos + n1);
  if (p > 0)
    traits::move(this->data_ + pos + n2, this->data_ + pos + n1, p);

  traits::copy(this->data_ + pos, s, n2);
  this->strlen_ = newlen;
  // null terminate
  this->data_[this->strlen_] = 0;

  return *this;
}

template <class charT, class traits>
vcl_basic_string <charT, traits>& vcl_basic_string <charT, traits>::
replace (vcl_size_t pos, vcl_size_t n1, vcl_size_t n2, charT c)
{
  const vcl_size_t len = length ();
  OUTOFRANGE (pos > len);
  if (n1 > len - pos)
    n1 = len - pos;
  LENGTHERROR (len - n1 > max_size () - n2);
  vcl_size_t newlen = len - n1 + n2;

  realloc (newlen + 1);
  int p = len - (pos + n1);

  if (p > 0)
    traits::move(this->data_ + pos + n2, this->data_ + pos + n1, p);

  if (n2)
    traits::set(this->data_ + pos + n1, c, n2);
  this->strlen_ = newlen;
  // null terminate
  this->data_[this->strlen_] = 0;

  return *this;
}

template <class charT, class traits>
void vcl_basic_string <charT, traits>::
resize (vcl_size_t n, charT c)
{
  LENGTHERROR (n > max_size ());

  if (n > length ())
    append (n - length (), c);
  //  else
  // bigfixme    remove (n);
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
copy (charT* s, vcl_size_t n, vcl_size_t pos)
{
  OUTOFRANGE (pos > length ());

  if (n > length () - pos)
    n = length () - pos;

  traits::copy (s, data () + pos, n);
  return n;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  vcl_size_t xpos = pos;
  for (; xpos + n <= length (); ++xpos)
    if (traits::eq (data () [xpos], *s)
        && traits::compare (data () + xpos, s, n) == 0)
      return xpos;
  return npos;
}

template <class charT, class traits>
inline vcl_size_t vcl_basic_string <charT, traits>::
_find (const charT* ptr, charT c, vcl_size_t xpos, vcl_size_t len)
{
  for (; xpos < len; ++xpos)
    if (traits::eq (ptr [xpos], c))
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find (charT c, vcl_size_t pos) const
{
  return _find (data (), c, pos, length ());
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
rfind (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  if (n > length ())
    return npos;

  vcl_size_t xpos = length () - n;
  if (xpos > pos)
    xpos = pos;

  for (++xpos; xpos-- > 0; )
    if (traits::eq (data () [xpos], *s)
        && traits::compare (data () + xpos, s, n) == 0)
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
rfind (charT c, vcl_size_t pos) const
{
  if (1 > length ())
    return npos;

  vcl_size_t xpos = length () - 1;
  if (xpos > pos)
    xpos = pos;

  for (++xpos; xpos-- > 0; )
    if (traits::eq (data () [xpos], c))
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find_first_of (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  vcl_size_t xpos = pos;
  for (; xpos < length (); ++xpos)
    if (_find (s, data () [xpos], 0, n) != npos)
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find_last_of (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  vcl_size_t xpos = length ();
  for (; xpos-- > pos; )
    if (_find (s, data () [xpos], 0, n) != npos)
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find_first_not_of (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  vcl_size_t xpos = pos;
  for (; xpos < length (); ++xpos)
    if (_find (s, data () [xpos], 0, n) == npos)
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find_first_not_of (charT c, vcl_size_t pos) const
{
  vcl_size_t xpos = pos;
  for (; xpos < length (); ++xpos)
    if (traits::ne (data () [xpos], c))
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find_last_not_of (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  vcl_size_t xpos = length ();
  for (; xpos-- > pos; )
    if (_find (s, data () [xpos], 0, n) == npos)
      return xpos;
  return npos;
}

template <class charT, class traits>
vcl_size_t vcl_basic_string <charT, traits>::
find_last_not_of (charT c, vcl_size_t pos) const
{
  vcl_size_t xpos = length ();
  for (; xpos-- > pos; )
    if (traits::ne (data () [xpos], c))
      return xpos;
  return npos;
}

template <class charT, class traits>
int vcl_basic_string <charT, traits>::
compare (const vcl_basic_string& str, vcl_size_t pos, vcl_size_t n) const
{
  OUTOFRANGE (pos > length ());

  vcl_size_t rlen = length () - pos;
  if (rlen > n)
    rlen = n;
  if (rlen > str.length ())
    rlen = str.length ();
  int r = traits::compare (data () + pos, str.data (), rlen);
  if (r != 0)
    return r;
  if (rlen == n)
    return 0;
  return (length () - pos) - str.length ();
}

template <class charT, class traits>
int vcl_basic_string <charT, traits>::
compare (const charT* s, vcl_size_t pos, vcl_size_t n) const
{
  OUTOFRANGE (pos > length ());

  vcl_size_t rlen = length () - pos;
  if (rlen > n)
    rlen = n;
  int r = traits::compare (data () + pos, s, rlen);
  if (r != 0)
    return r;
  return (length () - pos) - n;
}

template <class charT, class traits>
vcl_istream &
operator>> (vcl_istream &is, vcl_basic_string <charT, traits> &s)
{
  int w = is.width (0);
#ifdef __GNUG__
  if (is.ipfx0 ())
#endif
    {
      register vcl_streambuf *sb = is.rdbuf ();
      s.resize (0);
      while (1)
        {
          int ch = sb->sbumpc ();
          if (ch == EOF)
            {
#ifdef __GNUC__
              is.setstate (vcl_ios::eofbit);
#endif
              break;
            }
          else if (traits::is_del (ch))
            {
#ifdef __GNUC__
              sb->sungetc ();
#endif
              break;
            }
          s += ch;
          if (--w == 1)
            break;
        }
    }
#if __SUNPRO_CC >= 0x500
  vcl_istream::sentry sen(is); // this is how you call isfx in ansi c++
#else
  is.isfx ();
#endif
#ifdef __GNUC__
  if (s.length () == 0)
    is.setstate (vcl_ios::failbit);
#endif

  return is;
}

template <class charT, class traits>
vcl_istream&
getline (vcl_istream &is, vcl_basic_string <charT, traits>& s, charT delim)
{
  if (is.ipfx1 ())
    {
      _IO_size_t count = 0;
      vcl_streambuf *sb = is.rdbuf ();
      s.resize (0);

      while (1)
        {
          int ch = sb->sbumpc ();
          if (ch == EOF)
            {
              is.setstate (count == 0
                           ? (vcl_ios::failbit|vcl_ios::eofbit)
                           : vcl_ios::eofbit);
              break;
            }

          ++count;

          if (ch == delim)
            break;

          s += ch;

          if (s.length () == s.npos - 1)
            {
              is.setstate (vcl_ios::failbit);
              break;
            }
        }
    }

  // We need to be friends with istream to do this.
  // is._gcount = count;
  is.isfx ();

  return is;
}
} // extern "C++"

// Instantiation macro.

#undef VCL_BASIC_STRING_INSTANTIATE
#define VCL_BASIC_STRING_INSTANTIATE(charT,traits) \
typedef vcl_basic_string<charT, traits > charType; \
template class vcl_basic_string<charT, traits >;\
VCL_INSTANTIATE_INLINE(vcl_ostream& operator<<(vcl_ostream &, charType const& s));\
template vcl_istream& operator >> (vcl_istream &i, charType & s);\
VCL_INSTANTIATE_INLINE(charType operator+(charType const &, charType const &));\
VCL_INSTANTIATE_INLINE(charType operator+(charType const &, charT const *));\
VCL_INSTANTIATE_INLINE(charType operator+(charType const &, charT));\
VCL_INSTANTIATE_INLINE(charType operator+(charT const *, charType const &));\
VCL_INSTANTIATE_INLINE(bool operator==(charType const &, charType const &));\
VCL_INSTANTIATE_INLINE(bool operator==(charType const &, charT const *));\
VCL_INSTANTIATE_INLINE(bool operator!=(charType const &, charT const *));\
VCL_INSTANTIATE_INLINE(bool operator!=(charType const &, charType const &));\
VCL_INSTANTIATE_INLINE(bool operator<(charType const &, charType const &))
// End macro

#endif // vcl_emulation_string_txx_
