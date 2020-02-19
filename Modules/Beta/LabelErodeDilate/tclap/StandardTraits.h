// -*- Mode: c++; c-basic-offset: 4; tab-width: 4; -*-

/******************************************************************************
 *
 *  file:  StandardTraits.h
 *
 *  Copyright (c) 2007, Daniel Aarno, Michael E. Smoot .
 *  All rights reverved.
 *
 *  See the file COPYING in the top directory of this distribution for
 *  more information.
 *
 *  THE SOFTWARE IS PROVIDED _AS IS_, WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 *  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *  DEALINGS IN THE SOFTWARE.
 *
 *****************************************************************************/

// This is an internal tclap file, you should probably not have to
// include this directly

#ifndef TCLAP_STANDARD_TRAITS_H
#define TCLAP_STANDARD_TRAITS_H

#ifdef HAVE_CONFIG_H
#  include <config.h> // To check for long long
#endif

namespace TCLAP
{

// ======================================================================
// Integer types
// ======================================================================

/**
 * longs have value-like semantics.
 */
template <>
struct ArgTraits<long>
{
  using ValueCategory = ValueLike;
};

/**
 * ints have value-like semantics.
 */
template <>
struct ArgTraits<int>
{
  using ValueCategory = ValueLike;
};

/**
 * shorts have value-like semantics.
 */
template <>
struct ArgTraits<short>
{
  using ValueCategory = ValueLike;
};

/**
 * chars have value-like semantics.
 */
template <>
struct ArgTraits<char>
{
  using ValueCategory = ValueLike;
};

#ifdef HAVE_LONG_LONG
/**
 * long longs have value-like semantics.
 */
template <>
struct ArgTraits<long long>
{
  using ValueCategory = ValueLike;
};
#endif

// ======================================================================
// Unsigned integer types
// ======================================================================

/**
 * unsigned longs have value-like semantics.
 */
template <>
struct ArgTraits<unsigned long>
{
  using ValueCategory = ValueLike;
};

/**
 * unsigned ints have value-like semantics.
 */
template <>
struct ArgTraits<unsigned int>
{
  using ValueCategory = ValueLike;
};

/**
 * unsigned shorts have value-like semantics.
 */
template <>
struct ArgTraits<unsigned short>
{
  using ValueCategory = ValueLike;
};

/**
 * unsigned chars have value-like semantics.
 */
template <>
struct ArgTraits<unsigned char>
{
  using ValueCategory = ValueLike;
};

#ifdef HAVE_LONG_LONG
/**
 * unsigned long longs have value-like semantics.
 */
template <>
struct ArgTraits<unsigned long long>
{
  using ValueCategory = ValueLike;
};
#endif

// ======================================================================
// Float types
// ======================================================================

/**
 * floats have value-like semantics.
 */
template <>
struct ArgTraits<float>
{
  using ValueCategory = ValueLike;
};

/**
 * doubles have value-like semantics.
 */
template <>
struct ArgTraits<double>
{
  using ValueCategory = ValueLike;
};

// ======================================================================
// Other types
// ======================================================================

/**
 * bools have value-like semantics.
 */
template <>
struct ArgTraits<bool>
{
  using ValueCategory = ValueLike;
};

/**
 * wchar_ts have value-like semantics.
 */
template <>
struct ArgTraits<wchar_t>
{
  using ValueCategory = ValueLike;
};

/**
 * Strings have string like argument traits.
 */
template <>
struct ArgTraits<std::string>
{
  using ValueCategory = StringLike;
};

template <typename T>
void
SetString(T & dst, const std::string & src)
{
  dst = src;
}

} // namespace TCLAP

#endif
