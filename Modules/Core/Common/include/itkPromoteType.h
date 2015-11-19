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
#ifndef itkPromoteType_h
#define itkPromoteType_h

#include "itkMacro.h"

// Simplification of boost::common_type
namespace itk {

/// \cond HIDE_META_PROGRAMMING
namespace mpl {

namespace Details {

/** Helper class to implement \c itk::PromoteType<>.
 * This struct is meant to be specialized with \c ITK_ASSOCIATE macros for the
 * primitive types, or by the end-developper in order to support overloads.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <int N, typename TA, typename TB> struct SizeToType;

/** Helper class to implement \c itk::PromoteType<>.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <int N> struct Identity { typedef char Type[N]; };

/** Helper macro to implement \c itk::PromoteType<>.
 * This macros helps specializing \c SizeToType<> for any pair of type.
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
#define ITK_ASSOCIATE(N, Typed)\
  template <typename TA, typename TB> struct SizeToType<N,TA,TB> { typedef Typed Type; };

  ITK_ASSOCIATE(1, TA);
  ITK_ASSOCIATE(2, TB);

  ITK_ASSOCIATE(3,  short);
  ITK_ASSOCIATE(4,  unsigned short);
  ITK_ASSOCIATE(5,  int);
  ITK_ASSOCIATE(6,  unsigned int);
  ITK_ASSOCIATE(7,  long);
  ITK_ASSOCIATE(8,  unsigned long);
  ITK_ASSOCIATE(9,  long long);
  ITK_ASSOCIATE(10, unsigned long long);
  ITK_ASSOCIATE(11, float);
  ITK_ASSOCIATE(12, double);
  ITK_ASSOCIATE(13, long double);
#undef ITK_ASSOCIATE
} // Details namespace

/** Helper class to deduce, in C++98, the resulting type of an operation between two input types.
 * \tparam TA Input type 1
 * \tparam TB Input type 2
 * \return \c Type the resulting type compatible with \c TA and \c TB.
 *
 * In order to support user defined type, specialize \c
 * itk::PromoteType<> in consequence. For instance, to support type promotion
 * between \c std::complex<>, write:
 \code
 namespace itk {
   template <typename TA, typename TB>
   struct PromoteType<std::complex<TA>, std::complex<TB> > {
     typedef std::complex<typename PromoteType<TA,TB>::Type> Type;
   };
 }
 \endcode
 *
 * \ingroup MetaProgrammingLibrary
 * \ingroup ITKCommon
 */
template <typename TA, typename TB> struct PromoteType
{
  static TA a;
  static TB b;

  // Aimed at supporting overloads
  template <typename T> static Details::Identity<1>::Type& Check(typename Details::SizeToType<1,  TA, TB>::Type, T);
  template <typename T> static Details::Identity<2>::Type& Check(typename Details::SizeToType<2,  TA, TB>::Type, T);

  // Common numeric types
  static Details::Identity<3 >::Type& Check(typename Details::SizeToType<3,  TA, TB>::Type, int);
  static Details::Identity<4 >::Type& Check(typename Details::SizeToType<4,  TA, TB>::Type, int);
  static Details::Identity<5 >::Type& Check(typename Details::SizeToType<5,  TA, TB>::Type, int);
  static Details::Identity<6 >::Type& Check(typename Details::SizeToType<6,  TA, TB>::Type, int);
  static Details::Identity<7 >::Type& Check(typename Details::SizeToType<7,  TA, TB>::Type, int);
  static Details::Identity<8 >::Type& Check(typename Details::SizeToType<8,  TA, TB>::Type, int);
  static Details::Identity<9 >::Type& Check(typename Details::SizeToType<9,  TA, TB>::Type, int);
  static Details::Identity<10>::Type& Check(typename Details::SizeToType<10, TA, TB>::Type, int);
  static Details::Identity<11>::Type& Check(typename Details::SizeToType<11, TA, TB>::Type, int);
  static Details::Identity<12>::Type& Check(typename Details::SizeToType<12, TA, TB>::Type, int);
  static Details::Identity<13>::Type& Check(typename Details::SizeToType<13, TA, TB>::Type, int);
public:
  /** Type result of operations between \c TA and \c TB.
   * \internal
   * We let the compiler deduce the type resulting of the operation `a+b`. As
   * the standard says (see the extract in the test file itkPromoteType.cxx),
   * <em>«arithmetic operators do not accept types smaller than int as
   * arguments»</em>, that's why \c ITK_ASSOCIATE() is not used with \c char
   * nor \c short.
   *
   * Then, we let the compiler choose the right \c Check() overload. From here,
   * it'll know which \c Detail::Identity<> specialization is returned.
   * \c sizeof can thus be used to extract the size of the specialization.
   *
   * Finally, \c Details::SizeToType<> returns the type that holds the result
   * of `a+b`.
   */
  typedef typename Details::SizeToType<sizeof Check(a+b, 0), TA, TB>::Type Type;
};
} // itk::mpl namespace

/// \endcond
} // itk namespace

#endif // itkPromoteType_h
