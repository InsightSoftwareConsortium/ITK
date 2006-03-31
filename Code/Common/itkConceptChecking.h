/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConceptChecking.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConceptChecking_h
#define __itkConceptChecking_h

#include "itkPixelTraits.h"

/** Choose a concept checking implementation based on compiler abilities. */
#ifndef ITK_CONCEPT_NO_CHECKING
#  if defined(_MSC_VER) && !defined(__ICL)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  elif defined(__BORLANDC__) && (__BORLANDC__ <= 0x551)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  elif defined(__MWERKS__) && (__MWERKS__ <= 0x3002)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  elif defined(__SUNPRO_CC)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  else
#    define ITK_CONCEPT_IMPLEMENTATION_STANDARD
#  endif
#endif

/** Define the concept checking implementation chosen above. */
#if defined(ITK_CONCEPT_IMPLEMENTATION_STANDARD)

/**
 * Standard instantiation-time concept check.  No run-time overhead
 * introduced.  This implementation is based on "Concept Checking:
 * Binding Parametric Polymorphism in C++" by Jeremy Siek and Andrew
 * Lumsdaine, University of Notre Dame.
 */

// Leave ()'s off the sizeof to force the caller to pass them in the
// concept argument of the itkConceptMacro.  This is necessary because
// the argument may contain commas.
#  define itkConceptConstraintsMacro() \
    template <void (Constraints::*)()> struct Enforcer {}; \
    typedef Enforcer<&Constraints::constraints> EnforcerInstantiation
#  define itkConceptMacro(name, concept) enum { name = sizeof concept }

#elif defined(ITK_CONCEPT_IMPLEMENTATION_VTABLE)

/**
 * Alternate implementation for some compilers.  This introduces no
 * run-time overhead.  The "vtable" approach was invented for this
 * project by Brad King at Kitware.
 */
#  define itkConceptConstraintsMacro() \
    virtual void Enforcer() { &Constraints::constraints; }
#  define itkConceptMacro(name, concept) enum { name = sizeof concept }

#elif defined(ITK_CONCEPT_IMPLEMENTATION_CALL)

/** Not implemented.  */
#  define itkConceptConstraintsMacro()
#  define itkConceptMacro(name, concept) enum { name = 0 }

#else

/** Disable concept checking.  */
#  define itkConceptConstraintsMacro()
#  define itkConceptMacro(name, concept) enum { name = 0 }

#endif

namespace itk
{

/** All concept class definitions are contained in the "itk::Concept"
    namespace. */
namespace Concept
{

/**
 * Some concept implementation details are adapted from the BOOST C++
 * libraries (www.boost.org).  These are marked with "(BOOST)" in the
 * corresponding comment.
 */

/** Namespace containing concept check implementation details. */
namespace Detail
{

template <typename T> struct UniqueType {};
template <int> struct UniqueType_int {};
template <unsigned int> struct UniqueType_unsigned_int {};
template <bool> struct UniqueType_bool {};


/**
 * Concept checks may require a variable to be declared but not used.
 * This function can be called with the variable to prevent the compiler
 * warning. (BOOST)
 */
template <typename T> inline void IgnoreUnusedVariable(T) {}

/**
 * Concept checks may require that an expression be convertible to bool.
 * Passing the expression to this function will enforce this requirement.
 * (BOOST)
 */
template <class T>
void RequireBooleanExpression(const T& t)
{
  bool x = t;
  IgnoreUnusedVariable(x);
}

} // namespace Detail


/** Concept requiring T to have a default constructor. (BOOST) */
template <typename T>
struct DefaultConstructible
{
  struct Constraints
  {
    void constraints()
      {
      T a;
      Detail::IgnoreUnusedVariable(a);
      }
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have a copy constructor. (BOOST) */
template <typename T>
struct CopyConstructible
{
  struct Constraints
  {
    void constraints()
      {
      T a(b);
      T* p = &a;
      const_constraints(a);
      Detail::IgnoreUnusedVariable(p);
      }
    void const_constraints(const T& a)
      {
      T c(a);
      const T* p = &a;
      Detail::IgnoreUnusedVariable(c);
      Detail::IgnoreUnusedVariable(p);
      }
    T b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to be convertible to T2. (BOOST) */
template <typename T1, typename T2>
struct Convertible
{
  struct Constraints
  {
    void constraints()
      {
      T2 b = static_cast<T2>(a);
      Detail::IgnoreUnusedVariable(b);
      }
    T1 a;
  };
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operator =.  (BOOST) */
template <typename T>
struct Assignable
{
  struct Constraints
  {
    void constraints()
      {
      a = a;
      const_constraints(a);
      }
    void const_constraints(const T& b)
      {
      a = b;
      }
    T a;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to have operators < and <= with a right-hand operator
    of type T2.  (BOOST) */
template <typename T1, typename T2=T1>
struct LessThanComparable
{
  struct Constraints
  {
    void constraints()
      {
      Detail::RequireBooleanExpression(a < b);
      Detail::RequireBooleanExpression(a <= b);
      }
    T1 a;
    T2 b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to have operators > and >= with a right-hand operator
    of type T2.  (BOOST) */
template <typename T1, typename T2=T1>
struct GreaterThanComparable
{
  struct Constraints
  {
    void constraints()
      {
      Detail::RequireBooleanExpression(a > b);
      Detail::RequireBooleanExpression(a >= b);
      }
    T1 a;
    T2 b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to have operators == and != with a right-hand operator
    of type T2.  (BOOST) */
template <typename T1, typename T2=T1>
struct EqualityComparable
{
  struct Constraints
  {
    void constraints()
      {
      Detail::RequireBooleanExpression(a == b);
      Detail::RequireBooleanExpression(a != b);
      }
    T1 a;
    T2 b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to have operators <, >, <=, >=, ==, != with a
    right-hand operator of type T2. (BOOST) */
template <typename T1, typename T2=T1>
struct Comparable
{
  struct Constraints
  {
    void constraints()
      {
      Detail::RequireBooleanExpression(a < b);
      Detail::RequireBooleanExpression(a > b);
      Detail::RequireBooleanExpression(a <= b);
      Detail::RequireBooleanExpression(a >= b);
      Detail::RequireBooleanExpression(a == b);
      Detail::RequireBooleanExpression(a != b);
      }
    T1 a;
    T2 b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to have operators +, -, +=, -= in the form 
    T1 op T2 = T3.  */
template <typename T1, typename T2=T1, typename T3=T1>
struct AdditiveOperators
{
  struct Constraints
  {
    void constraints()
      {
      a = b + c;
      a = b - c;
      a += c;
      a -= c;
      const_constraints(b, c);
      }
    void const_constraints(const T1& d, const T2& e)
      {
      a = d + e;
      a = d - e;
      a += e;
      a -= e;
      }
    T3 a;
    T1 b;
    T2 c;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operator * in the form
    T1 op T2 = T3. */
template <typename T1, typename T2=T1, typename T3=T1>
struct MultiplyOperator
{
  struct Constraints
  {
    void constraints()
      {
      a = b * c;
      const_constraints(b, c);
      }
    void const_constraints(const T1& d, const T2& e)
      {
      a = d * e;
      }
    T3 a;
    T1 b;
    T2 c;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operator  *= in the form
    T2 op= T1. */
template <typename T1, typename T2=T1>
struct MultiplyAndAssignOperator
{
  struct Constraints
  {
    void constraints()
      {
      a *= b;
      const_constraints(b);
      }
    void const_constraints(const T1& d)
      {
      a *= d;
      }
    T2 a;
    T1 b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operators / and /= in the form
    T1 op T2 = T3. */
template <typename T1, typename T2=T1, typename T3=T1>
struct DivisionOperators
{
  struct Constraints
  {
    void constraints()
      {
      a = b / c;
      a /= c;
      const_constraints(b, c);
      }
    void const_constraints(const T1& d, const T2& e)
      {
      a = d / e;
      a /= e;
      }
    T3 a;
    T1 b;
    T2 c;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T1 to have operators &, |, and ^ in the form 
    T1 op T2 = T3.  */
template <typename T1, typename T2=T1, typename T3=T1>
struct LogicalOperators
{
  struct Constraints
  {
    void constraints()
      {
      a = b & c;
      a = b | c;
      a = b ^ c;
      a &= c;
      a |= c;
      a ^= c;
      const_constraints(b, c);
      }
    void const_constraints(const T1& d, const T2& e)
      {
      a = d & e;
      a = d | e;
      a = d ^ e;
      a &= e;
      a |= e;
      a ^= e;
      }
    T3 a;
    T1 b;
    T2 c;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operator !.  */
template <typename T>
struct NotOperator
{
  struct Constraints
  {
    void constraints()
      {
      a = !a;
      }
    T a;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operators ++ and --.  */
template <typename T>
struct IncrementDecrementOperators
{
  struct Constraints
  {
    void constraints()
      {
        a++;
        a--;
        ++a;
        --a;
      }
    T a;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to be writable to an ostream.  */
template <typename T>
struct OStreamWritable
{
  struct Constraints
  {
    void constraints()
      {
        std::cout << a;
      }
    T a;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to be signed. */
template <typename T>
struct Signed
{
  typedef Signed Self;
  itkStaticConstMacro(IsSigned, bool, NumericTraits<T>::is_signed);
  struct Constraints
  {
    typedef Detail::UniqueType_bool<true> TrueT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(IsSigned)> SignedT;
    void constraints()
      {
        SignedT a = TrueT();
        Detail::IgnoreUnusedVariable(a);
      }
  };
  
  itkConceptConstraintsMacro();
};
  
/** Concept requiring T1 and T2 to be the same type. */
template <typename T1, typename T2>
struct SameType
{
  struct Constraints
  {
    void constraints()
      {
        Detail::UniqueType<T1> a = Detail::UniqueType<T2>();
        Detail::IgnoreUnusedVariable(a);
      }
  };
  itkConceptConstraintsMacro();
};

/** Concept requiring D1 and D2 to be the same dimension. */
template <unsigned int D1, unsigned int D2>
struct SameDimension
{
  struct Constraints
  {
    typedef Detail::UniqueType_unsigned_int<D1> DT1;
    typedef Detail::UniqueType_unsigned_int<D2> DT2;
    void constraints()
      {
        DT1 a = DT2();
        Detail::IgnoreUnusedVariable(a);
      }
  };
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have NumericTraits */
template <typename T>
struct HasNumericTraits
{
  struct Constraints
  {
    void constraints()
      { 
        typedef typename NumericTraits<T>::ValueType ValueType;
        typedef typename NumericTraits<T>::PrintType PrintType;
        typedef typename NumericTraits<T>::AbsType AbsType;
        typedef typename NumericTraits<T>::AccumulateType AccumulateType;
        typedef typename NumericTraits<T>::RealType RealType;
        typedef typename NumericTraits<T>::ScalarRealType ScalarRealType;
        typedef typename NumericTraits<T>::FloatType FloatType;
        T a;
        bool b;
        a = NumericTraits<T>::Zero;
        a = NumericTraits<T>::One;
        a = NumericTraits<T>::NonpositiveMin();
        a = NumericTraits<T>::ZeroValue();
        b = NumericTraits<T>::IsPositive(a);
        b = NumericTraits<T>::IsNonpositive(a);
        b = NumericTraits<T>::IsNegative(a);
        b = NumericTraits<T>::IsNonnegative(a);
        Detail::IgnoreUnusedVariable(a);
        Detail::IgnoreUnusedVariable(b);
      }
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have PixelTraits */
template <typename T>
struct HasPixelTraits
{
  struct Constraints
  {
    void constraints()
      { 
      typedef typename PixelTraits<T>::ValueType ValueType;
      unsigned int a = PixelTraits<T>::Dimension;
      Detail::IgnoreUnusedVariable(a);
      }
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have JoinTraits */
template <typename T1, typename T2>
struct HasJoinTraits
{
  struct Constraints
  {
    void constraints()
      { 
        typedef typename JoinTraits<T1, T2>::ValueType ValueType;
      }
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring D1 and D2 to be the same dimension or D2-1 = D1. */
template <unsigned int D1, unsigned int D2>
struct SameDimensionOrMinusOne
{
  struct Constraints
  {
    typedef Detail::UniqueType_unsigned_int< D1 > Type1;
    typedef Detail::UniqueType_unsigned_int< D1-1 > Type2;

    void f( Type1 ) {}
    void f( Type2, int = 0 ) {}

    void constraints()
      {
      Detail::UniqueType_unsigned_int< D2 > tt;
      this->f( tt );
      }
  };
  itkConceptConstraintsMacro();
};

/** Concept requiring T to be integer. */
template <typename T>
struct IsInteger
{
  typedef IsInteger Self;
  itkStaticConstMacro(Integral, bool, NumericTraits<T>::is_integer);
  struct Constraints
  {
    typedef Detail::UniqueType_bool<true> TrueT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(Integral)> IntegralT;
    void constraints()
      {
        IntegralT a = TrueT();
        Detail::IgnoreUnusedVariable(a);
      }
  };
  
  itkConceptConstraintsMacro();
};
  
/** Concept requiring T to be non-integer. */
template <typename T>
struct IsNonInteger
{
  typedef IsNonInteger Self;
  itkStaticConstMacro(NonIntegral, bool, NumericTraits<T>::is_integer);
  struct Constraints
  {
    typedef Detail::UniqueType_bool<false> FalseT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(NonIntegral)> NonIntegralT;
    void constraints()
      {
        NonIntegralT a = FalseT();
        Detail::IgnoreUnusedVariable(a);
      }
  };
  
  itkConceptConstraintsMacro();
};
  
/** Concept requiring T to be floating point. */
template <typename T>
struct IsFloatingPoint
{
  typedef IsFloatingPoint Self;
  itkStaticConstMacro(Integral, bool, NumericTraits<T>::is_integer);
  itkStaticConstMacro(IsExact, bool, NumericTraits<T>::is_exact);
  struct Constraints
  {
    typedef Detail::UniqueType_bool<false> FalseT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(Integral)> IntegralT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(IsExact)> ExactT;
    void constraints()
      {
        IntegralT a = FalseT();
        ExactT b = FalseT();
        Detail::IgnoreUnusedVariable(a);
        Detail::IgnoreUnusedVariable(b);
      }
  };
  
  itkConceptConstraintsMacro();
};
  
/** Concept requiring T to be fixed point. */
template <typename T>
struct IsFixedPoint
{
  typedef IsFixedPoint Self;
  itkStaticConstMacro(Integral, bool, NumericTraits<T>::is_integer);
  itkStaticConstMacro(IsExact, bool, NumericTraits<T>::is_exact);
  struct Constraints
  {
    typedef Detail::UniqueType_bool<true> TrueT;
    typedef Detail::UniqueType_bool<false> FalseT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(Integral)> IntegralT;
    typedef Detail::UniqueType_bool<itkGetStaticConstMacro(IsExact)> ExactT;
    void constraints()
      {
        IntegralT a = FalseT();
        ExactT b = TrueT();
        Detail::IgnoreUnusedVariable(a);
        Detail::IgnoreUnusedVariable(b);
      }
  };
  
  itkConceptConstraintsMacro();
};
  
} // end namespace Concept

} // end namespace itk

#endif
