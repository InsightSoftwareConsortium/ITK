/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConceptChecking.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConceptChecking_h
#define _itkConceptChecking_h

/** Choose a concept checking implementation based on compiler abilities. */
#ifndef ITK_CONCEPT_NO_CHECKING
#  if defined(_MSC_VER) && !defined(__ICL)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  elif defined(__BORLANDC__) && (__BORLANDC__ <= 0x551)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  elif defined(__MWERKS__) && (__MWERKS__ <= 0x3002)
#    define ITK_CONCEPT_IMPLEMENTATION_VTABLE
#  elif defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x500)
#    define ITK_CONCEPT_IMPLEMENTATION_CALL
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
      T2 b = a;
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

/** Concept requiring T to have operator <.  (BOOST) */
template <typename T>
struct LessThanComparable
{
  struct Constraints
  {
    void constraints()
      {
      Detail::RequireBooleanExpression(a < b);
      }
    T a, b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operators == and != (BOOST) */
template <typename T>
struct EqualityComparable
{
  struct Constraints
  {
    void constraints()
      {
      Detail::RequireBooleanExpression(a == b);
      Detail::RequireBooleanExpression(a != b);
      }
    T a, b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operators <, >, <=, >=, ==, !=. (BOOST) */
template <typename T>
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
    T a, b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operators +, -, +=, -=. */
template <typename T>
struct AdditiveOperators
{
  struct Constraints
  {
    void constraints()
      {
      a = b + b;
      a = b - b;
      a += b;
      a -= b;
      const_constraints(b);
      }
    void const_constraints(const T& c)
      {
      a = c + c;
      a = c - c;
      a += c;
      a -= c;
      }
    T a, b;
  };
  
  itkConceptConstraintsMacro();
};

/** Concept requiring T to have operators *, /, *=, /=. */
template <typename T>
struct MultiplicativeOperators
{
  struct Constraints
  {
    void constraints()
      {
      a = b * b;
      a = b / b;
      a *= b;
      a /= b;
      const_constraints(b);
      }
    void const_constraints(const T& c)
      {
      a = c * c;
      a = c / c;
      a *= c;
      a /= c;
      }
    T a, b;
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

} // namespace Concept

} // namespace itk

#endif
