/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConceptChecking.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkConceptChecking_h
#define _itkConceptChecking_h

#include "itkMacro.h"

/**
 * Concept checking based on paper
 * "Concept Checking: Binding Parametric Polymorphism in C++"
 * by Jeremy Siek and Andrew Lumsdaine, University of Notre Dame.
 *
 * ITK_CLASS_REQUIRES macros adapted from BOOST libraries (www.boost.org).
 * Several standard concepts also adapted from BOOST.
 */

#if !defined(ITK_CONCEPT_CHECKING)

// Define fake concept specification macros.
// Concept checks will not be done.

#define ITK_FUNCTION_REQUIRES(type_var, concept)
#define ITK_FUNCTION_REQUIRES2(type_var1, type_var2, concept)
#define ITK_FUNCTION_REQUIRES3(type_var1, type_var2, type_var3, concept)
#define ITK_FUNCTION_REQUIRES4(type_var1, type_var2, type_var3, type_var4, concept)

#define ITK_CLASS_REQUIRES(type_var, concept)
#define ITK_CLASS_REQUIRES2(type_var1, type_var2, concept)
#define ITK_CLASS_REQUIRES3(type_var1, type_var2, type_var3, concept)
#define ITK_CLASS_REQUIRES4(type_var1, type_var2, type_var3, type_var4, concept)

#else

// Define real concept specification macros.
// Concept checks will be done.

#define ITK_FUNCTION_REQUIRES(type_var, concept) \
  void (::itk::Concepts::concept <type_var>::* var##type_var##concept)() = &::itk::Concepts::concept<type_var>::constraints; \
  ::itk::Concepts::ignore_unused_variable(var##type_var##concept)

#define ITK_FUNCTION_REQUIRES2(type_var1, type_var2, concept) \
  void (::itk::Concepts::concept <type_var1,type_var2>::* var##type_var1##type_var2##concept)() = &::itk::Concepts::concept<type_var1,type_var2>::constraints; \
  ::itk::Concepts::ignore_unused_variable(var##type_var1##type_var2##concept)

#define ITK_FUNCTION_REQUIRES3(type_var1, type_var2, type_var3, concept) \
  void (::itk::Concepts::concept <type_var1,type_var2,type_var3>::* var##type_var1##type_var2##type_var3##concept)() = &::itk::Concepts::concept<type_var1,type_var2,type_var3>::constraints; \
  ::itk::Concepts::ignore_unused_variable(var##type_var1##type_var2##type_var3##concept)

#define ITK_FUNCTION_REQUIRES4(type_var1, type_var2, type_var3, type_var4, concept) \
  void (::itk::Concepts::concept <type_var1,type_var2,type_var3,type_var4>::* var##type_var1##type_var2##type_var3##type_var4##concept)() = &::itk::Concepts::concept<type_var1,type_var2,type_var3,type_var4>::constraints; \
  ::itk::Concepts::ignore_unused_variable(var##type_var1##type_var2##type_var3##type_var4##concept)

#define ITK_CLASS_REQUIRES(type_var, concept) \
  typedef void (::itk::Concepts::concept <type_var>::* func##type_var##concept)(); \
  template <func##type_var##concept> \
  struct concept_checking_##type_var##concept { }; \
  typedef concept_checking_##type_var##concept< \
    &::itk::Concepts::concept <type_var>::constraints> \
    concept_checking_typedef_##type_var##concept

#define ITK_CLASS_REQUIRES2(type_var1, type_var2, concept) \
  typedef void (::itk::Concepts::concept <type_var1,type_var2>::* func##type_var1##type_var2##concept)(); \
  template <func##type_var1##type_var2##concept> \
  struct concept_checking_##type_var1##type_var2##concept { }; \
  typedef concept_checking_##type_var1##type_var2##concept< \
    &::itk::Concepts::concept <type_var1,type_var2>::constraints> \
    concept_checking_typedef_##type_var1##type_var2##concept

#define ITK_CLASS_REQUIRES3(type_var1, type_var2, type_var3, concept) \
  typedef void (::itk::Concepts::concept <type_var1,type_var2,type_var3>::* func##type_var1##type_var2##type_var3##concept)(); \
  template <func##type_var1##type_var2##type_var3##concept> \
  struct concept_checking_##type_var1##type_var2##type_var3##concept { }; \
  typedef concept_checking_##type_var1##type_var2##type_var3##concept< \
    &::itk::Concepts::concept <type_var1,type_var2,type_var3>::constraints>  \
  concept_checking_typedef_##type_var1##type_var2##type_var3##concept

#define ITK_CLASS_REQUIRES4(type_var1, type_var2, type_var3, type_var4, concept) \
  typedef void (::itk::Concepts::concept <type_var1,type_var2,type_var3,type_var4>::* func##type_var1##type_var2##type_var3##type_var4##concept)(); \
  template <func##type_var1##type_var2##type_var3##type_var4##concept> \
  struct concept_checking_##type_var1##type_var2##type_var3##type_var4##concept { }; \
  typedef concept_checking_##type_var1##type_var2##type_var3##type_var4##concept< \
    &::itk::Concepts::concept <type_var1,type_var2,type_var3,type_var4>::constraints>  \
    concept_checking_typedef_##type_var1##type_var2##type_var3##type_var4##concept

#endif

namespace itk
{
namespace Concepts
{

/**
 * Concept checks may require a variable to be declared but not used.
 * This function can be called with the variable to prevent the compiler
 * warning.
 */
template <typename T>
inline void ignore_unused_variable(const T&)
{
}


/*@{
 * Utility class to guarantee a unique type.
 */
template <typename T> struct UniqueTypeForType {};
template <int> struct UniqueTypeFor_int {};
template <unsigned int> struct UniqueTypeFor_unsigned_int {};
//@}

  
/**
 * Concept that requires a default constructor.
 */
template <typename T>
struct DefaultConstructorConcept
{
  void constraints()
    {
      T a;
      ignore_unused_variable(a);
    }
};


/**
 * Concept that requires a copy constructor.
 */
template <class T>
struct CopyConstructorConcept
{
  void constraints()
    {
      T b(a);
    }
  void const_constraints(const T& b)
    {
      T c(b); // require copy constructor to take a const argument.
      ignore_unused_variable(c);
    }
  T a;
};


/**
 * Concept that requires an assignment operator.
 */
template <typename T>
struct AssignmentOperatorConcept
{
  void constraints()
    {
      a = a;
      const_constraints(a);
    }
  void const_constraints(const T& b)
    {
      a = b; // require const argument to assignment operator.
    }
  T a;
};


/**
 * Concept that requires math operators +,-,+=,-=, =.
 */
template <typename T>
struct AdditiveOperatorsConcept
{
  void constraints()
    {
      // Require that the operators exist, and that their results can
      // convert back to type T in an assignment.
      a = b + b;
      a = b - b;
      a += b;
      a -= b;
      const_constraints(b);
    }
  void const_constraints(const T& c)
    {
      // Require that operators work with const argument types.
      a = c + c;
      a = c - c;
      a += c;
      a -= c;
    }
  T a, b;
};


/**
 * Concept that requires math operators *,/,*=,/=, =.
 */
template <typename T>
struct MultiplicativeOperatorsConcept
{
  void constraints()
    {
      // Require that the operators exist, and that their results can
      // convert back to type T in an assignment.
      a = b * b;
      a = b / b;
      a *= b;
      a /= b;
      const_constraints(b);
    }
  void const_constraints(const T& c)
    {
      // Require that operators work with const argument types.
      a = c * c;
      a = c / c;
      a *= c;
      a /= c;      
    }
  T a, b;
};


/**
 * Concept that requires a conversion from its first type to its second.
 */
template <typename T1, typename T2>
struct ConvertibleConcept
{
  void constraints()
    {
      T2 b = a;
      ignore_unused_variable(b);
    }
  T1 a;
};


/**
 * Concept that requires two types to be exactly the same.
 */
template <typename T1, typename T2>
struct SameTypeConcept
{
  void constraints()
    {
      UniqueTypeForType<T1> x = UniqueTypeForType<T2>();
      ignore_unused_variable(x);
    }
};


/**
 * Concept that requires two dimension values to be the same.
 */
template <unsigned int D1, unsigned int D2>
struct SameDimensionConcept
{
  void constraints()
    {
      UniqueTypeFor_unsigned_int<D1> x = UniqueTypeFor_unsigned_int<D2>();
      ignore_unused_variable(x);
    }
};


/**
 * Concept that requires the standard "Self", "Pointer", and "ConstPointer"
 * typedefs in a class.
 */
template <class T>
struct itkStandardTypedefsConcept
{
  void constraints()
    {
      typedef typename T::Self Self;
      typedef typename T::Pointer Pointer;
      typedef typename T::ConstPointer ConstPointer;
    }
};

} // namespace Concepts
} // namespace itk

#endif
