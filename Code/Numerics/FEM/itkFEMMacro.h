/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMacro.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMMacro_h
#define __itkFEMMacro_h




/**
 * \file itkFEMMacro.h
 * \brief Definitions of macros used in FEM code.
 *
 * itkFEMMacro.h defines macros that allow simple and consistent FEM code
 * creation. Use these macros whenever posible (always)!
 */


/**
 * \brief If defined, FEM classes will use smart pointers.
 *
 * Define this macro if you want to compile the FEM classes so that
 * they use itk's SmartPointer object instead of standard c++ pointers.
 */
//#define FEM_USE_SMART_POINTERS




/**
 * \brief If defined, FEM classes will include routines for drawing
 *        on the device context.
 *
 * Define this macro if you want to compile the FEM Element and Node
 * classes so that they include Draw() virtual member function. Calling
 * this function draws the element or node on the specified windows
 * device context.
 *
 * \note This only works on Windows systems and requires MFC classes.
 * If you need to define this macro, make sure that you define it
 * both when compiling the FEM library as well as when you're using it,
 * because class declarations depend on it.
 * \sa Element::Draw()
 */
//#define FEM_BUILD_VISUALIZATION




/** 
 * This must be included before itk includes windows.h, otherwise nobody
 * can ever use MFC again. Including it here also ensures that all
 * FEM classes that require MFC, automatically get it, as long as they
 * include this file.
 */
#ifdef FEM_BUILD_VISUALIZATION
#include <afxwin.h>        /* required to draw the element on device context */
#endif

#include "itkFEMObjectFactory.h"




/* Required includes for itk's SmartPointer compatibility */
#ifdef FEM_USE_SMART_POINTERS
#include "itkMacro.h"
#include "itkSmartPointer.h"
#include "itkObjectFactory.h"
#include "itkLightObject.h"
#endif




/**
 * \def FEM_ABSTRACT_CLASS(thisClass,parentClass)
 * \brief Defines typedefs for pointers to class.
 *
 * This macro should be called immediately after the { in class declaration.
 * It defines Self, Superclass, Pointer and ConstPointer typedef members in
 * a class. It also includes all the necessary typedefs for compatibility
 * when SmartPointer classes are used (itkTypeMacro).
 *
 * \param thisClass Name of the class that is being declared.
 * \param parentClass Name of the class from which the current class is
 *        being derived. If this is the base class that is not derived from 
 *        anything, let parentClass=thisClass.
 *
 * \note Use this macro only for abstract classes that can't be instantiated.
 *       Otherwise use #FEM_CLASS macro.
 */
#ifndef FEM_USE_SMART_POINTERS

#define FEM_ABSTRACT_CLASS(thisClass,parentClass)           \
public:                                               \
  /** Standard "Self" typedef.*/                      \
  typedef thisClass Self;                             \
  /** Standard "Superclass" typedef. */               \
  typedef parentClass Superclass;                     \
  /**  Pointer or SmartPointer to an object. */       \
  typedef Self* Pointer;                              \
  /**  Const pointer or SmartPointer to an object. */ \
  typedef const Self* ConstPointer;                   \
private:  // everything that follows from here is private by default (like in the beginning of class)

#else

#define FEM_ABSTRACT_CLASS(thisClass,parentClass)  \
public:                                      \
  /** Standard "Self" typedef.*/             \
  typedef thisClass Self;                    \
  /** Standard "Superclass" typedef. */      \
  typedef parentClass Superclass;            \
  /** SmartPointer to an object. */          \
  typedef SmartPointer<Self> Pointer;        \
  /** const SmartPointer to an object. */    \
  typedef SmartPointer<const Self> ConstPointer;  \
  itkTypeMacro(thisClass,parentClass)        \
private:  // everything that follows from here is private by default (like in the beginning of class)

#endif




/**
 * \def FEM_CLASS(thisClass,parentClass)
 * \brief Defines typedefs for pointers to class.
 *
 * This macro should be called immediately after the { in class declaration.
 * It first calls the #FEM_ABSTRACT_CLASS macro. In addition it defines the Clone()
 * function, CLID member that holds the class ID for FEMObjectFactory. Also,
 * the New() static member is defined, as required, for compatibility with
 * SmartPointer classes (itkNewMacro is called).
 *
 * \param thisClass Name of the class that is being declared.
 * \param parentClass Name of the class from which the current class is
 *        being derived.
 *
 * \note Macro assumes that the Baseclass typedef is already present and
 *       specifies the base class from which the current class is derived.
 *       The Baseclass typedef is used as a return value of a Clone() function.
 */
#ifndef FEM_USE_SMART_POINTERS
  #define FEM_CLASS(thisClass,parentClass)   \
    /*  Pointers.... */                      \
    FEM_ABSTRACT_CLASS(thisClass,parentClass)      \
  public:                                    \
    /** Create a new object from the existing one  */ \
    virtual Baseclass::Pointer Clone() const \
      { return new Self(*this); }            \
    /** Object creation in an itk compatible way */ \
    static Pointer New()                     \
      { return new Self(); }                 \
    /** Same as New() but returns pointer to base class */ \
    static Baseclass::Pointer NewB()         \
      { return New(); }                      \
    /** Class ID for FEM object factory */   \
    static int CLID(void);                   \
    /** Virtual function to access the class ID */ \
    virtual int ClassID() const              \
      { return CLID(); }                     \
  private:  // everything that follows from here is private by default (like in the beginning of class)
#else
  #define FEM_CLASS(thisClass,parentClass)   \
    /*  Pointers.... */                      \
    FEM_ABSTRACT_CLASS(thisClass,parentClass)      \
  public:                                    \
    /** Create a new object from the existing one */  \
    virtual Baseclass::Pointer Clone() const \
      { Pointer o=new Self(*this);           \
        o->SetReferenceCount(1);             \
        return o; }                          \
    /** Object creation through itk's objectfactory  */ \
    itkNewMacro(Self)                        \
    /** Same as New() but returns pointer to base class */ \
    static Baseclass::Pointer NewB()         \
      { return New(); }                      \
    /** Class ID for FEM object factory */   \
    static int CLID(void)                    \
    /** Virtual function to access the class ID */ \
    virtual int ClassID() const              \
      { return CLID(); }                     \
  private:  // everything that follows from here is private by default (like in the beginning of class)
#endif



/**
 * \def FEM_CLASS_REGISTER(thisClass)
 * \brief Register the specified class with FEMObjectFactory.
 *
 * Registering is required for every class that the object factory will
 * later be able to create. The class must contain static const int
 * member CLID and must define or inherit Baseclass typedef. This is
 * automatic if #FEM_CLASS macro was used when declaring a class.
 * CLID is initialized to a value assigned by the FEMObjectFactory.
 *
 * This macro provides the definition for CLID static member function
 * of a class. This function can't be defined inline.
 *
 * \param thisClass Name of the class that needs to be registered with
 *        FEMObjectFactory.
 *
 * \note Call this macro after the class definition is complete in .cxx
 *       file but still within itk::fem namespace.
 */
// FIXME: Remove definition, when no longer required.
#define FEM_CLASS_REGISTER(thisClass) \
  int thisClass::CLID(void) \
  { static const int CLID_ = FEMObjectFactory<thisClass::Baseclass>::Register( thisClass::NewB, #thisClass); \
    return CLID_; }




namespace itk {
namespace fem {

/**
 * \class INITClass
 * \brief Class that is used in #FEM_CLASS_INIT macro.
 */
struct INITClass {
  INITClass(int i) {
    /*
     * Do something with the passed variable to
     * make sure that it is evaluated. This should
     * avoid all optimizations that compiler may
     * want to perform.
     */
    volatile int Dummy=i;
    Dummy++;
  }
};

}} // end namespace itk::fem



/**
 * \def FEM_CLASS_INIT(thisClass)
 * \brief Perform any initialization tasks for a class.
 *
 * This macro creates a static object of INITClass class that references
 * thisClass::CLID static member in a constructor. This insures that
 * any initialization code for CLID is always executed, and thisClass
 * is properly registered with FEMObjectFactory.
 *
 * \param thisClass Name of the class that needs to be initialized.
 *
 * \note Call this macro in .h file after class declaration and
 *       within itk::fem namespace.
 */
#define FEM_CLASS_INIT(thisClass) \
  static INITClass Initializer_##thisClass(thisClass::CLID());



#endif // #ifndef __itkFEMMacro_h
