/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMMacro.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * Define this macro if you want to compile the FEM classes so that
 * they use itk's SmartPointer object instead of standard c++ pointers.
 * This macro should be defined (if required) on a command line of a
 * compiler. Normally this is done automatically by CMake.
 *
 * \note If you need to define this macro, make sure that you define it
 * both when compiling the FEM library as well as when you're using it,
 * because class declarations depend on it.
 */
//#define FEM_USE_SMART_POINTERS




/**
 * \brief If defined, FEM classes will include routines for drawing
 *        on the device context.
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
 * classes which require MFC, automatically get it, as long as they
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
 * \brief Defines typedefs for pointers to class.
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

#define FEM_CLASS_SP(thisClass,parentClass)           \
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

#define FEM_CLASS_SP(thisClass,parentClass)  \
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
 * \brief Defines typedefs for pointers to class.
 * This macro should be called immediately after the { in class declaration.
 * It first calls the #FEM_CLASS_SP macro. In addition it defines the Clone()
 * function, OFID member that holds the class ID for FEMObjectFactory. Also,
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
    FEM_CLASS_SP(thisClass,parentClass)      \
  public:                                    \
    /** Create a new object from the existing one  */ \
    virtual Baseclass::Pointer Clone() const \
      { return new Self(*this); }            \
    /** Class ID for FEM object factory */   \
    static const int OFID;                   \
  private:  // everything that follows from here is private by default (like in the beginning of class)
#else
  #define FEM_CLASS(thisClass,parentClass)   \
    /*  Pointers.... */                      \
    FEM_CLASS_SP(thisClass,parentClass)      \
  public:                                    \
    /** Create a new object from the existing one */  \
    virtual Baseclass::Pointer Clone() const \
      {  Self::Pointer o=new Self(*this);    \
        o->SetReferenceCount(1);             \
        return o; }                          \
    /** Class ID for FEMObjectFactory */     \
    static const int OFID;                   \
    /** Object creation through itk's objectfactory  */ \
    itkNewMacro(Self)                        \
  private:  // everything that follows from here is private by default (like in the beginning of class)
#endif



/**
 * \brief Register the specified class with FEMObjectFactory.
 * Registering is required for every class that the object factory will
 * later be able to create. The class must contain static const int
 * member OFID and must define or inherit Baseclass typedef. This is
 * automatic if #FEM_CLASS macro was used when declaring a class.
 * OFID is initialized to a value assigned by the FEMObjectFactory.
 *
 * \param thisClass Name of the class that needs to be registered with
 *        FEMObjectFactory.
 *
 * \note Call this macro after the class definition is complete in .cxx
 *       file but still within itk::fem namespace.
 */
#ifndef FEM_USE_SMART_POINTERS
  #define FEM_CLASS_REGISTER(thisClass) \
  namespace { static thisClass::Baseclass::Pointer New##thisClass() { return new thisClass; } }\
  const int thisClass::OFID=FEMObjectFactory<thisClass::Baseclass>::Register( New##thisClass, #thisClass);
#else
  #define FEM_CLASS_REGISTER(thisClass) \
  namespace { static thisClass::Baseclass::Pointer New##thisClass() { return thisClass::New(); } }\
  const int thisClass::OFID=FEMObjectFactory<thisClass::Baseclass>::Register( New##thisClass, #thisClass);
#endif




/**
 * \brief Perform any initialization tasks for a class.
 * The macro creates a constant reference to class OFID that is globally
 * accesable. This also insures that the class is properly registered with
 * FEMObjectFactory.
 *
 * Some compilers (MSVC for example) don't initialize static class members
 * if they are never used. As a consequence that class is never registered
 * with FEMObjectFactory, since OFID was never initialized.
 *
 * Defining a static const reference to OFID member of a class prevents
 * that from happening.
 *
 * \note Call this macro in .h file after class declaration and
 *       within itk::fem namespace.
 */
#define FEM_CLASS_INIT(thisClass) \
  /** Globaly accesible const reference to class ID */ \
  static const int& OFID_##thisClass = thisClass::OFID;



#endif // #ifndef __itkFEMMacro_h
