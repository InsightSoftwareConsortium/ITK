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
 * This must be included before itk includes windows.h, otherwise nobody
 * can ever use MFC again...
 */
#ifdef _FEM_Build_Visualization_Routines_
#include <afxwin.h>        /** required to draw the element on device context */
#endif

#include "itkFEMObjectFactory.h"




/**
 * \file itkFEMMacro.h
 * \brief Definitions of macros used in FEM code.
 *
 * itkFEMMacro.h defines macros that allow for simple and consistent FEM code
 * creation. Use these macros whenever posible (always)!
 */


/**
 * Do we want itk's SmartPointer support???
 */
//#define FEM_USE_SMART_POINTERS

#ifdef FEM_USE_SMART_POINTERS
#include "itkMacro.h"
#include "itkSmartPointer.h"
#include "itkObjectFactory.h"
#include "itkLightObject.h"
#endif




/**
 * Include typedefs for SmartPointer support.
 * ONLY USE THIS MACRO FOR ABSTRACT CLASSES THAT CAN'T BE INSTANTIATED OR
 * NON-POLYMORPHIC CLASSES.
 * Otherwise use FEM_CLASS macro.
 */
#ifndef FEM_USE_SMART_POINTERS

#define FEM_CLASS_SP(thisClass,parentClass)  \
public:                                      \
  /** Standard "Self" typedef.*/             \
  typedef thisClass Self;                    \
  /** Standard "Superclass" typedef. */      \
  typedef parentClass Superclass;            \
  /**  Dumb pointer typedef support. */      \
  typedef Self* Pointer;                     \
  typedef const Self* ConstPointer;          \
private:  // everything that follows from here is private by default (like in the beginning of class)

#else

#define FEM_CLASS_SP(thisClass,parentClass)  \
public:                                      \
  /** Standard "Self" typedef.*/             \
  typedef thisClass Self;                    \
  /** Standard "Superclass" typedef. */      \
  typedef parentClass Superclass;            \
  /**  Smart pointer typedef support. */     \
  typedef SmartPointer<Self> Pointer;        \
  typedef SmartPointer<const Self> ConstPointer;  \
  itkTypeMacro(thisClass,"")                 \
private:  // everything that follows from here is private by default (like in the beginning of class)

#endif




/**
 * Automatically include all standard typedefs and members when declaring new
 * class. This also ensures compatibility with itk standards. Must provide
 * a string containing a class name (thisClass). Baseclass typedef must also
 * already be defined in class. Call this macro immediately after { when 
 * declaring a class.
 */
#ifndef FEM_USE_SMART_POINTERS
  #define FEM_CLASS(thisClass,parentClass)   \
    /**  Pointers.... */                     \
    FEM_CLASS_SP(thisClass,parentClass)      \
  public:                                    \
    /**                                      \
     * Create a new object from the existing one  \
     */                                      \
     virtual Baseclass::Pointer Clone() const \
      { return new Self(*this); }            \
    /**                                      \
     * Class ID for FEM object factory       \
     */                                      \
    static const int OFID;                   \
  private:  // everything that follows from here is private by default (like in the beginning of class)
#else
  #define FEM_CLASS(thisClass,parentClass)   \
    /**  Pointers.... */                     \
    FEM_CLASS_SP(thisClass,parentClass)      \
  public:                                    \
    /**                                      \
     * Create a new object from the existing one  \
     */                                      \
     virtual Baseclass::Pointer Clone() const \
      {  Self::Pointer o=new Self(*this);    \
        o->SetReferenceCount(1);             \
        return o; }                          \
    /**                                      \
     * Class ID for FEM object factory       \
     */                                      \
    static const int OFID;                   \
    /** Object creation through itk's objectfactory  */ \
    itkNewMacro(Self)                        \
  private:  // everything that follows from here is private by default (like in the beginning of class)
#endif



/**
 * Register the specified class with FEMObjectFactory. The class must contain
 * static member OFID and must define (or inherit) Baseclass typedef.
 * Call this macro after the class definition is complete in .cxx file but still
 * within itk::fem namespace.
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
 * Performs any initialization tasks for a class. Currently does the following:
 * 
 *  - Creates a constant reference to class ID that is globally accesable.
 *    This in turn also insures that the class is registered with FEM object factory
 *
 * Call this macro in .h file after class declaration and within itk::fem namespace.
 */
#define FEM_CLASS_INIT(thisClass) \
static const int& OFID_##thisClass = thisClass::OFID;



#endif // #ifndef __itkFEMMacro_h
