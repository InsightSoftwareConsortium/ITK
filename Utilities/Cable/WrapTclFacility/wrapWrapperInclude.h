/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperInclude.h
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
// Note: This header is inteded for multiple inclusion!  The include
// blockers are missing on purpose.

namespace _wrap_
{

/**
 * A specialization of the Wrapper class for actaully wrapping a
 * particular type.  This file, "wrapWrapperInclude.h" should be
 * included by a C++ source implementing a Wrapper.  The following
 * macros must be defined before including this source:
 *
 * _wrap_WRAPPED_TYPE
 *   should be the type to be wrapped.
 *
 * _wrap_WRAPPED_TYPE_NAME
 *   should be a string holding the name of the type to be wrapped.
 *
 * _wrap_METHOD_WRAPPER_PROTOTYPES
 *   should list the method wrapper prototypes.  The last one should not
 *   end in a semicolon.
 *
 * These methods must be defined by the rest of the source after
 * including this file.  They must be registered in the definition of
 * RegisterMethodWrappers().
 *
 * The prototypes of the method wrappers should look like this:
 *
 *   static void Method_name(const Arguments&) const; ??
 *
 * Where "name" is the name of the method being wrapped.  In the case of
 * overloaded methods, a count of the form "#_" should be added before
 * the method name.  If the method is an operator, it should instead
 * begin with "Operator_" instead of "Method_".  Here are some examples:
 *
 * void Method_0_SetV(const Argument&, const Arguments&) const;
 * void Method_1_SetV(const Argument&, const Arguments&) const;
 * void Method_GetVector(const Argument&, const Arguments&) const;
 * void Operator_Plus(const Argument&, const Arguments&) const;
 * void Operator_PlusEquals(const Argument&, const Arguments&) const;
 *
 * This naming scheme should avoid name collisions.
 */
template <>
class Wrapper< _wrap_WRAPPED_TYPE >: public WrapperBase
{
public:
  /**
   * The actual type being wrapped.
   */
  typedef _wrap_WRAPPED_TYPE WrappedType;
  
  // Pull this typedef from our superclass.
  typedef WrapperBase::WrapperFunction WrapperFunction;
  
  // Constructor.  This is implemented below.
  Wrapper(WrapperFacility*);

private:
  // Everything in the class below here must be implemented by the
  // file that includes this one.
  
  // Declare the method wrappers for this Wrapper.
  _wrap_METHOD_WRAPPER_PROTOTYPES;
  
  /**
   * Register the method wrappers for this Wrapper with its WrapperBase.
   */
  void RegisterMethodWrappers();
};


/**
 * Constructor initializes the Wrapper to work with the given
 * WrapperFacility.  It passes down some information to the
 * WrapperBase constructor.
 */
Wrapper< _wrap_WRAPPED_TYPE >:: 
Wrapper(WrapperFacility* wrapperFacility):
  WrapperBase(wrapperFacility, _wrap_WRAPPED_TYPE_NAME)
{
  // Setup our superclass's record of the representation of the
  // wrapped type.
  m_WrappedTypeRepresentation =
    ClassType::SafeDownCast(CvType<WrappedType>::type.GetType());
  
  // Register the Wrapper with the WrapperFacility.
  m_WrapperFacility->SetWrapper(m_WrappedTypeRepresentation, this);
  
  // Register our method wrappers with the superclass.
  this->RegisterMethodWrappers();
  
  // Setup this instance of the Wrapper to work with its interpreter.
  Tcl_CreateObjCommand(m_Interpreter,
                       const_cast<char*>(m_WrappedTypeName.c_str()),
                       this->GetClassWrapperFunction(),
                       static_cast<WrapperBase*>(this), 0);
#ifdef _wrap_ALTERNATE_NAMES
  static char* alternateNames[] = { _wrap_ALTERNATE_NAMES, 0 };
  for(char** alternate = alternateNames; *alternate; ++alternate)
    {
    Tcl_CreateObjCommand(m_Interpreter, *alternate,
                         this->GetClassWrapperFunction(),
                         static_cast<WrapperBase*>(this), 0);
    }
#endif
}


// We only want the Initialization functions once per file that
// includes this header.
#ifndef _wrap_INITIALIZE_FUNCTION
#define _wrap_INITIALIZE_FUNCTION
namespace
{
/**
 * Initializes this file's CvType<> specializations.
 */
void InitializeTypeRepresentations();

/**
 * Initialize the given interpreter to do conversions for this file's
 * CvType<> specializations.
 */
void InitializeConversions(Tcl_Interp* interp);

/**
 * This is called by InitializeGroupForInterpreter() to initialize the
 * non-interpreter-specific parts of the wrapper module.
 */
void InitializeGroup()
{
  // Make sure this is only executed once.
  static bool initialized = false;
  if(initialized) { return; }
  
  // Make sure the wrapper facility has been initialized.
  WrapperFacility::ClassInitialize();
  
  // Initialize this module's type representations.
  InitializeTypeRepresentations();
  
  initialized = true;
}

/**
 * Makes sure that InitializeConversions is called exactly once per
 * interpreter.
 */
void InitializeGroupForInterpreter(Tcl_Interp* interp)
{
  // Make sure the global group stuff has been initialized.
  InitializeGroup();
  
  // Initialize interpreter-specific stuff for this group.
  InitializeConversions(interp);
}

} // anonymous namespace
#endif


} // namespace _wrap_
