/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectFactoryBase.h
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
#ifndef __itkObjectFactoryBase_h
#define __itkObjectFactoryBase_h

#include "itkObject.h"
#include "itkCreateObjectFunction.h"
#include <list>
#include <vector>

namespace itk
{
/** \class ObjectFactoryBase
 * \brief Create instances of classes using an object factory.
 *
 * ObjectFactoryBase is used to create itk objects. The base class
 * ObjectFactoryBase contains a static method CreateInstance() that is
 * used to create itk objects from the list of registerd ObjectFactoryBase
 * sub-classes.  The first time CreateInstance() is called, all dll's or
 * shared libraries in the environment variable ITK_AUTOLOAD_PATH are loaded
 * into the current process.  The C function itkLoad is called on each dll.
 * itkLoad should return an instance of the factory sub-class implemented in
 * the shared library. ITK_AUTOLOAD_PATH is an environment variable
 * containing a colon separated (semi-colon on win32) list of paths.
 *
 * This can be use to overide the creation of any object in ITK.  
 */

class OverRideMap;

class ITK_EXPORT ObjectFactoryBase : public Object
{
public:  
  /**
   * Standard "Self" typedef.
   */
  typedef ObjectFactoryBase   Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Class Methods used to interface with the registered factories
   */
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ObjectFactoryBase, Object);

  /**
   * Create and return an instance of the named itk object.
   * Each loaded ObjectFactoryBase will be asked in the order
   * the factory was in the ITK_AUTOLOAD_PATH.  After the
   * first factory returns the object no other factories are asked.
   */
  static LightObject::Pointer CreateInstance(const char* itkclassname);

  /**
   * Create and return all possible instances of the named itk object.
   * Each loaded ObjectFactoryBase will be asked in the order
   * the factory was in the ITK_AUTOLOAD_PATH.  All created objects
   * will be returned in the list.
   */
  static std::list<LightObject::Pointer>
  CreateAllInstance(const char* itkclassname);

  /**
   * Re-check the ITK_AUTOLOAD_PATH for new factory libraries.
   * This calls UnRegisterAll before re-loading.
   */
  static void ReHash(); 

  /**
   * Register a factory so it can be used to create itk objects.
   */
  static void RegisterFactory(ObjectFactoryBase* );

  /**
   * Remove a factory from the list of registered factories.
   */
  static void UnRegisterFactory(ObjectFactoryBase*);

  /**
   * Unregister all factories.
   */
  static void UnRegisterAllFactories();
  
  /**
   * Return the list of all registered factories.  This is NOT a copy,
   * do not remove items from this list!
   */
  static std::list<ObjectFactoryBase*> GetRegisteredFactories();

  /**
   * All sub-classes of ObjectFactoryBase should must return the version of 
   * ITK they were built with.  This should be implemented with the macro
   * ITK_SOURCE_VERSION and NOT a call to Version::GetITKSourceVersion.
   * As the version needs to be compiled into the file as a string constant.
   * This is critical to determine possible incompatible dynamic factory loads.
   */
  virtual const char* GetITKSourceVersion() = 0;

  /**
   * Return a descriptive string describing the factory.
   */
  virtual const char* GetDescription() const = 0;

  /**
   * Return a list of classes that this factory overrides.
   */
  virtual std::list<std::string> GetClassOverrideNames();

  /**
   * Return a list of the names of classes that override classes.
   */
  virtual std::list<std::string> GetClassOverrideWithNames();

  /**
   * Return a list of descriptions for class overrides.
   */
  virtual std::list<std::string> GetClassOverrideDescriptions();

  /**
   * Return a list of enable flags.
   */
  virtual std::list<bool> GetEnableFlags();

  /**
   * Set the Enable flag for the specific override of className.
   */
  virtual void SetEnableFlag(bool flag,
			     const char* className,
			     const char* subclassName);
  /**
   * Get the Enable flag for the specific override of className.
   */
  virtual bool GetEnableFlag(const char* className,
                             const char* subclassName);

  /**
   * Set all enable flags for the given class to 0.  This will
   * mean that the factory will stop producing class with the given
   * name.
   */
  virtual void Disable(const char* className);
  
  /**
   * This returns the path to a dynamically loaded factory.
   */
  const char* GetLibraryPath();

  /** \class OverrideInformation
   * \brief Internal implementation class for ObjectFactorBase.
   */
  struct OverrideInformation
  {
    std::string m_Description;
    std::string m_OverrideWithName;
    bool m_EnabledFlag;
    CreateObjectFunctionBase::Pointer m_CreateObject;
  };

protected:
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Register object creation information with the factory.
   */
  void RegisterOverride(const char* classOverride,
			const char* overrideClassName,
			const char* description,
			bool enableFlag,
			CreateObjectFunctionBase* createFunction);
		
  /**
   * This method is provioded by sub-classes of ObjectFactoryBase.
   * It should create the named itk object or return 0 if that object
   * is not supported by the factory implementation.
   */
  virtual LightObject::Pointer CreateObject(const char* itkclassname );
  
  ObjectFactoryBase();
  ~ObjectFactoryBase();
  ObjectFactoryBase(const ObjectFactoryBase&) {};
  void operator=(const ObjectFactoryBase&) {};

  OverRideMap* m_OverrideMap;

private:
  /**
   * Initialize the static members of ObjectFactoryBase.   RegisterDefaults
   * is called here.
   */
  static void Initialize();

  /**
   * Register default factories which are not loaded at run time.
   */
  static void RegisterDefaults();

  /**
   * Load dynamic factories from the ITK_AUTOLOAD_PATH
   */
  static void LoadDynamicFactories();

  /**
   * Load all dynamic libraries in the given path
   */
  static void LoadLibrariesInPath( const char*);
  
  /** 
   * list of registered factories
   */
  static std::list<ObjectFactoryBase*>* m_RegisteredFactories; 
  
  /** 
   * member variables for a factory set by the base class
   * at load or register time
   */
  void* m_LibraryHandle;
  unsigned long m_LibraryDate;
  std::string m_LibraryPath;
};

} // end namespace itk

#endif
