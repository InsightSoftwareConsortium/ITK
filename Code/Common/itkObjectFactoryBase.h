/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectFactoryBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkObjectFactoryBase is used to create itk objects. The base class
 * itkObjectFactoryBase contains a static method CreateInstance() that is
 * used to create itk objects from the list of registerd itkObjectFactoryBase
 * sub-classes.  The first time CreateInstance() is called, all dll's or
 * shared libraries in the environment variable ITK_AUTOLOAD_PATH are loaded
 * into the current process.  The C function itkLoad is called on each dll.
 * itkLoad should return an instance of the factory sub-class implemented in
 * the shared library. ITK_AUTOLOAD_PATH is an environment variable
 * containing a colon separated (semi-colon on win32) list of paths.
 *
 * This can be use to overide the creation of any object in ITK.  
 */

#ifndef __itkObjectFactoryBase_h
#define __itkObjectFactoryBase_h

#include "itkObject.h"
#include "itkCreateObjectFunction.h"
#include <list>
#include <vector>
class itkOverRideMap;

class ITK_EXPORT itkObjectFactoryBase : public itkObject
{
public:  
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer<itkObjectFactoryBase> Pointer;

  // Class Methods used to interface with the registered factories
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkObjectFactoryBase, itkObject);

  /**
   * Create and return an instance of the named itk object.
   * Each loaded itkObjectFactoryBase will be asked in the order
   * the factory was in the ITK_AUTOLOAD_PATH.  After the
   * first factory returns the object no other factories are asked.
   */
  static itkObject* CreateInstance(const char* itkclassname);

  /**
   * Re-check the ITK_AUTOLOAD_PATH for new factory libraries.
   * This calls UnRegisterAll before re-loading.
   */
  static void ReHash(); 

  /**
   * Register a factory so it can be used to create itk objects.
   */
  static void RegisterFactory(itkObjectFactoryBase* );

  /**
   * Remove a factory from the list of registered factories.
   */
  static void UnRegisterFactory(itkObjectFactoryBase*);

  /**
   * Unregister all factories.
   */
  static void UnRegisterAllFactories();
  
  /**
   * Return the list of all registered factories.  This is NOT a copy,
   * do not remove items from this list!
   */
  static std::list<itkObjectFactoryBase*> GetRegisteredFactories();

  /**
   * All sub-classes of itkObjectFactoryBase should must return the version of 
   * ITK they were built with.  This should be implemented with the macro
   * ITK_SOURCE_VERSION and NOT a call to itkVersion::GetITKSourceVersion.
   * As the version needs to be compiled into the file as a string constant.
   * This is critical to determine possible incompatible dynamic factory loads.
   */
  virtual const char* GetITKSourceVersion() = 0;

  /**
   * Return a descriptive string describing the factory.
   */
  virtual const char* GetDescription() = 0;

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

protected:
  virtual void PrintSelf(std::ostream& os, itkIndent indent);

  /**
   * Register object creation information with the factory.
   */
  void RegisterOverride(const char* classOverride,
			const char* overrideClassName,
			const char* description,
			bool enableFlag,
			itkCreateObjectFunctionBase* createFunction);
		
  /**
   * This method is provioded by sub-classes of itkObjectFactoryBase.
   * It should create the named itk object or return 0 if that object
   * is not supported by the factory implementation.
   */
  virtual itkObject* CreateObject(const char* itkclassname );
  
  itkObjectFactoryBase();
  ~itkObjectFactoryBase();
  itkObjectFactoryBase(const itkObjectFactoryBase&) {};
  void operator=(const itkObjectFactoryBase&) {};
public:
  struct OverrideInformation
  {
    std::string m_Description;
    std::string m_OverrideWithName;
    bool m_EnabledFlag;
    itkCreateObjectFunctionBase::Pointer m_CreateObject;
  };
protected:
  itkOverRideMap* m_OverrideMap;

private:
  /**
   * Initialize the static members of itkObjectFactoryBase.   RegisterDefaults
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
  static std::list<itkObjectFactoryBase*>* m_RegisteredFactories; 
  
  /** 
   * member variables for a factory set by the base class
   * at load or register time
   */
  void* m_LibraryHandle;
  unsigned long m_LibraryDate;
  std::string m_LibraryPath;
};


bool operator==(const itkObjectFactoryBase::OverrideInformation& rhs, 
		const itkObjectFactoryBase::OverrideInformation& lhs)
      {
	return (rhs.m_Description == lhs.m_Description
		&& rhs.m_OverrideWithName == lhs.m_OverrideWithName);
      }


#endif
