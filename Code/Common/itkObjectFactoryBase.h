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
// .NAME itkObjectFactoryBase - abstract base class for itkObjectFactories
// .SECTION Description
// itkObjectFactoryBase is used to create itk objects.   The base class
// itkObjectFactoryBase contains a static method CreateInstance which is used
// to create itk objects from the list of registerd itkObjectFactoryBase 
// sub-classes.   The first time CreateInstance is called, all dll's or shared
// libraries in the environment variable ITK_AUTOLOAD_PATH are loaded into
// the current process.   The C function itkLoad is called on each dll.  
// itkLoad should return an instance of the factory sub-class implemented
// in the shared library. ITK_AUTOLOAD_PATH is an environment variable 
// containing a colon separated (semi-colon on win32) list of paths.
//
// This can be use to overide the creation of any object in ITK.  
//



#ifndef __itkObjectFactoryBase_h
#define __itkObjectFactoryBase_h


#include "itkObject.h"
#include "itkCreateObjectFunction.h"
#include <list>
#include <vector>
#include <map>

class ITK_EXPORT itkObjectFactoryBase : public itkObject
{
public:  
  typedef itkSmartPointer<itkObjectFactoryBase> Pointer;
  // Description:
  // Print ObjectFactor to stream.
  virtual void PrintSelf(std::ostream& os, itkIndent indent);

  // Class Methods used to interface with the registered factories
  
  // Description:
  // Create and return an instance of the named itk object.
  // Each loaded itkObjectFactoryBase will be asked in the order
  // the factory was in the ITK_AUTOLOAD_PATH.  After the
  // first factory returns the object no other factories are asked.
  static itkObject* CreateInstance(const char* itkclassname);
  // Description:
  // Re-check the ITK_AUTOLOAD_PATH for new factory libraries.
  // This calls UnRegisterAll before re-loading
  static void ReHash(); 
  // Description:
  // Register a factory so it can be used to create itk objects
  static void RegisterFactory(itkObjectFactoryBase* );
  // Description:
  // Remove a factory from the list of registered factories
  static void UnRegisterFactory(itkObjectFactoryBase*);
  // Description:
  // Unregister all factories
  static void UnRegisterAllFactories();
  
  // Description:
  // Return the list of all registered factories.  This is NOT a copy,
  // do not remove items from this list!
  static std::list<itkObjectFactoryBase*> GetRegisteredFactories();

  // Description:
  // All sub-classes of itkObjectFactoryBase should must return the version of 
  // ITK they were built with.  This should be implemented with the macro
  // ITK_SOURCE_VERSION and NOT a call to itkVersion::GetITKSourceVersion.
  // As the version needs to be compiled into the file as a string constant.
  // This is critical to determine possible incompatible dynamic factory loads.
  virtual const char* GetITKSourceVersion() = 0;

  // Description:
  // Return a descriptive string describing the factory.
  virtual const char* GetDescription() = 0;

  // Return a list of classes that this factory overrides.
  virtual std::list<std::string> GetClassOverrideNames();
  // Return a list of the names of classes that override classes.
  virtual std::list<std::string> GetClassOverrideWithNames();
  // Retrun a list of descriptions for class overrides
  virtual std::list<std::string> GetClassOverrideDescriptions();
  // Return a list of enable flags
  virtual std::list<bool> GetEnableFlags();

  // Description:
  // Set and Get the Enable flag for the specific override of className
  virtual void SetEnableFlag(bool flag,
			     const char* className,
			     const char* subclassName);
  virtual bool GetEnableFlag(const char* className,
			    const char* subclassName);

  // Description:
  // Set all enable flags for the given class to 0.  This will
  // mean that the factory will stop producing class with the given
  // name.
  virtual void Disable(const char* className);
  
  // Description:
  // This returns the path to a dynamically loaded factory.
  const char* GetLibraryPath();

protected:
  // Description:
  // Register object creation information with the factory.
  void RegisterOverride(const char* classOverride,
			const char* overrideClassName,
			const char* description,
			bool enableFlag,
			itkCreateObjectFunctionBase* createFunction);
		
  // Description:
  // This method is provioded by sub-classes of itkObjectFactoryBase.
  // It should create the named itk object or return 0 if that object
  // is not supported by the factory implementation.
  virtual itkObject* CreateObject(const char* itkclassname );
  
  itkObjectFactoryBase();
  ~itkObjectFactoryBase();
  itkObjectFactoryBase(const itkObjectFactoryBase&) {};
  void operator=(const itkObjectFactoryBase&) {};
  struct OverrideInformation
  {
    std::string m_Description;
    std::string m_OverrideWithName;
    bool m_EnabledFlag;
    itkCreateObjectFunctionBase::Pointer m_CreateObject;
    
  };
  typedef  std::multimap<std::string, OverrideInformation> OverRideMap;
  OverRideMap m_OverrideMap;

private:
  // Description:
  // Initialize the static members of itkObjectFactoryBase.   RegisterDefaults
  // is called here.
  static void Init();
  // Description:
  // Register default factories which are not loaded at run time.
  static void RegisterDefaults();
  // Description:
  // Load dynamic factories from the ITK_AUTOLOAD_PATH
  static void LoadDynamicFactories();
  // Description:
  // Load all dynamic libraries in the given path
  static void LoadLibrariesInPath( const char*);
  
  // list of registered factories
  static std::list<itkObjectFactoryBase*>* m_RegisteredFactories; 
  
  // member variables for a factory set by the base class
  // at load or register time
  void* m_LibraryHandle;
  unsigned long m_LibraryDate;
  std::string m_LibraryPath;
};


#endif
