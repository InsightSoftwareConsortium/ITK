/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkObjectFactoryBase_h
#define itkObjectFactoryBase_h

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
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */

// Forward reference because of private implementation
class OverRideMap;
struct ObjectFactoryBasePrivate;

class ITKCommon_EXPORT ObjectFactoryBase:public Object
{
public:

  /** Standard class typedefs. */
  typedef ObjectFactoryBase          Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ObjectFactoryBase, Object);

  /** Create and return an instance of the named itk object.
   * Each loaded ObjectFactoryBase will be asked in the order
   * the factory was in the ITK_AUTOLOAD_PATH.  After the
   * first factory returns the object no other factories are asked. */
  static LightObject::Pointer CreateInstance(const char *itkclassname);

  /** Create and return all possible instances of the named itk object.
   * Each loaded ObjectFactoryBase will be asked in the order
   * the factory was in the ITK_AUTOLOAD_PATH.  All created objects
   * will be returned in the list. */
  static std::list< LightObject::Pointer >
  CreateAllInstance(const char *itkclassname);

  /** Re-check the ITK_AUTOLOAD_PATH for new factory libraries.
   * This calls UnRegisterAll before re-loading. */
  static void ReHash();

  /** Register a factory so it can be used to create itk objects.
   *  This method is intended to be called only for built-in default
   *  factories, not for loadable factories.
   *
   * Factories that are registered with this method will be
   * regisistered after ReHash.
   */
  static void RegisterFactoryInternal(ObjectFactoryBase *);

  /** Position at which the new factory will be registered in the
   *  internal factory container.
   */
  typedef enum
    {
    INSERT_AT_FRONT,
    INSERT_AT_BACK,
    INSERT_AT_POSITION
    }  InsertionPositionType;

  /** Register a factory so it can be used to create itk objects.
   *
   * When INSERT_AT_POSITION is selected, a third argument must be provided
   * with the actual integer number of the intended position. The position
   * number must be in the range [0, numberOfRegisteredFactories-1].
   *
   * Usage should be any of the following:
   *
   * itk::ObjectFactoryBase::RegisterFactory( newFactory1 ); // at back
   * itk::ObjectFactoryBase::RegisterFactory( newFactory2, INSERT_AT_FRONT );
   * itk::ObjectFactoryBase::RegisterFactory( newFactory3, INSERT_AT_BACK );
   * itk::ObjectFactoryBase::RegisterFactory( newFactory4, INSERT_AT_POSITION, 5 );
   *
   * If the position value is out of range, an exception will be
   * thrown.
   * Returns true if the factory was successfully registered.
   * Returns false if factory is already loaded.
   */
  static bool RegisterFactory(ObjectFactoryBase *,
    InsertionPositionType where=INSERT_AT_BACK, size_t position = 0);

  /** Remove a factory from the list of registered factories. */
  static void UnRegisterFactory(ObjectFactoryBase *);

  /** Unregister all factories. */
  static void UnRegisterAllFactories();

  /** Return the list of all registered factories.  This is NOT a copy,
   * do not remove items from this list! */
  static std::list< ObjectFactoryBase * > GetRegisteredFactories();

  /** All sub-classes of ObjectFactoryBase should must return the version of
   * ITK they were built with.  This should be implemented with the macro
   * ITK_SOURCE_VERSION and NOT a call to Version::GetITKSourceVersion.
   * As the version needs to be compiled into the file as a string constant.
   * This is critical to determine possible incompatible dynamic factory loads. */
  virtual const char * GetITKSourceVersion(void) const = 0;

  /** Require the ITK version of this application to exactly match the ITK
   * version used to compile a dynamic library. When this is set to true, if the
   * versions do not match, an exception will be thrown. When this is false, and
   * the versions do not match, only a warning message is printed out in the
   * console, and the factory is still registered. */
  static void SetStrictVersionChecking( bool );
  static void StrictVersionCheckingOn();
  static void StrictVersionCheckingOff();
  static bool GetStrictVersionChecking();

  /** Return a descriptive string describing the factory. */
  virtual const char * GetDescription(void) const = 0;

  /** Return a list of classes that this factory overrides. */
  virtual std::list< std::string > GetClassOverrideNames();

  /** Return a list of the names of classes that override classes. */
  virtual std::list< std::string > GetClassOverrideWithNames();

  /** Return a list of descriptions for class overrides. */
  virtual std::list< std::string > GetClassOverrideDescriptions();

  /** Return a list of enable flags. */
  virtual std::list< bool > GetEnableFlags();

  /** Set the Enable flag for the specific override of className. */
  virtual void SetEnableFlag(bool flag,
                             const char *className,
                             const char *subclassName);

  /** Get the Enable flag for the specific override of className. */
  virtual bool GetEnableFlag(const char *className,
                             const char *subclassName);

  /** Set all enable flags for the given class to 0.  This will
   * mean that the factory will stop producing class with the given
   * name. */
  virtual void Disable(const char *className);

  /** This returns the path to a dynamically loaded factory. */
  const char * GetLibraryPath();

  /** \class OverrideInformation
   * \brief Internal implementation class for ObjectFactorBase.
   * \ingroup ITKCommon
   */
  struct OverrideInformation {
    std::string m_Description;
    std::string m_OverrideWithName;
    bool m_EnabledFlag;
    CreateObjectFunctionBase::Pointer m_CreateObject;
  };

  /** Set/Get the pointer to ObjectFactoryBasePrivate.
   * Note that these functions are not part of the public API and should not be
   * used outside of ITK. They are an implementation detail and will be
   * removed in the future. Also note that SetObjectFactoryBasePrivate is not
   * concurrent thread safe. */
  static ObjectFactoryBasePrivate *GetObjectFactoryBase();
  static void SynchronizeObjectFactoryBase(ObjectFactoryBasePrivate * objectFactoryBasePrivate);

protected:
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Register object creation information with the factory. */
  void RegisterOverride(const char *classOverride,
                        const char *overrideClassName,
                        const char *description,
                        bool enableFlag,
                        CreateObjectFunctionBase *createFunction);

  /** This method is provided by sub-classes of ObjectFactoryBase.
   * It should create the named itk object or return 0 if that object
   * is not supported by the factory implementation. */
  virtual LightObject::Pointer CreateObject(const char *itkclassname);

  /** This method creates all the objects with the class overide of
   * itkclass name, which are provide by this object
   */
  virtual std::list< LightObject::Pointer >
  CreateAllObject(const char *itkclassname);

  ObjectFactoryBase();
  virtual ~ObjectFactoryBase() ITK_OVERRIDE;

private:
  OverRideMap *m_OverrideMap;

  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectFactoryBase);

  /** Initialize the static list of Factories. */
  static void InitializeFactoryList();

  /** Initialize the static members of ObjectFactoryBase.
   *  RegisterInternal() and InitializeFactoryList() are called here. */
  static void Initialize();

  /** Register default factories which are not loaded at run time. */
  static void RegisterInternal();

  /** Load dynamic factories from the ITK_AUTOLOAD_PATH */
  static void LoadDynamicFactories();

  /** Load all dynamic libraries in the given path */
  static void LoadLibrariesInPath(const char *);

  static void DeleteNonInternalFactory(  ObjectFactoryBase * );

  /** Member variables for a factory set by the base class
   * at load or register time */
  void *        m_LibraryHandle;
  unsigned long m_LibraryDate;
  std::string   m_LibraryPath;

  static  bool  m_StrictVersionChecking;

  // This variable should NOT be accessed directly, but through GetObjectFactoryBase
  static ObjectFactoryBasePrivate * m_ObjectFactoryBasePrivate;
};
} // end namespace itk

#endif
