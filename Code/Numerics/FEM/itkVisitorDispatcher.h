/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVisitorDispatcher.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __VisitorDispatcher_h
#define __VisitorDispatcher_h

#include "itkFEMMacro.h"
#include "itkFEMException.h"
#include "itkFastMutexLock.h"
#include <typeinfo>
#include <map>

namespace itk {
namespace fem {

template< class TVisitedClass,
          class TVisitorBase>
class VisitorDispatcherTemplateHelper
{
public:
  typedef void (*FunctionPointerType )(typename TVisitedClass::ConstPointer, typename TVisitorBase::Pointer);
};
  



/**
 * \class VisitorDispatcher
 * \brief This class provides the functionality needed to apply the correct
 *        visitor function to object of some class. The specific visitor
 *        function is choosen, based on a given pointer to some object.
 *
 * A visitor function is a function, that can perform various operations on
 * objects of various classes. Usually we want this operation applied on
 * any of the polymorphic derived classes. The specific operation is
 * defined in visitor functions.
 *
 * For example: calculating the area of Shape objects. In this case the
 * function that calculates the area of a specific shape, is called the
 * visitor function. A specific version of this function must be defined
 * for each class, on which you want the perform the operation (Area(Circle*);
 * Area(Square*); ...).
 *
 * Now suppose that you want different operations performed on the shape
 * objects. Which operation will be performed is specified by the class of
 * the Visitor object. If the Visitor object is of class Area, then the
 * area of objects will be calculated. If the visitor is object of class
 * Circumference, then the circumference of the shapes will be calculated...
 *
 * In order to be able to do that and provide the framework to easily add
 * new Visitor as well as Visited classes, we create the VisitorDispatcher
 * class. It is implemented as a singelton. It stores pointers to Visitor
 * functions together with the information about which Visitor function must
 * be called in order to perform an operation specified by Visitor class on
 * objects of Visited class.
 *
 * To make a specific base class and all its derived classes visitable, you
 * must make the following changes to your code:
 * 
 * 1. Declare the folowing virtual member function in the base class:
 *
 *      class BaseVisitable
 *      {
 *        ...
 *        virtual ReturnType AcceptVisitor( VisitorBase* ) = 0;
 *        ...
 *      };
 * 
 * 2. Implement this function in ALL derived classes like this:
 *
 *      class MyVisitableClass : public BaseVisitable
 *      {
 *        ...
 *        virtual ReturnType AcceptVisitor( VisitorBase* l ) 
 *        { 
 *          return VisitorDispatcher<MyVisitableClass,VisitorBase,VisitFunctionPointerType>::Visit( <parameters> );
 *        }
 *        ...
 *      };
 *
 *    Since this code is the same for all derived element classes, you should
 *    probably put it in the macro.
 *
 * 3. Register each visitor class with the VisitorDispatcher class before it
 *    is called. This is done by calling the member function RegisterVisitor
 *    of the VisitorDispatcher class and providing the pointer to the Visitor
 *    function that performs the required task. The visitor function must be 
 *    declared according to the VisitFunctionPointerType template parameter.
 *
 *      ReturnType MyVisitor_Function( ... );
 *
 *
 * Once all this is done, you can perform various operations on objects of
 * all derived classes by simply calling the Visit function on the pointer
 * to base class and providing a pointer to the specific Visitor object:
 *
 *    object->AcceptVisitor(visitor);
 * 
 *
 * The Visitor class is templated over several classes that make its use
 * generic and simple.
 *
 *  - TVisitedClass Class of objects that will be visited.
 *
 *  - TVisitorBase  Base class of Visitor objects. Objects of class
 *                  TVisitedClass will be visited by object of any
 *                  registered class that is derived from TVisitorBase.
 *
 *  - TVisitFunctionPointerType Type of visit functions. Visitor dispatcher
 *                  stores an array of pointers to these functions. Default
 *                  function pointer type is provided.
 * 
 * \note Template parameter TVisitFunctionPointerType in general doesn't
 *       have to be a pointer to function. In fact, it can be any type
 *       Object of this type will be returned, when calling the
 *       VisitorDispatcher::Visit function.
 */
template< class TVisitedClass,
          class TVisitorBase,
          class TVisitFunctionPointerType= ITK_TYPENAME VisitorDispatcherTemplateHelper<TVisitedClass, TVisitorBase>::FunctionPointerType >
class VisitorDispatcher
{
public:

  /**
   * TVisitedClass is class to which visitor functions will be applied. 
   */
  typedef TVisitedClass VisitedClass;

  /**
   * TVisitorBase is base class for visitor objects. Any class derived from
   * TVisitorBase could in general be applied to TVisitedClass.
   */
  typedef TVisitorBase VisitorBase;

  /**
   *
   */
  typedef typename VisitedClass::Pointer VisitedClassPointer;
  typedef typename VisitedClass::ConstPointer VisitedClassConstPointer;
  typedef typename VisitorBase::Pointer VisitorBasePointer;



  /**
   * Type that holds pointers to visit functions
   */
  typedef TVisitFunctionPointerType VisitFunctionPointerType;

  /**
   * Type that holds class IDs. Pointer to type_info object provides a
   * unique identifier for each class.
   */
  typedef const std::type_info* ClassIDType;

  /**
   * Type that holds array of pairs of class ID and pointer to visit
   * functions.
   *
   * FIXME: Maybe std::map is not the most efficient way of storing these
   *        pointers. Change this to some other array if necessary. Maybe try
   *        keeping the class IDs within classes.
   */
  typedef std::map<ClassIDType, VisitFunctionPointerType> VisitorsArrayType;

  /**
   * Adds function visitor_function to the VisitorDispatcher class and
   * associates this function with the class TVisitorClass. This means
   * that when member function Visit(e,l) is called, the fucntion
   * visitor_function will be called, if object pointed to by l
   * is of class TVisitorClass.
   *
   * To automatically register visitor on library initialization, you
   * would call this function immediatly after defining an
   * implementation function class.
   *
   *   bool Dummy = VisitorDispatcher<Bar,Load>::RegisterVisitor((LoadGrav*)0, &LoadGravImpl);
   * 
   * \param visitor_function Pointer to a visitor function.
   *
   * \note Dummy class pointer must be passed as a first parameter to
   *       automatically deduct the correct template parameter TVisitorClass.
   *       Technically we would like to call the this function as
   *       VisitorDispatcher<...>::RegisterVisitor<MyVisitorClass>(...),
   *       but MS C compiler crashes if we do this. This is a work around.
   *       You should pass null pointer casted to the TVisitorClass when
   *       calling this function.
   *
   * \sa Visit
   */
  template<class TVisitorClass>
  inline static bool RegisterVisitor(TVisitorClass*, VisitFunctionPointerType visitor_function)
  {
    typedef TVisitorClass VisitorClass;
    bool status;
    Instance().m_MutexLock.Lock();
    status=Instance().visitors.insert(VisitorsArrayType::value_type(&typeid(VisitorClass),visitor_function)).second;
    Instance().m_MutexLock.Unlock();
    if ( status )
    {
      // Visitor class was successfully registered
//      std::cout<<"Visitor "<<typeid(VisitorClass).name()<<" ("<<typeid(VisitedClass).name()<<") registered.\n";
//      std::cout<<"Visitor registered:\n  Visitee:"<<typeid(TVisitedClass).name()<<"\n  Visitor:"<<typeid(TVisitorClass).name()<<"\n  Func   :"<<typeid(VisitFunctionPointerType).name()<<"\n\n";
    }
    else
    {
      // The visitor function was already registered.
      // FIXME: implement the proper error handler if required
      std::cout<<"Warning: Visitor "<<typeid(VisitorClass).name()<<" that operates on objects of "<<typeid(VisitedClass).name()<<" was already registered! Ignoring the re-registration.\n";
    }
    return status;
  }

  /**
   * Returns the pointer to the correct implementation of the visit function.
   * based on the class of object passed in l.
   * 
   * Before this function can be called, the visitor functions must be added
   * to the VisitorDispatcher class for visitor class that is derived from
   * TVisitorBase.
   *
   * \param l Pointer to an object. Class of this object determines which
   *          pointer to visit function will be returned.
   *
   * \sa RegisterVisitor
   */
  static VisitFunctionPointerType Visit(VisitorBasePointer l);

private:

  static VisitorDispatcher& Instance();

  static void CleanUP(void) { delete obj; }

  /**
   * Pointer to the only instance of the VisitorDispatcher class.
   */
  static VisitorDispatcher* obj;

  VisitorsArrayType visitors;

  /**
   * Mutex lock to protect modification to the visitors during
   * class registration.
   */
  mutable SimpleFastMutexLock m_MutexLock;

};




template<class TVisitedClass, class TVisitorBase, class TVisitFunctionPointerType>
VisitorDispatcher<TVisitedClass, TVisitorBase, TVisitFunctionPointerType>*
VisitorDispatcher<TVisitedClass, TVisitorBase, TVisitFunctionPointerType>
::obj = 0;




template<class TVisitedClass, class TVisitorBase, class TVisitFunctionPointerType>
VisitorDispatcher<TVisitedClass, TVisitorBase, TVisitFunctionPointerType>&
VisitorDispatcher<TVisitedClass, TVisitorBase, TVisitFunctionPointerType>
::Instance()
{
  // Implementation of the singleton design pattern
  if (!obj) 
  { 
    // Create a new VisitorDispatcher object if we don't have it already.
    obj=new VisitorDispatcher;

    // Make sure that the object that we just created is also destroyed
    // when program finishes.
    atexit(&CleanUP);
  }

  // Return the actual VisitorDispatcher object
  return *obj;
}




template<class TVisitedClass, class TVisitorBase, class TVisitFunctionPointerType>
typename VisitorDispatcher<TVisitedClass, TVisitorBase, TVisitFunctionPointerType>::VisitFunctionPointerType
VisitorDispatcher<TVisitedClass, TVisitorBase, TVisitFunctionPointerType>
::Visit(VisitorBasePointer l)
{
  typename VisitorsArrayType::const_iterator i = Instance().visitors.find(&typeid(*l));
  if( i==Instance().visitors.end() )
  {
    // Visitor function not found... FIXME: write the proper error handler.
    std::cout<<"Error: Visitor "<<typeid(*l).name()<<" that operates on objects of "<<typeid(VisitedClass).name()<<" not found!\n";
    throw FEMException(__FILE__, __LINE__, "FEM error");
  }
  return i->second;
}




}} // end namespace itk::fem

#endif // __VisitorDispatcher_h
