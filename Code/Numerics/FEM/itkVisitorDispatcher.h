#ifndef __VisitorDispatcher_h
#define __VisitorDispatcher_h

#include <typeinfo.h>
#include <map>




/**
 * \class VisitorDispatcher
 * \brief This class provides the functionality needed to apply the correct
 *        visitor function to object of some class. The specific visitor
 *        function choosen, based on a given pointer to some object.
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
 *          return VisitorDispatcher<MyVisitableClass,VisitorBase,ReturnType>::Visit(this,l);
 *        }
 *        ...
 *      };
 *
 *    Since this code is the same for all derived element classes, you should
 *    probably put it in the macro. For basic visitors you can use the
 *    macro provided with the definition of the VisitorDispatcher class.
 *
 * 3. Register each visitor class with the VisitorDispatcher class before it
 *    is called. This is done by calling the member function RegisterVisitor
 *    of the VisitorDispatcher class and providing the pointer to the Visitor
 *    function that performs the required task. The visitor function must be 
 *    declared like this:
 *
 *      ReturnType MyVisitor_Function(MyVisitableClass*, VisitorBase*);
 *
 *
 * Once all this is done, you can perform various operations on objects of
 * all derived classes by simply calling the Visit function on the pointer
 * to base class and providing a pointer to the specific Visitor object:
 *
 *    object->Visit(visitor);
 * 
 *
 * The Visitor class is templated over several classes that make its use
 * generic and simple.
 *
 * \param TVisitedClass Class of objects that will be visited.
 *
 * \param TVisitorBase  Base class of Visitor objects. Objects of class
 *                      TVisitedClass will be visited by object of any
 *                      registered class that is derived from TVisitorBase.
 *
 * \param TReturnType   Return type of all visitor functions.
 *
 */
template<class TVisitedClass, class TVisitorBase, class TReturnType>
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
   * Return type of the visitor functions.
   */
  typedef TReturnType ReturnType;


  /**
   *
   */
  typedef VisitedClass::Pointer VisitedClassPointer;
  typedef VisitorBase::Pointer VisitorBasePointer;



  /**
   * Type that holds pointers to visitor functions
   */
  typedef ReturnType (*VisitFunctionPointerType)(VisitedClassPointer, VisitorBasePointer);

  /**
   * Type that holds class IDs. Pointer to type_info object provides a
   * unique identifier for each class.
   */
  typedef const type_info* ClassIDType;

  /**
   * Type that holds array of pairs or class IDs and pointers to visitor
   * functions.
   *
   * FIXME: Maybe std::map is not the most efficient way of storing these
   *        pointers. Change this to some other array if necessary. Maybe try
   *        keeping the class IDs within classes.
   */
  typedef std::map<ClassIDType, VisitFunctionPointerType> VisitorsArrayType;

  /**
   * Choose the appropriate function that will be applied to object pointed
   * to by e of class TVisitedClass. The choice of function depends on class
   * of object given in l.
   * 
   * Before this function can be called, the visitor functions must be added
   * to the VisitorDispatcher class for class that is derived from
   * TVisitorBase.
   *
   * \sa RegisterVisitor
   */
  static ReturnType Visit(VisitedClassPointer e, VisitorBasePointer l);

  /**
   * Adds function visitor_function to the VisitorDispatcher class and
   * associates this function with the class TVisitorClass. This means
   * that when member function Visit(e,l) is called, the fucntion
   * visitor_function will be called, if object pointed to by l
   * is of class TVisitorClass.
   *
   * You would call this function immediatly after declaring a class.
   *
   *   int Dummy = VisitorDispatcher<Bar,Load>::RegisterVisitor((LoadGrav*)0, &LoadGravImpl);
   * 
   * \param TVisitorClass* Dummy class pointer that automatically generates
   *                       the correct template parameter TVisitorClass.
   *                       Technically we would like to call Add function as
   *                       VisitorDispatcher<...>::Add<MyVisitorClass>(...),
   *                       but MS C compiler crashes if we do this. This is
   *                       a work around.
   *
   * \param visitor_function Pointer to a visitor function.
   *
   * \sa Visit
   */
  template<class TVisitorClass>
  inline static void RegisterVisitor(TVisitorClass*, VisitFunctionPointerType visitor_function)
  {
    typedef TVisitorClass VisitorClass;
    if ( Instance().visitors.insert(VisitorsArrayType::value_type(&typeid(VisitorClass),visitor_function)).second )
    {
      // Visitor class was successfully registered
//      std::cout<<"Visitor "<<typeid(VisitorClass).name()<<" that operates on objects of "<<typeid(VisitedClass).name()<<" was sucessfully registered\n";
//      std::cout<<"Debug info: "<<typeid(TVisitedClass).name()<<", "<<typeid(TVisitorBase).name()<<", "<<typeid(TReturnType).name()<<"\n";
      return;
    }
    // The visitor function was already registered.
    // FIXME: implement the proper error handler if required
    std::cout<<"Warning: Visitor "<<typeid(VisitorClass).name()<<" that operates on objects of "<<typeid(VisitedClass).name()<<" was already registered! Ignoring the re-registration.\n";

  }

private:

  static VisitorDispatcher& Instance();

  static void CleanUP(void) { delete obj; }

  /**
   * Pointer to the only instance of the VisitorDispatcher class.
   * FIXME: This should really be a SmartPointer, but due to performance limitations it is not.
   */
  static VisitorDispatcher* obj;

  VisitorsArrayType visitors;

};




template<class TVisitedClass, class TVisitorBase, class TReturnType>
VisitorDispatcher<TVisitedClass, TVisitorBase, TReturnType>*  VisitorDispatcher<TVisitedClass, TVisitorBase, TReturnType>::obj=0;




template<class TVisitedClass, class TVisitorBase, class TReturnType>
VisitorDispatcher<TVisitedClass, TVisitorBase, TReturnType>&  VisitorDispatcher<TVisitedClass, TVisitorBase, TReturnType>::Instance()
{
  if (!obj) 
  { 
    /**
     * Create a new VisitorDispatcher object if we don't have it already.
     */
    obj=new VisitorDispatcher;

    /**
     * Make sure that the object that we just created is also destroyed
     * when program finishes.
     */
    atexit(&CleanUP);
        
  }

  /**
   * Return the actual FEMObjectFactory object
   */
  return *obj;
}




template<class TVisitedClass, class TVisitorBase, class TReturnType>
VisitorDispatcher<TVisitedClass, TVisitorBase, TReturnType>::ReturnType VisitorDispatcher<TVisitedClass, TVisitorBase, TReturnType>::Visit(VisitedClassPointer e, VisitorBasePointer l)
{
  VisitorsArrayType::const_iterator i = Instance().visitors.find(&typeid(*l));
  if( i==Instance().visitors.end() )
  {
    // Visitor function not found... FIXME: write the proper error handler.
    std::cout<<"Error: Visitor "<<typeid(*l).name()<<" that operates on objects of "<<typeid(VisitedClass).name()<<" not found!\n";
    throw;
  }
  return (i->second)(e,l);
}





/**
 * \def ACCEPT_VISITOR(VisitorBase,ReturnType)
 * \brief Defines a AcceptVisitor member function.
 *
 * This function is needed for the class to be able to accept visitor.
 *
 * \note Use this macro in derived classes only! For base class use
 *       ACCEPT_VISITOR_BASE(VisitorBase,ReturnType)
 *
 * \param VisitorBase Base class of a acceptable visitor objects.
 * \param ReturnType Return type of the implementation of the visitor
 *        function.
 * \param FunctionName Name of the member function that will be declared.
 *        This allows you to change the name of the AcceptVisitor function
 *        above into anything meaningful.
 *
 * \sa ACCEPT_VISITOR_BASE(VisitorBase,ReturnType)
 */
#define ACCEPT_VISITOR(VisitorBase,ReturnType,FunctionName) \
  virtual ReturnType FunctionName( VisitorBase* l ) \
  { return VisitorDispatcher<Self,VisitorBase,ReturnType>::Visit(this,l); }



/**
 * \def ACCEPT_VISITOR_BASE(VisitorBase,ReturnType)
 * \brief Defines a AcceptVisitor member function in base class.
 *
 * This function is needed for the class to be able to accept visitor.
 *
 * \note Use this macro in base class only! For derived classes use
 *       ACCEPT_VISITOR(VisitorBase,ReturnType)
 *
 * \param VisitorBase Base class of a acceptable visitor objects.
 * \param ReturnType Return type of the implementation of the visitor
 *        function.
 * \param FunctionName Name of the member function that will be declared.
 *        This allows you to change the name of the AcceptVisitor function
 *        above into anything meaningful.
 *
 * \sa ACCEPT_VISITOR(VisitorBase,ReturnType)
 */
#define ACCEPT_VISITOR_BASE(VisitorBase,ReturnType,FunctionName) \
virtual ReturnType FunctionName( VisitorBase* l ) {return ReturnType();};




#endif // __VisitorDispatcher_h
