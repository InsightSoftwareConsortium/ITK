

// compile with

// define the group that is being defined in this file.
// a group is a set of classes that are wrapped in the same
// file.   A group is part of a package, and the package
// name is specified with a command line -D option, something
// like this:
//  -DITK_WRAP_PACKAGE="ITKCommonTcl" 
//  -DITK_WRAP_PACKAGE="ITKCommonPython" 

#define ITK_WRAP_GROUP(x) ITK_WRAP_GROUP0(ITK_WRAP_PACKAGE, x)
#define ITK_WRAP_GROUP0(p, x) ITK_WRAP_GROUP1(p, x)
#define ITK_WRAP_GROUP1(p, x) #p "_" #x

#define ITK_WRAP_PACKAGE_NAME(p) ITK_WRAP_PACKAGE_NAME0(ITK_WRAP_PACKAGE)
#define ITK_WRAP_PACKAGE_NAME0(p) ITK_WRAP_PACKAGE_NAME1(p)
#define ITK_WRAP_PACKAGE_NAME1(p) #p 

// Wrap an itk object, the wrap name is itkname, 
// this is for non-templated itk objects, so
// ITK_WRAP_OBJECT(Object) would wrap itk::Object to the wrapped name itkObject
#define ITK_WRAP_OBJECT(name) \
typedef itk::name::name itk##name; \
typedef itk::name::Pointer::SmartPointer itk##name##_Pointer

// define the template class wrapper macros

// Wrap an itk object with one template parameter 
// The wrapname is the name that will be used and usually
// encodes the template parameters, i.e. itk::Image<float, 2> would
// itkImageF2

#define ITK_WRAP_OBJECT1(name, arg1, wrapname) \
typedef itk::name<arg1 >::name wrapname; \
typedef itk::name<arg1 >::Pointer::SmartPointer wrapname##_Pointer 

// same as ITK_WRAP_OBJECT1 but also wraps the super class 
#define ITK_WRAP_OBJECT1_WITH_SUPERCLASS(name, arg1, wrapname) \
ITK_WRAP_OBJECT1(name, arg1, wrapname); \
typedef itk::name<arg1 >::Superclass::Self wrapname##_Superclass

// same as ITK_WRAP_OBJECT1 but for two template parameters
#define ITK_WRAP_OBJECT2(name, arg1, arg2, wrapname) \
typedef itk::name<arg1, arg2 >::name wrapname; \
typedef itk::name<arg1, arg2 >::Pointer::SmartPointer wrapname##_Pointer

#define ITK_WRAP_OBJECT2_WITH_SUPERCLASS(name, arg1, arg2, wrapname) \
ITK_WRAP_OBJECT2(name, arg1, arg2, wrapname); \
typedef itk::name<arg1,arg2 >::Superclass::Self wrapname##_Superclass

// same as ITK_WRAP_OBJECT2 but for three template parameters
#define ITK_WRAP_OBJECT3(name, arg1, arg2, arg3, wrapname) \
typedef itk::name<arg1, arg2, arg3 >::name wrapname; \
typedef itk::name<arg1, arg2, arg3 >::Pointer::SmartPointer wrapname##_Pointer

#define ITK_WRAP_OBJECT3_WITH_SUPERCLASS(name, arg1, arg2, arg3, wrapname) \
ITK_WRAP_OBJECT3(name, arg1, arg2, arg3, wrapname); \
typedef itk::name<arg1,arg2, arg3 >::Superclass::Self wrapname##_Superclass

// same as ITK_WRAP_OBJECT4 but for three template parameters
#define ITK_WRAP_OBJECT4(name, arg1, arg2, arg3, arg4, wrapname) \
typedef itk::name<arg1, arg2, arg3, arg4 >::name wrapname; \
typedef itk::name<arg1, arg2, arg3, arg4 >::Pointer::SmartPointer wrapname##_Pointer

#define ITK_WRAP_OBJECT4_WITH_SUPERCLASS(name, arg1, arg2, arg3, arg4, wrapname) \
ITK_WRAP_OBJECT4(name, arg1, arg2, arg3, arg4, wrapname); \
typedef itk::name<arg1,arg2, arg3, arg4 >::Superclass::Self wrapname##_Superclass

