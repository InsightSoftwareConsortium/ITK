#include "metaTypes.h"

#ifdef __sgi 
#  pragma once
#else
#  undef  META_ONCE
#  define NAMESPACE_METAARROW_H META_MERGE_TOKENS($METAIO_NAMESPACE, \
                                                 METAARROW_H)
#  if !defined($NAMESPACE_METAARROW_H)
#    define $NAMESPACE_METAARROW_H
#    define META_ONCE
#  endif
#endif

#if defined(__sgi) || defined(META_ONCE)

#include "metaUtils.h"
#include "metaObject.h"

#include <list>


/*!    MetaArrow (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaArrowFiles.
 *
 * \author Julien Jomier
 * 
 * \date Jan 05, 2005
 * 
 * Depends on:
 *    MetaUtils.h
 *    MetaObject.h
 */

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif

class METAIO_EXPORT MetaArrow : public MetaObject
  {

  /////
  //
  // PUBLIC
  //
  ////
  public:

    ////
    //
    // Constructors & Destructor
    //
    ////
    MetaArrow(void);

    MetaArrow(const char *_headerName);   

    MetaArrow(const MetaArrow *_Arrow);    

    MetaArrow(unsigned int dim);

    ~MetaArrow(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaArrow * _Arrow);

    void  Clear(void);
    
    void  Length(float length);
    float Length(void) const;

    void  Lenght(float length) {this->Length(length);}
    float Lenght(void) const {return Length();}


  ////
  //
  // PROTECTED
  //
  ////
  protected:

    void  M_Destroy(void);

    void  M_SetupReadFields(void);

    void  M_SetupWriteFields(void);

    bool  M_Read(void);

    float M_Length; // default 1.0

  };

#if (METAIO_USE_NAMESPACE)
};
#endif

#endif
