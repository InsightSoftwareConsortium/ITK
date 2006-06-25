#include "metaTypes.h"

#define NAMESPACE_METASCENE_H META_MERGE_TOKENS($METAIO_NAMESPACE, \
                                                 METASCENE_H)
#ifndef $NAMESPACE_METASCENE_H
#define $NAMESPACE_METASCENE_H

#include "metaUtils.h"
#include "metaObject.h"

#include <list>


/*!    MetaScene (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaTubeFiles.
 *
 * \author Julien Jomier
 * 
 * \date July, 2002
 *
 */

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif

class METAIO_EXPORT MetaScene : public MetaObject
  {

  /////
  //
  // PUBLIC
  //
  ////
  public:

   typedef METAIO_STL::list<MetaObject*>    ObjectListType;
   
   ////
    //
    // Constructors & Destructor
    //
    ////
    MetaScene(void);

    MetaScene(const MetaScene *_scene);
    
    MetaScene(unsigned int dim);

    ~MetaScene(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaScene * _scene);

    void AddObject(MetaObject* object);

    //
    //
    //
    // This function only reads registered tubes
    bool Read(const char *_headerName=NULL);

    bool Write(const char *_headName=NULL);

    bool Append(const char* =NULL) {METAIO_STREAM::cout << "Not Implemented !" << METAIO_STREAM::endl;return true;}

    void  Clear(void);

    
    //    NObjects(...)
    //       Required Field
    //       Number of points wich compose the tube
    void  NObjects(int nobjects);
    int   NObjects(void) const;


    ObjectListType * GetObjectList(void) {return & m_ObjectList;}

  ////
  //
  // PROTECTED
  //
  ////
  protected:

    bool  m_ElementByteOrderMSB;

    void  M_Destroy(void);

    void  M_SetupReadFields(void);

    void  M_SetupWriteFields(void);

    bool  M_Read(void);

    bool  M_Write(void);

    int m_NObjects;      // "NObjects = "         0

    ObjectListType    m_ObjectList;
    
  };

#if (METAIO_USE_NAMESPACE)
};
#endif

#endif
