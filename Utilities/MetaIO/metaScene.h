#ifndef METASCENE_H
#define METASCENE_H

#include <metaTypes.h>
#include <metaUtils.h>
#include <metaObject.h>

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


class MetaScene : public MetaObject
  {

  /////
  //
  // PUBLIC
  //
  ////
  public:

   typedef std::list<MetaObject*>    ObjectListType;
   
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

    bool Append(const char* =NULL) {std::cout << "Not Implemented !" << std::endl;return true;}

    void  Clear(void);

    
    //    NObjects(...)
    //       Required Field
    //       Number of points wich compose the tube
    void  NObjects(int nobjects);
    int   NObjects(void) const;


    ObjectListType GetObjectList(void) {return m_ObjectList;}

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


#endif
