#ifndef METAGroup_H
#define METAGroup_H

#include <metaTypes.h>
#include <metaUtils.h>
#include <metaObject.h>

#include <list>


/*!    MetaGroup (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaGroupFiles.
 *
 * \author Julien Jomier
 * 
 * \date May 22, 2002
 * 
 * Depends on:
 *    MetaUtils.h
 *    MetaObject.h
 */


class MetaGroup : public MetaObject
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
    MetaGroup(void);

    MetaGroup(const char *_headerName);   

    MetaGroup(const MetaGroup *_group);    

    MetaGroup(unsigned int dim);

    ~MetaGroup(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaGroup * _group);

    void  Clear(void);
    

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

  };


#endif
