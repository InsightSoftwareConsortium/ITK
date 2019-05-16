/*============================================================================
  MetaIO
  Copyright 2000-2010 Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#include "metaTypes.h"

#ifndef ITKMetaIO_METAGROUP_H
#define ITKMetaIO_METAGROUP_H

#include "metaUtils.h"
#include "metaObject.h"

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

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif

class METAIO_EXPORT MetaGroup : public MetaObject
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

    ~MetaGroup(void) override;

    void PrintInfo(void) const override;

    void CopyInfo(const MetaObject * _object) override;

    void  Clear(void) override;


  ////
  //
  // PROTECTED
  //
  ////
  protected:

    void  M_Destroy(void) override;

    void  M_SetupReadFields(void) override;

    void  M_SetupWriteFields(void) override;

    bool  M_Read(void) override;

};

#if (METAIO_USE_NAMESPACE)
};
#endif


#endif
