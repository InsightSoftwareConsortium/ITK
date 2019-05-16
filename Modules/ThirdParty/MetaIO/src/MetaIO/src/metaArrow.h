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

#ifndef ITKMetaIO_METAARROW_H
#define ITKMetaIO_METAARROW_H

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

    ~MetaArrow(void) override;

    void PrintInfo(void) const override;

    void CopyInfo(const MetaObject * _object) override;

    void  Clear(void) override;

    void  Length(float length);
    float Length(void) const;

    void  Lenght(float length) {this->Length(length);}
    float Lenght(void) const {return Length();}

    void  Direction(const double *direction);
    const double * Direction(void) const;


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

    float M_Length; // default 1.0

    double M_Direction[10];

};

#if (METAIO_USE_NAMESPACE)
};
#endif

#endif
