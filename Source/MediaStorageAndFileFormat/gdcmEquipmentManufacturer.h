/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMEQUIPMENTMANUFACTURER_H
#define GDCMEQUIPMENTMANUFACTURER_H

#include "gdcmTypes.h"

namespace gdcm
{

class DataSet;
/**
 * \brief 
 * \details  
 *
 */
class GDCM_EXPORT EquipmentManufacturer
{
public:

  typedef enum {
    UNKNOWN = 0,
    FUJI,
    GEMS,
    HITACHI,
    KODAK,
    MARCONI,
    PMS,
    SIEMENS,
    TOSHIBA
  } Type;

  static Type Compute( DataSet const & ds );

private:
};

} // end namespace gdcm

#endif // GDCMEQUIPMENTMANUFACTURER_H
