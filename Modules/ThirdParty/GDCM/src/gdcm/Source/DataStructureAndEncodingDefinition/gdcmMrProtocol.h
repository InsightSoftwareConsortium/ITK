/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMMRPROTOCOL_H
#define GDCMMRPROTOCOL_H

#include "gdcmTypes.h"
#include "gdcmDataSet.h"

namespace gdcm
{
class ByteValue;
/*
 * Everything done in this code is for the sole purpose of writing interoperable
 * software under Sect. 1201 (f) Reverse Engineering exception of the DMCA.
 * If you believe anything in this code violates any law or any of your rights,
 * please contact us (gdcm-developers@lists.sourceforge.net) so that we can
 * find a solution.
 */
//-----------------------------------------------------------------------------

class DataElement;
/**
 * \brief Class for MrProtocol
 */
class GDCM_EXPORT MrProtocol
{
  friend std::ostream& operator<<(std::ostream &_os, const MrProtocol &d);
public :
  MrProtocol();
  ~MrProtocol();

  bool Load( const ByteValue * bv, const char * str, int version );
  void Print(std::ostream &os) const;

  int GetVersion() const;

  const char * GetMrProtocolByName(const char *name) const;

  bool FindMrProtocolByName(const char *name) const;

  struct Vector3
  {
    double dSag;
    double dCor;
    double dTra;
  };
  struct Slice
  {
    Vector3 Normal;
    Vector3 Position;
  };
  struct SliceArray
  {
    std::vector< Slice > Slices;
  };
  bool GetSliceArray( MrProtocol::SliceArray & sa ) const;

private:
  struct Element;
  struct Internals;
  Internals *Pimpl;
};
//-----------------------------------------------------------------------------
inline std::ostream& operator<<(std::ostream &os, const MrProtocol &d)
{
  d.Print( os );
  return os;
}

} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif //GDCMMRPROTOCOL_H
