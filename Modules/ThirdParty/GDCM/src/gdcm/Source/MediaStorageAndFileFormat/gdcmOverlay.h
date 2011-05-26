/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMOVERLAY_H
#define GDCMOVERLAY_H

#include "gdcmTypes.h"
#include "gdcmObject.h"

namespace gdcm
{

class OverlayInternal;
class ByteValue;
class DataSet;
class DataElement;
/**
 * \brief Overlay class
 * \note
 * see AreOverlaysInPixelData
 *
 * \todo
 *  Is there actually any way to recognize an overlay ? On images with multiple overlay I do not see
 *  any way to differenciate them (other than the group tag).
 *
 *  Example:
 */
class GDCM_EXPORT Overlay : public Object
{
public:
  Overlay();
  ~Overlay();
  /// Print
  void Print(std::ostream &) const;

  /// Will traverse the dataset in search for overlay elements
  /// this is an hybrid method I don't like having it attach to an Overlay object
  static unsigned int GetNumberOfOverlays(DataSet const & ds);

  /// Update overlay from data element de:
  void Update(const DataElement & de);

  /// Set Group number
  void SetGroup(unsigned short group);
  /// Get Group number
  unsigned short GetGroup() const;
  /// set rows
  void SetRows(unsigned short rows);
  /// get rows
  unsigned short GetRows() const;
  /// set columns
  void SetColumns(unsigned short columns);
  /// get columns
  unsigned short GetColumns() const;
  /// set number of frames
  void SetNumberOfFrames(unsigned int numberofframes);
  /// set description
  void SetDescription(const char* description);
  /// get description
  const char *GetDescription() const;
  /// set type
  void SetType(const char* type);
  /// get type
  const char *GetType() const;
  /// set origin
  void SetOrigin(const signed short *origin);
  /// get origin
  const signed short * GetOrigin() const;
  /// set frame origin
  void SetFrameOrigin(unsigned short frameorigin);
  /// set bits allocated
  void SetBitsAllocated(unsigned short bitsallocated);
  /// return bits allocated
  unsigned short GetBitsAllocated() const;
  /// set bit position
  void SetBitPosition(unsigned short bitposition);
  /// return bit position
  unsigned short GetBitPosition() const;
  /// set overlay from byte array + length
  void SetOverlay(const char *array, unsigned int length);
  ///
  bool GrabOverlayFromPixelData(DataSet const &ds);

  const ByteValue &GetOverlayData() const;

  bool IsEmpty() const;

  /// return true if all bits are set to 0
  bool IsZero() const;

  // return if the Overlay is stored in the pixel data or not
  bool IsInPixelData() const;
  void IsInPixelData(bool b);

  void Decode(std::istream &is, std::ostream &os);

  void Decompress(std::ostream &os) const;

  bool GetBuffer(char *buffer) const;
  bool GetUnpackBuffer(unsigned char *buffer) const;

  Overlay(Overlay const &ov);

private:
  OverlayInternal *Internal;
};

} // end namespace gdcm

#endif //GDCMOVERLAY_H
