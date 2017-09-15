/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMFILEDECOMPRESSLOOKUPTABLE_H
#define GDCMFILEDECOMPRESSLOOKUPTABLE_H

#include "gdcmSubject.h"
#include "gdcmFile.h"
#include "gdcmPixmap.h"

namespace gdcm
{

class DataElement;
/**
 * \brief FileDecompressLookupTable class
 * \details It decompress the segmented LUT into linearized one (only PALETTE_COLOR images)
 * Output will be a PhotometricInterpretation=RGB image
 */
class GDCM_EXPORT FileDecompressLookupTable : public Subject
{
public:
  FileDecompressLookupTable() {}
  ~FileDecompressLookupTable() {}

  /// Decompress
  bool Change();

  /// Set/Get File
  void SetFile(const File& f) { F = f; }
  File &GetFile() { return *F; }

  const Pixmap& GetPixmap() const { return *PixelData; }
  Pixmap& GetPixmap() { return *PixelData; }
  void SetPixmap(Pixmap const &img) { PixelData = img; }

protected:

private:
  SmartPointer<File> F;
  SmartPointer<Pixmap> PixelData;
};

} // end namespace gdcm

#endif //GDCMFILEDECOMPRESSLOOKUPTABLE_H
