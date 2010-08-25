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
#ifndef GDCMPIXMAP_H
#define GDCMPIXMAP_H

#include "gdcmBitmap.h"
#include "gdcmCurve.h"
#include "gdcmIconImage.h"
#include "gdcmOverlay.h"

namespace gdcm
{

/**
 * \brief Pixmap class
 * A bitmap based image. Used as parent for both IconImage and the main Pixel Data Image
 * It does not contains any World Space information (IPP, IOP)
 *
 * \see PixmapReader
 */
class GDCM_EXPORT Pixmap : public Bitmap
{
public:
  Pixmap();
  ~Pixmap();
  void Print(std::ostream &) const {}

  /// returns if Overlays are stored in the unused bit of the pixel data:
  bool AreOverlaysInPixelData() const;

  /// Curve: group 50xx
  Curve& GetCurve(unsigned int i = 0) {
    assert( i < Curves.size() );
    return Curves[i];
  }
  const Curve& GetCurve(unsigned int i = 0) const {
    assert( i < Curves.size() );
    return Curves[i];
  }
  unsigned int GetNumberOfCurves() const { return Curves.size(); }
  void SetNumberOfCurves(unsigned int n) { Curves.resize(n); }

  /// Overlay: group 60xx
  Overlay& GetOverlay(unsigned int i = 0) {
    assert( i < Overlays.size() );
    return Overlays[i];
  }
  const Overlay& GetOverlay(unsigned int i = 0) const {
    assert( i < Overlays.size() );
    return Overlays[i];
  }
  unsigned int GetNumberOfOverlays() const { return Overlays.size(); }
  void SetNumberOfOverlays(unsigned int n) { Overlays.resize(n); }

  /// Set/Get Icon Image
  const IconImage &GetIconImage() const { return Icon; }
  IconImage &GetIconImage() { return Icon; }

//private:
protected:
  std::vector<Overlay>  Overlays;
  std::vector<Curve>  Curves;
  IconImage Icon;
};

} // end namespace gdcm

#endif //GDCMPIXMAP_H
