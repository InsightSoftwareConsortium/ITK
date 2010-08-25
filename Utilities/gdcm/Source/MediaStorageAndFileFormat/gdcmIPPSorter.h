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
#ifndef GDCMIPPSORTER_H
#define GDCMIPPSORTER_H

#include "gdcmSorter.h"

#include <vector>
#include <string>

namespace gdcm
{
/**
 * \brief IPPSorter
 * Implement a simple Image Position (Patient) sorter, along the Image
 * Orientation (Patient) direction.
 * This algorithm does NOT support duplicate and will FAIL in case of duplicate
 * IPP.
 * \warning See special note for SetZSpacingTolerance when computing the
 * ZSpacing from the IPP of each DICOM files (default tolerance for consistant
 * spacing is: 1e-6mm)
 *
 * For more information on Spacing, and how it is defined in DICOM, advanced
 * users may refers to:
 *
 * http://sourceforge.net/apps/mediawiki/gdcm/index.php?title=Imager_Pixel_Spacing
 *
 * \bug There currently a couple of bug in this implementation:
 * \li Frame Of Reference UID is not taken into account
 * \li Gantry Tilt is not considered
 */
class GDCM_EXPORT IPPSorter : public Sorter
{
public:
  IPPSorter();
  ~IPPSorter();

  // FIXME: I do not like public virtual function...
  /// Main entry point to the sorter.
  /// It will execute the filter, option should be set before
  /// running this function (SetZSpacingTolerance, ...)
  /// Return value indicate if sorting could be achived. Warning this does *NOT* imply
  /// that spacing is consistant, it only means the file are sorted according to IPP
  /// You should check if ZSpacing is 0 or not to deduce if file are actually a 3D volume
  virtual bool Sort(std::vector<std::string> const & filenames);

  /// Functions related to Z-Spacing computation
  /// Set to true when sort algorithm should also perform a regular
  /// Z-Spacing computation using the Image Position (Patient)
  /// Potential reason for failure:
  /// 1. ALL slices are taken into account, if one slice if
  /// missing then ZSpacing will be set to 0 since the spacing
  /// will not be found to be regular along the Series
  void SetComputeZSpacing(bool b) { ComputeZSpacing = b; }
  /// 2. Another reason for failure is that that Z-Spacing is only
  /// slightly changing (eg 1e-3) along the serie, a human can determine
  /// that this is ok and change the tolerance from its default value: 1e-6
  void SetZSpacingTolerance(double tol) { ZTolerance = tol; }
  double GetZSpacingTolerance() const { return ZTolerance; }

  /// Read-only function to provide access to the computed value for the Z-Spacing
  /// The ComputeZSpacing must have been set to true before execution of
  /// sort algorithm. Call this function *after* calling Sort();
  /// Z-Spacing will be 0 on 2 occasions:
  /// \li Sorting simply failed, potentially duplicate IPP => ZSpacing = 0
  /// \li ZSpacing could not be computed (Z-Spacing is not constant, or ZTolerance is too low)
  double GetZSpacing() const { return ZSpacing; }

protected:
  bool ComputeZSpacing;
  double ZSpacing;
  double ZTolerance;

private:
  bool ComputeSpacing(std::vector<std::string> const & filenames);
};


} // end namespace gdcm

#endif //GDCMIPPSORTER_H
