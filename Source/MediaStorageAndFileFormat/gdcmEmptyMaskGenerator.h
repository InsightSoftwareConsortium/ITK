/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMEMPTYMASKGENERATOR_H
#define GDCMEMPTYMASKGENERATOR_H

#include "gdcmSubject.h"

namespace gdcm {
/**
 * \brief EmptyMaskGenerator
 * Main class to generate a Empty Mask Series from an input Series.  This class
 * takes an input folder and generates a series of DICOM files in the specified
 * output directory.
 * This class handles multiples DICOM Series within the same input directory.
 *
 * The class allow two mode of operations:
 * - UseOriginalSOPClassUID
 * - UseGrayscaleSecondaryImageStorage
 *
 * UseOriginalSOPClassUID is the mode where original attributes are copied from
 * the original DICOM instance.
 *
 * UseGrayscaleSecondaryImageStorage is the mode where attributes are generated
 * so as to create a MultiframeGrayscaleByteSecondaryCaptureImageStorage
 * (MultiframeGrayscaleWordSecondaryCaptureImageStorage) instance.
 *
 * In both mode:
 * - the Study references (StudyInstanceUID and StudyID) are preserved.
 * - the PatientID reference is preserved.
 * - the Image Type attribute will be setup so that the fourth
 *   element is set to 'MASK'.
 * - a new Series Instance UID is generated. It is thus required to run the process
 *   over all files using the same input Series Instance UID so that a proper mapping
 *   from the old Series UID is done to the new one.
 *   Since a new Series Instance UID is generated, there is no sense to
 *   preserve the original Frame of Reference UID, altough it would have made
 *   sense here.
 */
class GDCM_EXPORT EmptyMaskGenerator
{
public:
  EmptyMaskGenerator();
  ~EmptyMaskGenerator();

  enum SOPClassUIDMode {
    UseOriginalSOPClassUID = 0, // default
    UseGrayscaleSecondaryImageStorage
  };

  /// Select generation of SOP Class UID method:
  /// Default is UseOriginalSOPClassUID
  void SetSOPClassUIDMode( SOPClassUIDMode mode );

  /// Specify input directory
  void SetInputDirectory( const char * dirname );

  /// Specify output directory
  void SetOutputDirectory( const char * dirname );

  /// Main loop
  bool Execute();

private:
  struct impl;
  // PIMPL idiom
  impl* pimpl;
};
} // end namespace gdcm
#endif //GDCMEMPTYMASKGENERATOR_H
