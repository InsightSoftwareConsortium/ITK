/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMSPLITMOSAICFILTER_H
#define GDCMSPLITMOSAICFILTER_H

#include "gdcmFile.h"
#include "gdcmImage.h"

namespace gdcm
{

/*
 * Everything done in this code is for the sole purpose of writing interoperable
 * software under Sect. 1201 (f) Reverse Engineering exception of the DMCA.
 * If you believe anything in this code violates any law or any of your rights,
 * please contact us (gdcm-developers@lists.sourceforge.net) so that we can
 * find a solution.
 */
/**
 * \brief SplitMosaicFilter class
 * \details Class to reshuffle bytes for a SIEMENS Mosaic image
 * Siemens CSA Image Header
 * CSA:= Common Siemens Architecture, sometimes also known as Common syngo Architecture
 *
 * \warning when private attributes are not found, the acquisition matrix is
 * used to compute the NumberOfImagesInMosaic. This means trailing black slices
 * will be considered in the volume (instead of discarded).
 * CSA 0029,1010 is needed for correct NumberOfImagesInMosaic
 * CSA 0029,1020 is needed to compute the correct origin
 * without above info default are taken (may not be accurate).
 */
class GDCM_EXPORT SplitMosaicFilter
{
public:
  SplitMosaicFilter();
  ~SplitMosaicFilter();

  /// Split the SIEMENS MOSAIC image
  bool Split();

  /// Compute the new dimensions according to private information
  /// stored in the MOSAIC header.
  bool ComputeMOSAICDimensions(unsigned int dims[3]);

  /// Extract the value for SliceNormalVector (CSA header)
  bool ComputeMOSAICSliceNormal( double dims[3], bool & inverted );

  /// Extract the value for ImagePositionPatient (requires inverted flag)
  bool ComputeMOSAICSlicePosition( double pos[3], bool inverted );

  void SetImage(const Image& image);
  const Image &GetImage() const { return *I; }
  Image &GetImage() { return *I; }

  void SetFile(const File& f) { F = f; }
  File &GetFile() { return *F; }
  const File &GetFile() const { return *F; }

  /// Get the Acquisition Matrix (non zero value):
  static bool GetAcquisitionSize(unsigned int size[2], DataSet const & ds);

  /// Return the value for NumberOfImagesInMosaic, or compute it from Acquisition Size
  static unsigned int GetNumberOfImagesInMosaic( File const & file );

protected:

private:
  SmartPointer<File> F;
  SmartPointer<Image> I;
};

} // end namespace gdcm

#endif //GDCMSPLITMOSAICFILTER_H
