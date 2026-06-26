/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMSPLITGRIDFILTER_H
#define GDCMSPLITGRIDFILTER_H

#include "gdcmFile.h"
#include "gdcmImage.h"

namespace gdcm
{

/**
 * \brief SplitGridFilter class
 * \details Class to reshuffle bytes for a UIH Grid image
 * 
 * Only known documentation at the time was found at:
 * https://github.com/rordenlab/dcm2niix/issues/225
 * 
 * Pay attention that SliceNormalVector seems to be inspired from SIEMENS but is 
 * always inverted. Also the IPP in the SQ seems to be accurate, do not use this value for now.
 */
class GDCM_EXPORT SplitGridFilter
{
public:
  SplitGridFilter();
  ~SplitGridFilter();

  /// Split the UIH GRID image
  bool Split();

  /// Compute the new dimensions according to private information
  /// stored in the GRID header.
  bool ComputeGRIDDimensions(unsigned int dims[3]);

  /// Extract the value for SliceNormalVector (UIH header)
  bool ComputeGRIDSliceNormal( double dims[3], bool & inverted );

  /// Extract the value for ImagePositionPatient at index
  bool GetGRIDSlicePosition( unsigned int index, double pos[3]);

  /// Extract the value for ImagePositionPatient
  bool ComputeGRIDImagePositionPatient( double pos[3],
    const double ipp[6],
    const double dircos[6],
    const double pixelspacing[3],
    const unsigned int image_dims[3] ,
    const unsigned int mosaic_dims[3], bool inverted );

  void SetImage(const Image& image);
  const Image &GetImage() const { return *I; }
  Image &GetImage() { return *I; }

  void SetFile(const File& f) { F = f; }
  File &GetFile() { return *F; }
  const File &GetFile() const { return *F; }

  /// Get the Acquisition Matrix (non zero value):
  static bool GetAcquisitionSize(unsigned int size[2], DataSet const & ds);

  /// Return the value for NumberOfImagesInGrid, or compute it from Acquisition Size
  static unsigned int GetNumberOfImagesInGrid( File const & file );
  
private:
  SmartPointer<File> F;
  SmartPointer<Image> I;
};

} // end namespace gdcm

#endif // GDCMSPLITGRIDFILTER_H
