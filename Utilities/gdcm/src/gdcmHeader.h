/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmHeader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#ifndef GDCMHEADER_H
#define GDCMHEADER_H

#include "gdcmCommon.h"
#include "gdcmDocument.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief
 * The purpose of an instance of Header is to act as a container of
 * all the DICOM elements and their corresponding values (and
 * additionaly the corresponding DICOM dictionary entry) of the header
 * of a DICOM file.
 *
 * The typical usage of instances of class Header is to classify a set of
 * dicom files according to header information e.g. to create a file hierarchy
 * reflecting the Patient/Study/Serie informations, or extracting a given
 * SerieId. Accessing the content (image[s] or volume[s]) is beyond the
 * functionality of this class and belongs to gdmcFile.
 * \note  The various entries of the explicit value representation (VR) shall
 *        be managed within a dictionary which is shared by all Header
 *        instances.
 * \note  The Header::Set*Tag* family members cannot be defined as
 *        protected due to Swig limitations for as Has_a dependency between
 *        File and Header.
 */

//-----------------------------------------------------------------------------
// Dicom Part 3.3 Compliant
enum ModalityType {
   Unknow,
   AU,       // Voice Audio
   AS,       // Angioscopy
   BI,       // Biomagnetic Imaging
   CF,       // Cinefluorography
   CP,       // Culposcopy
   CR,       // Computed Radiography
   CS,       // Cystoscopy
   CT,       // Computed Tomography
   DD,       // Duplex Dopler
   DF,       // Digital Fluoroscopy
   DG,       // Diaphanography
   DM,       // Digital Microscopy
   DS,       // Digital Substraction Angiography
   DX,       // Digital Radiography
   ECG,      // Echocardiography
   EPS,      // Basic Cardiac EP
   ES,       // Endoscopy
   FA,       // Fluorescein Angiography
   FS,       // Fundoscopy
   HC,       // Hard Copy
   HD,       // Hemodynamic
   LP,       // Laparoscopy
   LS,       // Laser Surface Scan
   MA,       // Magnetic Resonance Angiography
   MR,       // Magnetic Resonance
   NM,       // Nuclear Medicine
   OT,       // Other
   PT,       // Positron Emission Tomography
   RF,       // Radio Fluoroscopy
   RG,       // Radiographic Imaging
   RTDOSE,   // Radiotherapy Dose
   RTIMAGE,  // Radiotherapy Image
   RTPLAN,   // Radiotherapy Plan
   RTSTRUCT, // Radiotherapy Structure Set
   SM,       // Microscopic Imaging
   ST,       // Single-photon Emission Computed Tomography
   TG,       // Thermography
   US,       // Ultrasound
   VF,       // Videofluorography
   XA,       // X-Ray Angiography
   XC        // Photographic Imaging
};
//-----------------------------------------------------------------------------

class GDCM_EXPORT Header : public Document
{
protected:
   /// \brief In some cases (e.g. for some ACR-NEMA images) the Header Entry Element
   /// Number of the 'Pixel Element' is *not* found at 0x0010. In order to
   /// make things easier the parser shall store the proper value in
   /// NumPixel to provide a unique access facility. See also the constructor
   /// \ref Header::Header
   uint16_t NumPixel;
   /// \brief In some cases (e.g. for some ACR-NEMA images) the header entry for
   /// the group of pixels is *not* found at 0x7fe0. In order to
   /// make things easier the parser shall store the proper value in
   /// GrPixel to provide a unique access facility. See also the constructor
   /// \ref Header::Header
   uint16_t GrPixel;

public:
   Header();
   Header( std::string const & filename );
 
   ~Header();

   // Standard values and informations contained in the header
   bool IsReadable();

   // Some heuristic based accessors, end user intended 
   int GetBitsStored();
   int GetBitsAllocated();
   int GetSamplesPerPixel();
   int GetPlanarConfiguration();
   int GetPixelSize();
   int GetHighBitPosition();
   bool IsSignedPixelData();
   bool IsMonochrome();
   bool IsPaletteColor();
   bool IsYBRFull();

   std::string GetPixelType();
   size_t GetPixelOffset();
   size_t GetPixelAreaLength();

   //Some image informations needed for third package imaging library
   int GetXSize();
   int GetYSize();
   int GetZSize();

   float GetXSpacing();
   float GetYSpacing();
   float GetZSpacing();

   // Useful for rescaling graylevel:
   float GetRescaleIntercept();
   float GetRescaleSlope();

   int GetNumberOfScalarComponents();
   int GetNumberOfScalarComponentsRaw();

   int GetImageNumber();
   ModalityType GetModality();

   float GetXOrigin();
   float GetYOrigin();
   float GetZOrigin();

   bool   HasLUT();
   int    GetLUTNbits();

   std::string GetTransfertSyntaxName();

   /// Accessor to \ref Header::GrPixel
   uint16_t GetGrPixel()  { return GrPixel; }
   
   /// Accessor to \ref Header::NumPixel
   uint16_t GetNumPixel() { return NumPixel; }

   /// Read (used in File)
   void SetImageDataSize(size_t expectedSize);

   void Write(std::ofstream* fp, FileType filetype);
 
protected:
   bool AnonymizeHeader();
   void GetImageOrientationPatient( float iop[6] );

private:
  friend class SerieHeader;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
