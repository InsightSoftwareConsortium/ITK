/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/**
 * \file   itkAnalyzeImageIO.h
 *         Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://analyzedirect.com/support/10.0Documents/Analyze_Resource_01.pdf
 * \author Hans J. Johnson
 *         The University of Iowa 2002
 */

#ifndef itkAnalyzeImageIO_h
#define itkAnalyzeImageIO_h
#if !defined( ITK_LEGACY_REMOVE )


#include <fstream>
#include "itkImageIOBase.h"
#include "itkAnalyzeDbh.h"

namespace itk
{
/**  \class AnalyzeImageIO
   * \author Hans J. Johnson
   * \brief Class that defines how to read Analyze file format.
   * Analyze IMAGE FILE FORMAT - As much information as I can determine from the Medical image
   * formats web site, and the Analyze75.pdf file provided from the Mayo clinic.
   * A special note of thanks to Dennis P. Hanson (dph@mayo.edu) for his generous contributions
   * in getting this information correct.
   *
   * Analyze image file sets consist of at least 2 files:
   * REQUIRED:
   *    - an image file  ([basename].img or [basename].img.gz or [basename].img.Z)
   *          This contains the binary represenation of the raw voxel values.
   *          If the file is uncompressed, it should be of of size (sizeof(storagetype)*NX*NY*NZ(*NT).
   *          The format of the image file is very simple; containing usually
   *          uncompressed voxel data for the images in one of the several
   *          possible voxel formats:
   *             - 1 bit  packed binary (slices begin on byte boundaries)
   *             - 8 bit  (unsigned char) gray scale unless .lkup file present
   *             - 16 bit signed short
   *             - 32 bit signed integers or float
   *             - 24 bit RGB, 8 bits per channel
   *    - a header file  ([basename].hdr)
   *          This a 348 byte file 99.99% of all images that contains a binary represenation of the C-struct
   *          defined in this file.  The analyze 7.5 header structure may, however, be extended beyond this minimal definition
   *          to encompase site specific information, and would have more than 348 bytes.  Given that the
   *          ability to extend the header has rarely been used, this implementation of the Analyze 7.5
   *          file will only read the first 348 bytes into the structure defined in this file, and all informaiton beyond the
   *          348 bytes will be ignored.
   * OPTIONAL:
   *    - a color lookup file ([basename].lkup)
   *      The .lkup file is a plain ASCII text file that contains 3 integer values between 0 and 255
   *      on each line.  Each line of the lkup file represents one color table entry for the Red,
   *      Green and Blue color components, respectively.  The total dynamic range of the image
   *      is divided by the number of colors in color table to determine mapping of the image through
   *      these colors.
   *       For example, an 8-color 'rainbow colors' lookup table is represented as:
   *       ===========================
   *       255 0 0
   *       255 128 0
   *       255 255 0
   *       128 255 0
   *       0 255 0
   *       0 0 255
   *       128 0 255
   *       255 0 255
   *       ===========================
   *    - an object file ([basename].obj)
   *      A specially formated file with a mapping between object name and image code used to associate
   *      image voxel locations with a label.  This file is run length encoded to save disk storage.
   * \deprecated
   * \ingroup ITKDeprecated
   * \ingroup IOFilters
   */
class AnalyzeImageIO:public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef AnalyzeImageIO       Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
       * \author Hans J Johnson
       * \param FileNameToRead The name of the file to test for reading.
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can read the file specified.
       */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
       * \param FileNameToWrite The name of the file to test for writing.
       * \author Hans J. Johnson
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can write the file specified.
       */
  virtual bool CanWriteFile(const char *FileNameToWrite) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE;

  /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
  virtual void Write(const void *buffer) ITK_OVERRIDE;

  /** Return the directions with a correction for the 2D case. */
  virtual std::vector< double > GetDirection(unsigned int i) const ITK_OVERRIDE;

  /** Return the directions to be assigned by default to recipient
   *  images whose dimension is smaller than the image dimension in file.  */
  virtual std::vector< double > GetDefaultDirection(unsigned int i) const ITK_OVERRIDE;

protected:
  AnalyzeImageIO();
  ~AnalyzeImageIO();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  /**
    * \enum ValidAnalyzeOrientationFlags
    * Valid Orientation values for objects
    * - Key  Description           Origin   dims[1]  dims[2]  dims[3]
    * - =================================================================
    * - 0    transverse-unflipped   IRP       R->L     P->A    I->S
    * - 1    coronal-unflipped      IRP       R->L     I->S    P->A
    * - 2    sagittal-unflipped     IRP       P->A     I->S    R->L
    * - 3    transverse-flipped     IRA       R->L     A->P    I->S
    * - 4    coronal-flipped        SRP       R->L     S->I    P->A
    * - 5    sagittal-flipped       ILP       P->A     I->S    L->R
    * - Where the Origin disignators are with respect to the patient
    * - [(I)nferior|(S)uperior] [(L}eft|(R)ight] [(A)nterior|(P)osterior]
    * \note Key's 0-5 correspond to the Analyze v7.5 orientations, and should not be changed.
    */
  typedef enum {
    ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE = 0,         /**< Denotes a
                                                          transverse data
                                                          orientation
                                                          Right-->Left, */
    ITK_ANALYZE_ORIENTATION_RIP_CORONAL   = 1,          /**< Denotes a coronal
                                                          data orientation */
    ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL  = 2,          /**< Denotes a sagittal
                                                          data orientation */
    ITK_ANALYZE_ORIENTATION_RAI_TRANSVERSE_FLIPPED = 3, /**<  */
    ITK_ANALYZE_ORIENTATION_RSP_CORONAL_FLIPPED = 4,    /**<  */
    ITK_ANALYZE_ORIENTATION_PIL_SAGITTAL_FLIPPED = 5    /**<  */
    } ValidAnalyzeOrientationFlags;

  ITK_DISALLOW_COPY_AND_ASSIGN(AnalyzeImageIO);

  void SwapBytesIfNecessary(void *buffer, SizeType numberOfPixels);

/**
 * Performs byte swapping of the Analyze Image header if necessary.
 * \author Hans J. Johnson
 * \param imageheader An Analyze 7.5 compliant image header.
 * \return void
 */
  void SwapHeaderBytesIfNecessary(struct dsr *const imageheader);

/**
 * Defines the header object data type feilds according to Analyze v7.5 specifications
 * \author Hans J. Johnson
 * \return nothing
 */
  void  DefineHeaderObjectDataType();

#if defined( REORIENT_IMAGES )
  void ReorientIfNecessary(char *p);

  struct ipl_dimensions {
    unsigned int slicestride;
    unsigned int rowstride;
    unsigned int componentstride; x
    unsigned int pixelsize;
    //
    // xsize,ysize,zsize == size in each direction in pixesls
    unsigned int xsize;
    unsigned int ysize;
    unsigned int zsize;
  };
/**
 * Get values needed to re-orient image data to
 * Coronal scan order
 * \author Kent Williams
 * \param dim - structure to fill in
 * \return nothing
 */
  void GetAllDimensions(ipl_dimensions & dim);

  ipl_dimensions m_OldDim, m_NewDim;
#endif
/**
 * Check the endedness of the header file.
 * \author Hans J. Johnson
 * \param temphdr - a reference to the header structure
 * \return The endedness of the file
 */
  ImageIOBase::ByteOrder CheckAnalyzeEndian(const struct dsr & temphdr);

/**  All of the information read in from the header file */
  struct dsr             m_Hdr;
  ImageIOBase::ByteOrder m_MachineByteOrder;
};
extern const char *const ANALYZE_ScanNumber;
extern const char *const ANALYZE_O_MAX;
extern const char *const ANALYZE_O_MIN;
extern const char *const ANALYZE_S_MAX;
extern const char *const ANALYZE_S_MIN;
extern const char *const ANALYZE_CAL_MAX;
extern const char *const ANALYZE_CAL_MIN;
extern const char *const ANALYZE_GLMAX;
extern const char *const ANALYZE_GLMIN;
extern const char *const ANALYZE_AUX_FILE_NAME;
extern const char *const ANALYZE_CALIBRATIONUNITS;
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif // itkAnalyzeImageIO_h
