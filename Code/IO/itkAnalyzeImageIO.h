/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkAnalyzeImageIO.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 * \file   Much of the code for this file reader/writer was taken from
 *         the University of Iowa Imaging Group library with the
 *         permission of the authors, Milan Sonka, Joseph Reinhardt,
 *         Ryan Long, Hans Johnson, Gary Christensen, and others.
 *         The specification for this file format is taken from the
 *         web site http://www.mayo.edu/bir/PDF/ANALYZE75.pdf.
 * \author Hans J. Johnson
 *         The University of Iowa 2002
 */

#ifndef __itkAnalyzeImageIO_h
#define __itkAnalyzeImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageIOBase.h"
#include "itkAnalyzeDbh.h"

namespace itk
{
  /**
   * \ingroup IOFilters
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
   *          defined in this file.  The analyze 7.5 header structure may, however, be extended beyond this minimal defintion
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
   */
  class ITK_EXPORT AnalyzeImageIO : public ImageIOBase
  {
    public:
      /** Standard class typedefs. */
      typedef AnalyzeImageIO            Self;
      typedef ImageIOBase  Superclass;
      typedef SmartPointer<Self>  Pointer;

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
      virtual bool CanReadFile(const char* FileNameToRead) ;

      /** Set the spacing and dimension information for the set filename. */
      virtual void ReadImageInformation();

      /** Get the type of the pixel.  */
      virtual const std::type_info& GetPixelType() const;

      /** Reads the data from disk into the memory buffer provided. */
      virtual void Read(void* buffer);

      /** Compute the size (in bytes) of the components of a pixel. For
       * example, and RGB pixel of unsigned char would have a
       * component size of 1 byte. */
      virtual unsigned int GetComponentSize() const;

      /*-------- This part of the interfaces deals with writing data. ----- */

      /** Determine if the file can be written with this ImageIO implementation.
       * \param FileNameToWrite The name of the file to test for writing.
       * \author Hans J. Johnson
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can write the file specified.
       */
      virtual bool CanWriteFile(const char * FileNameToWrite);

      /** Set the spacing and dimension information for the set filename. */
      virtual void WriteImageInformation();

      /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
      virtual void Write(const void* buffer);


    protected:
      AnalyzeImageIO();
      ~AnalyzeImageIO();
      void PrintSelf(std::ostream& os, Indent indent) const;
    private:
      AnalyzeImageIO(const Self&); //purposely not implemented
      void operator=(const Self&); //purposely not implemented
      void SwapBytesIfNecessary(void * buffer, unsigned long numberOfPixels);
      /**
       * \author Hans J. Johnson
       * Performs byte swapping of the Analyze Image header if necessary.
       * \param imageheader An Analyze 7.5 compliant image header.
       * \return void
       */
      void SwapHeaderBytesIfNecessary( struct dsr * const imageheader );

      /**
       * \author Hans J. Johnson
       * Defines the header object data type feilds according to Analyze v7.5 specifications
       * \return nothing
       */
      void  DefineHeaderObjectDataType(void);
#if defined(REORIENT_IMAGES)
      void ReorientIfNecessary(char *p);
      struct ipl_dimensions {
        unsigned int slicestride;
        unsigned int rowstride;
        unsigned int componentstride;
        unsigned int pixelsize;
        //
        // xsize,ysize,zsize == size in each direction in pixesls
        unsigned int xsize;
        unsigned int ysize;
        unsigned int zsize;
      };
      /**
       * \author Kent Williams
       * Get values needed to re-orient image data to
       * Coronal scan order
       * \param dim - structure to fill in
       * \return nothing
       */
      void GetAllDimensions(ipl_dimensions &dim);
      ipl_dimensions m_old_dim,m_new_dim;
#endif
      /**
       * \author Hans J. Johnson
       * Check the endedness of the header file.
       * \param temphdr - a reference to the header structure
       * \return The endedness of the file
       */
      ImageIOBase::ByteOrder CheckAnalyzeEndian(const struct dsr &temphdr);
      /**  All of the information read in from the header file */
      struct dsr m_hdr;
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

#endif // __itkAnalyzeImageIO_h
