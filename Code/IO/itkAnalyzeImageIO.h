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
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the Analyze 7.5 specification.
 *        An Analyze 7.5 specification consists of 2 files.  The first file is has a .hdr extension
 *        that is the header information containing specific information about the
 *        image dimensions, and other information concerning the data.
 *        The second file has a .img file extenstion and it contains the raw image data.
 */

/**
 * \todo
 *  \par Remove all DEBUG statements, and re-read NOTES to make more complete.
 *  \par Determine orientation of file from analyze header information and re-orient to a common orientation
 *       of transverse unflipped.
 *       This will require that the desired orientation.
 *  \par Determine how to maintain all the "non-image" information that is available in Analyze headers
 *       (i.e. patient id, patient name, scan date, etc).
 *  \par Determine what the compression flags mean, and allow for reading and writting of compressed images.
 *       For now, all image files are read with the gzread() in order to uncompress images when necessary images.
 *  \par Write a rigourous testing mechanism for this reader.
 *  \par Remove, or determine how to properly resolve hack that analyze 7.5 always expects a 4D image.
 *  \par Determine how to specifiy other format information, for example, how do you write out in all the
 *       different possible orientations. Currently, all images are output in an assumed orientation of
 *       transverse unflipped.
 *
 *  \ingroup IOFilters
 *
 */

#ifndef __itkAnalyzeImageIO_h
#define __itkAnalyzeImageIO_h

#include <fstream>
#include "itkImageIOBase.h"
#include "dbh.h"

namespace itk
{
  /**
   * \author Hans J. Johnson
   * \brief Class that defines how to read Analyze file format.
   * */
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
      ipl_dimensions m_old_dim,m_new_dim;
  };

} // end namespace itk

#endif // __itkAnalyzeImageIO_h
