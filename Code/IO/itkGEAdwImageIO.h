/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGEAdwImageIO.h
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
 * \author Kent Williams
 *         The University of Iowa 2003
 * \brief This file was written as a modification to the itkMetaImageIO
 *        as a new method for reading in files from the GE4 scanner.
 */

#ifndef __itkGEAdwImageIO_h
#define __itkGEAdwImageIO_h

#include <fstream>
#include "itkImageIOBase.h"
#include "itkIPLCommonImageIO.h"
#include "itkGEImageHeader.h"
#include "idbm_hdr_def.h"

namespace itk
{
  /**
   * \author Hans J. Johnson
   * \brief Class that defines how to read GEAdw file format.
   * */
  class ITK_EXPORT GEAdwImageIO : public IPLCommonImageIO
    {
    public:
      /** Standard class typedefs. */
      typedef GEAdwImageIO            Self;
      typedef IPLCommonImageIO  Superclass;
      typedef SmartPointer<Self>  Pointer;

      /** Method for creation through the object factory. */
      itkNewMacro(Self);

      /** Run-time type information (and related methods). */
      itkTypeMacro(GEAdwImageIO, Superclass);

      /*-------- This part of the interfaces deals with reading data. ----- */

      /** Determine if the file can be read with this ImageIO implementation.
       * \author Hans J Johnson
       * \param FileNameToRead The name of the file to test for reading.
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can read the file specified.
       */
      virtual bool CanReadFile(const char* FileNameToRead) ;

      /** Set the spacing and dimension information for the set filename. */
      // Implemented in superclass
      //      virtual void ReadImageInformation();

      /** Get the type of the pixel.  */
      // Implemented in superclass
      //      virtual const std::type_info& GetPixelType() const;

      /** Reads the data from disk into the memory buffer provided. */
      // Implemented in superclass
      //      virtual void Read(void* buffer);

      /** Compute the size (in bytes) of the components of a pixel. For
       * example, and RGB pixel of unsigned char would have a
       * component size of 1 byte. */
      // Implemented in superclass
      //      virtual unsigned int GetComponentSize() const;

      /*-------- This part of the interfaces deals with writing data. ----- */

      /** Determine if the file can be written with this ImageIO implementation.
       * \param FileNameToWrite The name of the file to test for writing.
       * \author Hans J. Johnson
       * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
       * \return Returns true if this ImageIO can write the file specified.
       */
      // Implemented in superclass
      //      virtual bool CanWriteFile(const char * FileNameToWrite);

      /** Set the spacing and dimension information for the set filename. */
      // Implemented in superclass
      //      virtual void WriteImageInformation();

      /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
      // Implemented in superclass
      //      virtual void Write(const void* buffer);
    protected:
      GEAdwImageIO();
      ~GEAdwImageIO();
      // Implemented in superclass
      //      void PrintSelf(std::ostream& os, Indent indent) const;
      virtual struct GEImageHeader *ReadHeader(const char *FileNameToRead);
    private:
      GEAdwImageIO(const Self&); //purposely not implemented
      void operator=(const Self&); //purposely not implemented
    };

} // end namespace itk

#endif // __itkAnalyzeImageIO_h
