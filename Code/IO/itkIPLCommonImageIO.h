/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkIPLCommonImageIO.h
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

#ifndef __itkIPLCommonImageIO_h
#define __itkIPLCommonImageIO_h

#include <fstream>
#include <cstdlib>
#include "itkImageIOBase.h"
#include "itkGEImageHeader.h"
#include "idbm_hdr_def.h"

namespace itk
{
  /**
   * \author Hans J. Johnson
   * \brief Class that defines how to read GE4 file format.
   * */
  class ITK_EXPORT IPLCommonImageIO : public ImageIOBase
    {
    public:
      /** Standard class typedefs. */
      typedef IPLCommonImageIO            Self;
      typedef ImageIOBase  Superclass;
      typedef SmartPointer<Self>  Pointer;
      typedef unsigned char U8;
      typedef signed char S8;
      typedef unsigned short U16;
      typedef signed short S16;
      typedef unsigned int U32;
      typedef signed int S32;
      typedef unsigned long long U64;
      typedef signed long long S64;
      typedef float F32;
      typedef double F64;

      /** Method for creation through the object factory. */
      itkNewMacro(Self);

      /** Run-time type information (and related methods). */
      itkTypeMacro(IPLCommonImageIO, Superclass);

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
      enum ENUM_PLANE_SELECTION
  {
    NO_PLANE = 0,
    AXIAL = 1,
    CORONAL = 2,
    SAGITTAL = 3,
    UNKNOWN_PLANE = 5
  };


      struct FILESORTINFOSTRUCT {
  char imageFileName[MAXPATHLEN+1];
  float SliceLocation;
  int  SliceOffset;
  int echoNumber;
  void * data;
      };
      typedef struct FILESORTINFOSTRUCT FILESORTINFO;
    protected:
      IPLCommonImageIO();
      ~IPLCommonImageIO();
      void PrintSelf(std::ostream& os, Indent indent) const;


      enum { MAX_FILENAMELIST_SIZE = 512 };

      struct FILENAMELISTSTRUCT
      {
  FILESORTINFO Info[MAX_FILENAMELIST_SIZE];
  int  XDim;
  int  YDim;
  int Key1;  /** Key that must be matched for image to be used, i.e. seriesNumber, extensionkey*/
  int Key2;  /** Key that must be matched for image to be used, i.e. echoNumber*/
  int numImageInfoStructs;
  int maxImageFileNames;
      };
      typedef struct FILENAMELISTSTRUCT FILENAMELIST;
      void InitializeFILENAMELIST( FILENAMELIST * const fnList );
      int AddElementToList(FILENAMELIST * const fnList,char const * const filename, const float sliceLocation, const int offset, const int XDim, const int YDim, const int Key1, const int Key2 );
      void sortImageListAscend (FILENAMELIST * const fnList);
      void sortImageListDescend (FILENAMELIST * const fnList);
      int statTimeToAscii (void *clock, char *timeString);

      virtual struct GEImageHeader *ReadHeader(const char *FileNameToRead);
      //
      // data members
      struct GEImageHeader *m_ImageHeader;
      ImageIOBase::ByteOrder m_system_byteOrder;
      FILENAMELIST m_fnlist;
      //
      // return 0 on success, -1 on failure
      int GetStringAt(std::ifstream &f,std::streamoff Offset,char *buf,
          std::size_t amount, bool throw_exception = true);
      int GetIntAt(std::ifstream &f,std::streamoff Offset,int *ip,
       bool throw_exception = true);
      int GetShortAt(std::ifstream &f,std::streamoff Offset,short *ip,
         bool throw_exception = true);
      int GetFloatAt(std::ifstream &f,std::streamoff Offset,float *ip,
         bool throw_exception = true);

      short hdr2Short(char *hdr);
      int hdr2Int(char *hdr);
      float hdr2Float(char *hdr);
    private:
      IPLCommonImageIO(const Self&); //purposely not implemented
      void operator=(const Self&); //purposely not implemented
    };

} // end namespace itk
#define RAISE_EXCEPTION() \
        { ExceptionObject exception(__FILE__, __LINE__); \
        exception.SetDescription("File cannot be read"); \
        throw exception; }

#define IOCHECK() \
      if(f.fail()) \
      { \
  if(f.is_open()) \
    f.close(); \
  RAISE_EXCEPTION(); \
      }


#endif // __itkAnalyzeImageIO_h
