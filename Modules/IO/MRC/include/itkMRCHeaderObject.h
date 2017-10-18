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
#ifndef itkMRCHeaderObject_h
#define itkMRCHeaderObject_h
#include "ITKIOMRCExport.h"

#include "itkObjectFactory.h"
#include "itkLightObject.h"
#include "itkIntTypes.h"

namespace itk
{

/** \class MRCHeaderObject
 * \brief This class is a light wrapper for a couple of plain old data
 * structures, so that they can be utilized in a MetaDataDictionary.
 *
 * The class is designed to be intialized from a chuck of bytes that
 * are read in from a file. Verification is performed to ensure that
 * the bytes appear to be the correct format for the headers.
 *
 * The POD structures a publicly avaible without access methods.
 *
 *  This code was contributed in the Insight Journal paper:
 *  "A Streaming IO Base Class and Support for Streaming the MRC and VTK File Format"
 *  by Lowekamp B., Chen D.
 *  http://www.insight-journal.org/browse/publication/729
 *  https://hdl.handle.net/10380/3171
 *
 * \sa MetaDataDictionary
 * \ingroup ITKIOMRC
 */
class ITKIOMRC_EXPORT MRCHeaderObject:
  public LightObject
{
public:
  /** Standard class typedefs. */
  typedef MRCHeaderObject            Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** plain old data structure of the MRC header as used by IMOD. This
   * header must be 1024 bytes.
   *
   * The structure of the MRC header taken from:
   * http://bio3d.colorado.edu/imod/doc/mrc_format.txt with permision
   * from David Mastronarde on 8/21/2009
   */
  struct Header
  {
    int32_t nx;            /**< Number of Columns */
    int32_t ny;            /**< Number of Rows */
    int32_t nz;            /**< Number of Sections */

    /** Types of pixel in image.  Values used by IMOD:
     * 0 = unsigned bytes,
     * 1 = signed short integers (16 bits),
     * 2 = float,
     * 3 = short * 2, (used for complex data)
     * 4 = float * 2, (used for complex data)
     * 6 = unsigned 16-bit integers (non-standard)
     * 16 = unsigned char * 3 (for rgb data, non-standard)
    */
    int32_t mode;

    /**  Starting point of sub image. (ignored) */
    int32_t nxstart;
    int32_t nystart;
    int32_t nzstart;

    /** Grid size in X, Y, and Z */
    int32_t mx;
    int32_t my;
    int32_t mz;

    /** Cell size; pixel spacing = xlen/mx */
    float xlen;
    float ylen;
    float zlen;

    /**cell angles (ignored) */
    float alpha;
    float beta;
    float gamma;

    int32_t mapc;           /**< map column  1=x,2=y,3=z. (ignored)  */
    int32_t mapr;           /**< map row     1=x,2=y,3=z. (ignored)  */
    int32_t maps;           /**< map section 1=x,2=y,3=z. (ignored)  */

    // These need to be set for proper scaling of
    // non byte data.
    float amin;               /**< Minimum pixel value.  */
    float amax;               /**< Maximum pixel value.  */
    float amean;              /**< Mean pixel value.  */

    int16_t ispg;           /**< image type  */
    int16_t nsymbt;         /**< space group number  */

    int32_t next;           /**< number of bytes in extended header */
    int16_t creatid;        /**< Creator ID  */

    int8_t notused1[30];     /**<     extra data (not used)  */

    // These two values specify the structure of data in the
    // extended header; their meaning depend on whether the
    // extended header has the Agard format, a series of
    // 4-byte integers then real numbers, or has data
    // produced by SerialEM, a series of short integers.
    // SerialEM stores a float as two shorts, s1 and s2, by:
    // value = (sign of s1)*(|s1|*256 + (|s2| modulo 256))
    // * 2**((sign of s2) * (|s2|/256))

    int16_t nint;          // Number of integers per section (Agard format) or
                           // number of bytes per section (SerialEM format)
    int16_t nreal;         // Number of reals per section (Agard format) or
    // flags for which types of short data (SerialEM format):
    // 1 = tilt angle * 100  (2 bytes)
    // 2 = piece coordinates for montage  (6 bytes)
    // 4 = Stage position * 25    (4 bytes)
    // 8 = Magnification / 100 (2 bytes)
    // 16 = Intensity * 25000  (2 bytes)
    // 32 = Exposure dose in e-/A2, a float in 4 bytes
    // 128, 512: Reserved for 4-byte items
    // 64, 256, 1024: Reserved for 2-byte items
    // If the number of bytes implied by these flags does
    // not add up to the value in nint, then nint and nreal
    // are interpreted as ints and reals per section

    /** extra data (not used) */
    int8_t notused2[28];

    // Explanation of type of data.
    int16_t idtype;        // ( 0 = mono, 1 = tilt, 2 = tilts, 3 = lina, 4 =
                           // lins)
    int16_t lens;
    int16_t nd1;           // for idtype = 1, nd1 = axis (1, 2, or 3)
    int16_t nd2;
    int16_t vd1;           // vd1 = 100. * tilt increment
    int16_t vd2;           // vd2 = 100. * starting angle

    // Used to rotate model to match new rotated image.
    float tiltangles[6];     // 0,1,2 = original:  3,4,5 = current

    // NEW-STYLE MRC image2000 HEADER - IMOD 2.6.20 and above:
    float xorg;              // Origin of image.  Used to auto translate model
    float yorg;              // to match a new image that has been translated.
    float zorg;

    char cmap[4];            /**< Contains "MAP "  */
    char stamp[4];           /**< First byte has 17 for big- or 68 for
                               little-endian  */
    float rms;

    // ALL HEADERS:
    int32_t nlabl;         /**< Number of labels with useful data.  */
    char label[10][80];    /**< 10 labels of 80 characters.  */
  };

  /** Fei/Agard extended header */
  struct FeiExtendedHeader
  {
    float atilt;        /**< alpha tilt  */
    float btilt;        /**< beta tilt  */
    float xstage;       /**< Stage x position  (unit=m, huh if > 1)  */
    float ystage;       /**< Stage y position  (unit=m, huh if > 1)  */
    float zstage;       /**< Stage z position  (unit=m, huh if > 1)  */
    float xshift;       /**< Image shift x (unit=m, huh if > 1)  */
    float yshift;       /**< Image shift y (unit=m, huh if > 1)  */
    float defocus;      /**< (unit=m, huh if > 1)  */
    float exptime;      /**< time is seconds  */
    float meanint;      /**< mean value  */
    float tiltaxis;     /**< tilt axis in degree  */
    float pixelsize;    /**< pixel size (unit=m, huh if > 1)  */
    float magnification;
    char notused[76];   /**< fill up 128 bytes  */
  };

  /** pixel type enumeration */
  enum { MRCHEADER_MODE_UINT8 = 0,
         MRCHEADER_MODE_IN16 = 1,
         MRCHEADER_MODE_FLOAT = 2,
         MRCHEADER_MODE_COMPLEX_INT16 = 3,
         MRCHEADER_MODE_COMPLEX_FLOAT = 4,
         MRCHEADER_MODE_UINT16 = 6,
         MRCHEADER_MODE_RGB_BYTE = 16 };

  /** map enumeration */
  enum { MRCHEADER_MAP_X = 1,
         MRCHEADER_MAP_Y = 2,
         MRCHEADER_MAP_Z = 3 };

public:

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MRCHeaderObject, LightObject);

  void DeepCopy(ConstPointer h);

  /** \param buffer is assumed to point to a 1024 block of memory
   * which has the header
   *
   * The return value indicates if buffer appears valid. If
   * false is returned then the header member varible is in an undefined
   * state.
   *
   * If the byte order of the header then byte swapping will be
   * performed.
   */
  bool SetHeader(const Header *buffer);

  const Header & GetHeader() const;

  /** After SetHeader is called GetExtendedHeaderSize contains the
   * extected size of the buffer argument. This buffer is expected to
   * be the bytes which follow the header in the file.
   *
   * The return value indicates if the extended header buffer is valid
   * and known. If false is returned then extended header information
   * is not available.
   */
  bool SetExtendedHeader(const void *buffer);

  /** the expected number of bytes in the extended header, this is only
   * valid after a successful call to SetHeader.
   */
  SizeValueType GetExtendedHeaderSize() const;

  /** the expected number of bytes in the header */
  SizeValueType GetHeaderSize(void) const
    {
      return sizeof( Header );
    }

  /** returns true if the original header from SetHeader was big
   * endian.
   */
  bool IsOriginalHeaderBigEndian() const;

  /** Public avaiable data : FIXME : NO MEMBER VARIABLES SHOULD BE PUBLIC. */
  Header m_Header;                    // FIXME : This should be private and
                                      // should have Get/Set Methods.

protected:

  MRCHeaderObject();
  ~MRCHeaderObject() ITK_OVERRIDE;

  /** Methods to fix the order of a set header */
  void swapHeader(bool bigEndian);

  /** Prints loads of information from the header */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MRCHeaderObject);

  SizeValueType m_ExtendedHeaderSize;
  void *        m_ExtendedHeader;

  FeiExtendedHeader *m_ExtendedFeiHeader;

  bool m_BigEndianHeader;
};
} // namespace itk

#endif
