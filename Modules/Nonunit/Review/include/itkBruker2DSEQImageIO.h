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
 * \file   itkBruker2DSEQImageIO.h
 *         The code for this file reader was written based on
 *         Bruker image file sets generated at the Center for NMR Research
 *         at the Penn State Milton S. Hershey Medical Center.
 *
 *      NOTE: Currently this only reads in the "necessary" information,
 *      but eventually it should read the hundreds of Bruker specific
 *      parameters and encapsulate them in the meta dictionary.
 *
 * \author Don C. Bigler
 *         The Pennsylvania State University 2004
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://hdl.handle.net/1926/1381
 *
 */

#ifndef __itkBruker2DSEQImageIO_h
#define __itkBruker2DSEQImageIO_h


#include "itkImageIOBase.h"
#include "itkVectorContainer.h"

namespace itk
{
/** \class Bruker2DSEQImageIO
 * \ingroup IOFilters
 * \author Don C. Bigler
 * \brief Class that defines how to read Bruker file format.
 * Bruker IMAGE FILE FORMAT - The following is a brief description of the Bruker
 * file format taken from:
 *
 * http://www.mrc-cbu.cam.ac.uk/Imaging/Common/brukerformat.shtml.
 *
 * For the Bruker format, a single scanning session is stored in its own
 * directory. The directory is named according to the subject name or number, as
 * typed in by the scanner operator. The directory name usually specifies the
 * subject name/number and which session this is for that subject. Thus, a
 * typical directory name would be
 *
 *         010005-m00.5X1
 *
 * where '010005' is the WBIC subject number of the subject, the 00 of 'm00'
 * means this is the first scanning session that subject number of the session
 * for that subject, on that day (1). The first two letters specify the day,
 * since some arbitrary base date - set in the Bruker software.
 *
 * The WBIC number (e.g. 010005) in turn is made up of the year the subject was
 * first scanned (01 = 2001) and where that subject is in the sequence of new
 * numbers allocated (5 = 5th subject in the year 2001 who has needed a new
 * subject number).
 *
 * Within the session directory (e.g 010005-m00.5X1), there are:
 *          1.  a text file, named 'subject'
 *          2.  A subdirectory numbered 1, 2, 3 etc, for each saved run of data
 *              acquisition in that session
 *
 * Within each of the run directories (e.g. 010005-m00.5X1/5) there are the
 * following:
 *          1.  an 'imnd' or 'method' text file, containing parameters used in
 *              setting up and acquiring the data from the scanner
 *          2.  an 'acqp' (ACquisition Parameter) text file, with further details
 *              of the acquisition parameters used for that run
 *          3.  a large 'fid' file, that contains the raw, unreconstructed MR
 *              Free Induction Decay data
 *          4.  a subdirectory 'pdata' (Processed Data), that contains any
 *              reconstructions of the data into images
 *          5.  various other files, including the 'pulseprogram' and 'grdprog.r'
 *              gradient programs
 *
 * Within each of the 'pdata' subdirectories (e.g 01001-m00.5X1/5/pdata), there
 * are numbered subdirectories (1,2,3 etc) for each new reconstruction of the raw
 * data into images. There is usually only one such reconstruction, in
 * subdirectory '1'.
 *
 * Within each reconstruction directory (e.g 01001-m00.5X1/5/pdata/1), there are:
 *          1.  a '2dseq' file. This is the 3D (structural etc) or 4D (FMRI)
 *              image file.  It is simply a binary block of data, which could
 *              simply be renamed as 'myscan.img' to make an acceptable Analyze
 *              .img file.
 *          2.  a 'reco' text file, containing some details of the
 *              reconstruction.
 *          3.  a 'meta' text file, which I think contains information for the
 *              display of the 2dseq file within ParaVision.
 *          4.  a 'd3proc' file, containing image size definition parameters,
 *              inter alia.
 *
 * The 'reco', 'acqp', 'd3proc', and '2dseq' files are required for this reader.
 * It will look for these files in the directory structure described above.  The
 * path and filename of the '2dseq' file must be the name of the file to read
 * (see CanReadFile).  This class supports reading only.
 * \ingroup ITKReview
 */
class Bruker2DSEQImageIO:public ImageIOBase
{
public:
  typedef ImageIOBase SuperClass;

  /** Standard class typedefs. */
  typedef Bruker2DSEQImageIO   Self;
  typedef ImageIOBase          Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Bruker2DSEQImageIO, Superclass);

  /** Special types used for Bruker meta data. */
  typedef VectorContainer< unsigned int, double > RECOFOVContainerType;
  typedef VectorContainer< unsigned int, int >    RECOTranspositionContainerType;
  typedef VectorContainer< unsigned int, double > ACQEchoTimeContainerType;
  typedef VectorContainer< unsigned int, double > ACQRepetitionTimeContainerType;
  typedef VectorContainer< unsigned int, double > ACQInversionTimeContainerType;
  typedef VectorContainer< unsigned int, double > ACQSliceSepnContainerType;

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
       * \author Don C. Bigler
       * \param FileNameToRead The name of the file to test for reading.
       * \return Returns true if this ImageIO can read the file specified.
       */
  virtual bool CanReadFile(const char *FileNameToRead) ITK_OVERRIDE;

  /** Set the spacing and dimension information for the set filename. */
  virtual void ReadImageInformation() ITK_OVERRIDE;

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void *buffer) ITK_OVERRIDE;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
       * FileNameToWrite The name of the file to test for writing.
       * \author Don C. Bigler
       * \post This function will always return false (Not implemented).
       * \return Returns true if this ImageIO can write the file specified.
       */
  virtual bool CanWriteFile( const char *itkNotUsed(FileNameToWrite) ) ITK_OVERRIDE
  {
    return false;
  }

  /** Set the spacing and dimension information for the set filename. */
  virtual void WriteImageInformation() ITK_OVERRIDE
  {
    return;
  }

  /** Writes the data to disk from the memory buffer provided. Make sure
       * that the IORegions has been set properly. */
  virtual void Write( const void *itkNotUsed(buffer) ) ITK_OVERRIDE
  {
    return;
  }

protected:
  Bruker2DSEQImageIO();
  ~Bruker2DSEQImageIO();
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  Bruker2DSEQImageIO(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

  void SwapBytesIfNecessary(void *buffer, SizeValueType numberOfPixels);

  ImageIOBase::ByteOrder m_MachineByteOrder;
};

extern const char *const RECO_BYTE_ORDER;
extern const char *const RECO_FOV;
extern const char *const RECO_SIZE;
extern const char *const RECO_WORDTYPE;
extern const char *const RECO_IMAGE_TYPE;
extern const char *const RECO_TRANSPOSITION;
extern const char *const ACQ_DIM;
extern const char *const NI; /*IMND_N_SLICES*/
extern const char *const NR;
extern const char *const ACQ_SLICE_THICK; /*IMND_SLICE_THICK*/
extern const char *const NECHOES;         /*IMND_N_ECHO_IMAGES*/
extern const char *const ACQ_SLICE_SEPN;  /*IMND_SLICE_SEPN*/
extern const char *const ACQ_SLICE_SEPN_MODE;
extern const char *const ACQ_ECHO_TIME;
extern const char *const ACQ_REPETITION_TIME;
extern const char *const ACQ_INVERSION_TIME;
} // end namespace itk

#endif // __itkBruker2DSEQImageIO_h
