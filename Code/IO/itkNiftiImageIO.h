/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNiftiImageIO.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 *         The specification for this file format is taken from the
 *         web site http://www.mayo.edu/bir/PDF/ANALYZE75.pdf.
 * \author Hans J. Johnson
 *         The University of Iowa 2002
 */

#ifndef __itkNiftiImageIO_h
#define __itkNiftiImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageIOBase.h"
#include <nifti1_io.h>

namespace itk
{
/**
   * \ingroup IOFilters
   * \author Hans J. Johnson
   * \brief Class that defines how to read Nifti file format.
   * Nifti IMAGE FILE FORMAT - As much information as I can determine from sourceforge.net/projects/Niftilib
   */
class ITK_EXPORT NiftiImageIO : public ImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef NiftiImageIO            Self;
  typedef ImageIOBase  Superclass;
  typedef SmartPointer<Self>  Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NiftiImageIO, Superclass);

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

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read(void* buffer);

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
  NiftiImageIO();
  ~NiftiImageIO();
  void PrintSelf(std::ostream& os, Indent indent) const;
private:
  void  DefineHeaderObjectDataType();
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
       * \note Key's 0-5 correspond to the Nifti v7.5 orientations, and should not be changed.
       */
  typedef enum {
    ITK_ANALYZE_ORIENTATION_RPI_TRANSVERSE=0,        /**< Denotes a transverse data orientation Right-->Left, */
    ITK_ANALYZE_ORIENTATION_RIP_CORONAL   =1,        /**< Denotes a coronal data orientation */
    ITK_ANALYZE_ORIENTATION_PIR_SAGITTAL  =2,        /**< Denotes a sagittal data orientation */
    ITK_ANALYZE_ORIENTATION_RAI_TRANSVERSE_FLIPPED=3,/**<  */
    ITK_ANALYZE_ORIENTATION_RSP_CORONAL_FLIPPED=4,   /**<  */
    ITK_ANALYZE_ORIENTATION_PIL_SAGITTAL_FLIPPED=5   /**<  */
  } ValidNiftiOrientationFlags;
  nifti_image * m_NiftiImage;
  double m_RescaleSlope;
  double m_RescaleIntercept;

  NiftiImageIO(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};
} // end namespace itk

#endif // __itkNiftiImageIO_h
