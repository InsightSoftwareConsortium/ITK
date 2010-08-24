/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRCImageIO.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMRCImageIO_h
#define __itkMRCImageIO_h

#include "itkStreamingImageIOBase.h"
#include "itkMRCHeaderObject.h"
#include <numeric>

namespace itk
{
/** \class MRCImageIO
 *
 *  \brief An ImageIO class to read the MRC file format.
 * The MRC file format frequently has the extension ".mrc" or
 * ".rec". It is used frequently for electron microscopy and is an
 * emerging standard for cryo-electron tomography and molecular
 * imaging. The format is used to represent 2D, 3D images along with
 * 2D tilt series for tomography.
 *
 * The header of the file can contain important information which can
 * not be represented in an Image. Therefor the header is placed into
 * the MetaDataDictionary of "this". The key to access this is
 * MetaDataHeaderName ( fix me when renamed ).
 * \sa MRCHeaderObject MetaDataDictionary
 *
 * This implementation is designed to support IO Streaming of
 * arbitrary regions.
 *
 * As with all ImageIOs this class is designed to work with
 * ImageFileReader and ImageFileWriter, so its direct use is
 * discouraged.
 *
 * \sa ImageFileWriter ImageFileReader ImageIOBase
 */
class ITK_EXPORT MRCImageIO:
  public StreamingImageIOBase
{
public:
  /** Standard class typedefs. */
  typedef MRCImageIO           Self;
  typedef StreamingImageIOBase Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MRCImageIO, StreamingImageIOBase);

  // we don't use this method
  virtual void WriteImageInformation(void) {}

  //-------- This part of the interface deals with reading data. ------

  // See super class for documentation
  virtual bool CanReadFile(const char *);

  // See super class for documentation
  virtual void ReadImageInformation();

  // See super class for documentation
  virtual void Read(void *buffer);

  // -------- This part of the interfaces deals with writing data. -----

  /** \brief Returns true if this ImageIO can write the specified
   * file.
   *
   * The methods verifies that the file extension is known to be
   * supported by this class.
   */
  virtual bool CanWriteFile(const char *);

  // see super class for documentation
  virtual void Write(const void *buffer);

  /** \todo Move to itkIOCommon with the other MetaDataDictionary
   * keys, likely rename the symbol to something like
   * ITK_MRCHHeader. (remember to fix class doc too)
   */
  static const char *m_MetaDataHeaderName;
protected:
  MRCImageIO();
  // ~MRCImageIO(); // default works
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Overloaded to return the actually header size of the file
   * specified. The header must be read before this methods is
   * called.
   */
  virtual SizeType GetHeaderSize(void) const;

private:

  MRCImageIO(const Self &);     //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  // internal methods to update the min and max in the header based on
  // the data, in the image buffer to be written
  template< typename TPixelType >
  void UpdateHeaderWithMinMaxMean(const TPixelType *bufferBegin)
  {
    typedef const TPixelType *ConstPixelPointer;

    ConstPixelPointer bufferEnd = bufferBegin + m_IORegion.GetNumberOfPixels();

    // this could be replaced with std::min_element and
    // std::max_element, but that is slighlty less efficient
    std::pair< ConstPixelPointer, ConstPixelPointer > mm =
      min_max_element(bufferBegin, bufferEnd);

    double mean = std::accumulate( bufferBegin, bufferEnd, double(0.0) )
                  / std::distance(bufferBegin, bufferEnd);

    m_MRCHeader->m_Header.amin = float(*mm.first);
    m_MRCHeader->m_Header.amax = float(*mm.second);
    m_MRCHeader->m_Header.amean = float(mean);
  }

  void UpdateHeaderWithMinMaxMean(const void *bufferBegin);

  // internal methods to update the header object from the ImageIO's
  // set member variables
  void UpdateHeaderFromImageIO(void);

  // reimplemented
  void InternalReadImageInformation(std::ifstream & is);

  virtual void WriteImageInformation(const void *bufferBegin);

  MRCHeaderObject::Pointer m_MRCHeader;
};
} // namespace itk

#endif
