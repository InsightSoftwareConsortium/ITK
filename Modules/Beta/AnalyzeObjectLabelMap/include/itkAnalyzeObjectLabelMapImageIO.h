/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    $RCSfile: itkAnalyzeObjectLabelMapImageIO.h,v $
Language:  C++
Date:      $Date: 2007/03/29 20:11:16 $
Version:   $Revision: 1.5 $

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef itkAnalyzeObjectLabelMapImageIO_h
#define itkAnalyzeObjectLabelMapImageIO_h

#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageRegionIterator.h"

#include "itkAnalyzeObjectEntry.h"

namespace itk
{
using AnalyzeObjectEntryArrayType = std::vector<AnalyzeObjectEntry::Pointer>;
const char *const ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY = "ANALYZE_OBJECT_LABEL_MAP_ENTRY_ARRAY";
/**
* Constants representing the current version number of the object map file for Analyze
*/
constexpr int VERSION1 = 880102;
constexpr int VERSION2 = 880801;
constexpr int VERSION3 = 890102;
static const int VERSION4 = 900302;
static const int VERSION5 = 910402;
static const int VERSION6 = 910926;
static const int VERSION7 = 20050829;

/**
  * Buffer size for reading in the run length encoded object data
  */
constexpr int NumberOfRunLengthElementsPerRead = 1;

/** \class AnalyzeObjectLabelMapImageIO
 *   \ingroup AnalyzeObjectMapIO
 *
 */
class ITK_EXPORT AnalyzeObjectLabelMapImageIO : public ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(AnalyzeObjectLabelMapImageIO);

  /** Standard class type alias. */
  using Self = AnalyzeObjectLabelMapImageIO;
  using Superclass = ImageIOBase;
  using Pointer = SmartPointer<Self>;

  using RGBPixelType = itk::RGBPixel<int>;
  using ImageType = itk::Image<unsigned char, 4>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeObjectLabelMapImageIO, Superclass);

  /*-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this ImageIO implementation.
   * \author Hans J Johnson
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can read the file specified.
   */
  bool CanReadFile(const char* FileNameToRead) override;

  /** Set the spacing and dimension information for the set filename. */
  void ReadImageInformation() override;

  /** Reads the data from disk into the memory buffer provided. */
  void Read(void* buffer) override;

  /*-------- This part of the interfaces deals with writing data. ----- */

  /** Determine if the file can be written with this ImageIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \author Hans J. Johnson
   * \post Sets classes ImageIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this ImageIO can write the file specified.
   */
  bool CanWriteFile(const char * FileNameToWrite) override;

  /** Set the spacing and dimension information for the set filename. */
  void WriteImageInformation() override;

  /** Writes the data to disk from the memory buffer provided. Make sure
   * that the IORegions has been set properly. */
  void Write(const void* buffer) override;

  // Streaming not yet supported, so use the default base class to return the LargestPossibleRegion
#if _USE_STREAMABLE_REGION_FOR_AOLM
  /** Calculate the region of the image that can be efficiently read
   *  in response to a given requested region. */
  virtual ImageIORegion GenerateStreamableReadRegionFromRequestedRegion( const ImageIORegion & requestedRegion ) const;

#endif

  bool CanStreamRead() override
  {
    return false;
  }

protected:
  AnalyzeObjectLabelMapImageIO();
  ~AnalyzeObjectLabelMapImageIO() override;
  void PrintSelf(std::ostream& os, Indent indent) const override;

private:

  std::ifstream m_InputFileStream;
  int           m_LocationOfFile;
  //  int           m_CollapsedDims[8];
};

} // end namespace itk

#endif //itkAnalyzeObjectLabelMapImageIO_h
