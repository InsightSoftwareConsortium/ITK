/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericSeriesFileIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericSeriesFileIterator_h
#define __itkNumericSeriesFileIterator_h

#include "itkFileIteratorBase.h"
#include "itkExceptionObject.h"

namespace itk
{

/** \brief Exception class for missing file in series. */
class NumericSeriesException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( NumericSeriesException, ExceptionObject );

  /** Constructor. */
  NumericSeriesException(const char *file, unsigned int line, 
                         const char* message = "Missing file in series") : 
    ExceptionObject(file, line)
    {
      SetDescription(message);
    }
};

/** \class NumericSeriesFileIterator
 * \brief Generate an ordered sequence of filenames.
 *
 * This class is used to generate an ordered sequence of files
 * whose filenames contain a single unique, non-negative, integral
 * value (e.g. test.1.png, test2.png, foo.3, etc.). The class oeprates
 * in either write or read mode. In write mode, the StartIndex is used to
 * name the first file, and the index is incremented by one to produce the
 * next file in the series. Also note that in write mode the files are 
 * assumed not to exist. In read mode, the files are assumed to exist and
 * NumericSeriesFileIterator will attempt to find them in sequence. Note
 * that in read mode the file iterator can be set up to throw an exception
 * if a file is missing in a sequence or continue reading until the end
 * of the sequence. (The instance variable "NumberOfFiles" should
 * be set to indicate the expected number of files in the sequence.)
 * 
 * The file name is created from a sprintf-style series
 * format which should contain an integer format string like "%d". Bad 
 * formats will cause the series reader to throw an exception.
 *
 * \ingroup IOFilters
 *
 */
class ITK_EXPORT NumericSeriesFileIterator : public FileIteratorBase
{
public:
  /** Standard class typedefs. */
  typedef NumericSeriesFileIterator    Self;
  typedef FileIteratorBase             Superclass;
  typedef SmartPointer<Self>           Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(NumericSeriesFileIterator, FileIteratorBase);

  /* -------- Satisfy the API of FileIteratorBase ------------- */

  /** Return the first file in the sequence. If the empty string "" 
   * is returned, then there are no files in the sequence. */
  virtual const std::string& Begin();

  /** Return the next file in the sequence. If the empty string "" 
   * is returned, then there are no additional files in the sequence. */
  virtual const std::string& operator++();

  /** Return the previous file in the sequence. If the empty string "" 
   * is returned, then there are no previous files in the sequence. */
  virtual const std::string& operator--();

  /* -------- Define the API for NumericSeriesFileIterator ----------- */

  /** Use this method to set the starting index of the numeric series.
   * The default value is 1. */
  itkSetMacro(StartIndex,unsigned long);
  itkGetMacro(StartIndex,unsigned long);

  /** Use this method to set the end index of the numeric series.
   * The default value is 10000. This file iterator will attempt to
   * read NumberOfFiles files beginning with StartIndex (make sure
   * ThrowExceptionOnMissingFile is off if there are gaps in the series
   * numbering.) If EndIndex is reached without reading NumberOfFiles
   * files then an exception is thrown. */
  itkSetMacro(EndIndex,unsigned long);
  itkGetMacro(EndIndex,unsigned long);

  /** Set/Get the number of files in the series. This is used in read
   * mode to limit the the number of possible files in the series. */
  itkSetMacro(NumberOfFiles,unsigned long);
  itkGetMacro(NumberOfFiles,unsigned long);

  /** If this flag is set (default is on) then a missing file in the
   * sequence causes an exception to be thrown. This flag is only
   * used when the file iterator is in read mode. */
  itkSetMacro(ThrowExceptionOnMissingFile,bool);
  itkGetMacro(ThrowExceptionOnMissingFile,bool);
  itkBooleanMacro(ThrowExceptionOnMissingFile);

protected:
  NumericSeriesFileIterator();
  ~NumericSeriesFileIterator();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** The starting index for writing a sequence of files. */
  unsigned long m_StartIndex;

  /** The ending index for writing a sequence of files. */
  unsigned long m_EndIndex;

  /** The number of files in the sequence. */
  unsigned long m_NumberOfFiles;

  /** The number of files in the sequence; the current number of files 
   * in the series. */
  unsigned long m_CurrentIndex;
  unsigned long m_CurrentNumberOfFiles;

  /** Throw an exception if the sequence ordering is interrupted. */
  bool m_ThrowExceptionOnMissingFile;

  /** A helper function produces filenames from the series format. */
  const std::string& ProduceNextFileName(unsigned long idx);

  /** A helper function determines whether a file exists. */
  bool FileExists(const char* filename);

private:
  NumericSeriesFileIterator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} //namespace ITK

#endif // __itkNumericSeriesFileIterator_h
