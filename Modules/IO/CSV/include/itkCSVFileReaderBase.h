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

#ifndef itkCSVFileReaderBase_h
#define itkCSVFileReaderBase_h

#include "itkLightProcessObject.h"
#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include <vcl_limits.h>
#endif
#include <limits>
#include "itkMacro.h"
#include "itkSize.h"
#include <fstream>
#include "ITKIOCSVExport.h"

namespace itk
{
/** \class CSVFileReaderBase
 * \brief A base class that contains common methods used for parsing csv files.
 *
 * CSVFileReaderBase is a base abstract class for reading csv files. It
 * contains the methods GetDataDimension() which is used to count the number of
 * rows and columns in the file data set. Only after this method is called
 * should calls to the GetNextField() method be made. The method
 * ConvertStringToValueType() can be used to convert a string obtained from
 * GetNextField() to some desired numeric type. The Parse() method is what is
 * used to do the full processing, that is, reading the file and parsing the
 * data into some object. It is pure virtual as a different parsing method
 * would be needed for parsing into different types of objects.
 *
 * The user would need to specify if there are row and column headers in the
 * file using the HasRowHeaders and HasColumnHeaders flags. Also, if row and/or
 * column headers are enclosed in "" or any other string delimiter character
 * being used, the user can set the option of turning the string delimiter
 * character on with the UseStringDelimiterCharacter flag. This is also
 * especially useful if headers such as "This,Header" contain commas or other
 * field delimiter characters within them.
 *
 * If the csv file has row and column headers, they do not necessarily need to
 * be enclosed in "" or other string delimiter characters as long as you
 * specify that row and/or column headers exist in the file. Turning on the
 * HasRowHeaders or HasColumnHeaders flags will tell the reader to recognize
 * them as headers.
 *
 * The PrepareForParsing() method does not need to be called explicitly as it
 * is called in the GetDataDimension() method.
 *
 * \ingroup ITKIOCSV
 */

class ITKIOCSV_EXPORT CSVFileReaderBase:public LightProcessObject
{
public:
  /** Standard class typedefs */
  typedef CSVFileReaderBase         Self;
  typedef LightProcessObject        Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(CSVFileReaderBase, LightProcessObject);

  /** Set the name of the file to be read */
  itkSetStringMacro(FileName);

  /** Set the field delimiter character. It is possible to set another character
  *  as a field delimiter character such as ';'. The default delimiter character
  *  is ',' */
  itkSetMacro(FieldDelimiterCharacter,char);

  /** Get the field delimiter character. */
  itkGetMacro(FieldDelimiterCharacter, char);

  /** Set the UseStringDelimiterCharacter flag on if column or row headers in
  *  the file are enclosed in "" or other characters. */
  itkSetMacro(UseStringDelimiterCharacter,bool);

  /** Get the value of the UseStringDelimiterCharacter flag. */
  itkGetConstMacro(UseStringDelimiterCharacter, bool);

  /** Set the string delimiter character if it is in use. */
  itkSetMacro(StringDelimiterCharacter,char);

  /** Get the string delimiter character. */
  itkGetMacro(StringDelimiterCharacter, char);

  /** Set the HasRowHeaders flag to indicate existence of row headers in the
  *  file. */
  itkSetMacro(HasRowHeaders,bool);

  /** Get the value of the HasRowHeaders flag.*/
  itkGetConstMacro(HasRowHeaders,bool);

  /** Set the HasColumnHeaders flag to indicate existence of column headers in
  *  the file. */
  itkSetMacro(HasColumnHeaders,bool);

  /** Get the value of the HasColumnHeaders flag. */
  itkGetConstMacro(HasColumnHeaders, bool);

  /** Boolean macros for setting HasRowHeaders, HasColumnHeaders and
  *  UseStringDelimiterCharacter. They can conveniently be set by appending
  *  On() or Off() to each of the variable names. */
  itkBooleanMacro(HasRowHeaders);
  itkBooleanMacro(HasColumnHeaders);
  itkBooleanMacro(UseStringDelimiterCharacter);

  /** Counts the number of rows and columns in a file and prepares the file
   * for iterative reading using the GetNextField() method. */
  void GetDataDimension(SizeValueType & rows, SizeValueType & columns);

  /** Gets the next entry in the file. Returns a string. This function
  *  must always only be called after GetDataDimension(). */
  void GetNextField(std::string & );

  /** Converting a string to other numeric value types. This is
  *  a template method in a non-templated class so it needs to
  *  be defined within the .h. The only way it would be possible
  *  to have the definition in the .cxx source file would be either to include
  *  the .cxx file or to use the export keyword, which will be removed
  *  from the c++ language.
  */
  template <typename TData>
  TData ConvertStringToValueType(const std::string str)
  {
    TData value;
    std::istringstream isstream(str);

    if ((isstream >> value).fail() || !(isstream >> std::ws).eof())
    {
      return std::numeric_limits<TData>::quiet_NaN();
    }
    else
    {
      return value;
    }
  }

  /** This method must be defined in derived classes to parse the entire
  *  file into some object. The GetNextField() method only gets one string
  *  at a time. This method would store those strings or convert them using
  *  ConvertStringToValueType() and store the numeric values into the
  *  object. */
  virtual void Parse()=0;

protected:
  std::string                  m_FileName;
  char                         m_FieldDelimiterCharacter;
  char                         m_StringDelimiterCharacter;
  bool                         m_UseStringDelimiterCharacter;
  bool                         m_HasRowHeaders;
  bool                         m_HasColumnHeaders;
  std::ifstream                m_InputStream;
  int                          m_EndOfColumnHeadersLine;
  std::string                  m_Line;

  CSVFileReaderBase();
  virtual ~CSVFileReaderBase() ITK_OVERRIDE {}
  /** Print method */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Check that all essential components are present and plugged in. */
  void PrepareForParsing();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CSVFileReaderBase);
};

} //end namespace itk

#endif
