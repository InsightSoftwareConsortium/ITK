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

#ifndef itkCSVArray2DFileReader_h
#define itkCSVArray2DFileReader_h

#include "itkCSVFileReaderBase.h"
#include "itkCSVArray2DDataObject.h"

namespace itk
{

/** \class CSVArray2DFileReader
 * \brief Parses csv files and stores the data in a itkCSVArray2DDataObject.
 *
 * CSVArray2DFileReader is used for reading csv files. This reader should
 * only be used for parsing numeric data fields. The Parse() method in this
 * class parses the data from the file into a itkCSVArray2DDataObject. As the
 * Array2D object only holds one data type, all the data, other than the row
 * and column headers, will be cast to a single data type. Row and column
 * headers will be parsed into std vectors of strings.
 *
 * The user would need to specify the file name using the SetFileName() method.
 * The field delimiter character is set to ',' by default but other field
 * delimiter characters may be set using SetFieldDelimiterCharacter(). If there
 * are row and column headers in the file, the user must specify whether or not
 * they exist using the HasRowHeaders and HasColumnHeaders flags. Also, if row
 * and/or column headers are enclosed in "" or within other string delimiter
 * characters being used, the user can set the option of turning the string
 * delimiter character on with the UseStringDelimiterCharacter flag. This is
 * also especially useful if headers such as "This,Header" contain commas or
 * other field delimiter characters within them.
 *
 * If the csv file has row and column headers, they do not necessarily need to
 * be enclosed in "" or other string delimiter characters as long as you
 * specify that row and/or column headers exist in the file. Turning on
 * HasColumnHeaders flags will tell the reader that the first line contains
 * column headers and turning on HasRowHeaders will tell the reader to
 * recognize the first field in all subsequent lines as row headers. After all
 * the inputs have been set, the user then calls the Parse() method. The user
 * can then get the results by instantiating a itkCSVArray2DDataObject and
 * assigning it using the GetOutput() method.
 *
 * Below is an example of how this class can be used to read and parse the data
 * from an input file:
 *
 * typedef itk::CSVArray2DFileReader<double> ReaderType;
 * ReaderType::Pointer reader = ReaderType::New();
 *
 * std::string filename = "NameOfFile.csv";
 * reader->SetFileName( filename );
 * reader->SetFieldDelimiterCharacter( ',' );
 * reader->SetStringDelimiterCharacter( '"' );
 * reader->HasColumnHeadersOn();
 * reader->HasRowHeadersOn();
 * reader->UseStringDelimiterCharacterOn();
 *
 * reader->Parse();
 *
 * \ingroup ITKIOCSV
 */
template <typename TData>
class ITK_TEMPLATE_EXPORT CSVArray2DFileReader:public CSVFileReaderBase
{
public:
  /** Standard class typedefs */
  typedef CSVArray2DFileReader      Self;
  typedef CSVFileReaderBase         Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CSVArray2DFileReader, CSVFileReaderBase);

  /** DataFrame Object types */
  typedef typename itk::CSVArray2DDataObject<TData>    Array2DDataObjectType;
  typedef typename Array2DDataObjectType::Pointer      Array2DDataObjectPointer;

  /** The value type of the dataset. */
  typedef TData ValueType;

  /** This method can be used to get the data frame object once the data from
  * the file has been parsed. */
  itkGetModifiableObjectMacro(Array2DDataObject, Array2DDataObjectType);

  /** Parses the data from the file. Gets all the fields and parses row and
  * column headers, if any, into std::vectors of strings and parses the numeric
  * data into an Array2D object. The vectors and the Array2D object are part of
  * the Array2DDataFrameobject. */
  virtual void Parse() ITK_OVERRIDE;

  /** Aliased to the Parse() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update();

  /** Aliased to the GetDataFrameObject() method to be consistent with the
   *  rest of the pipeline */
  virtual Array2DDataObjectPointer GetOutput();

protected:

  CSVArray2DFileReader();
  virtual ~CSVArray2DFileReader () ITK_OVERRIDE {}

  /** Print the reader. */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  Array2DDataObjectPointer   m_Array2DDataObject;

  ITK_DISALLOW_COPY_AND_ASSIGN(CSVArray2DFileReader);
};

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCSVArray2DFileReader.hxx"
#endif

#endif
