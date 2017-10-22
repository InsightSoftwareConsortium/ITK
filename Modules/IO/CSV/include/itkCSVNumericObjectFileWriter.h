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

#ifndef itkCSVNumericObjectFileWriter_h
#define itkCSVNumericObjectFileWriter_h

#include "itkLightProcessObject.h"
#include "itkMacro.h"
#include "itkArray2D.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkMatrix.h"
#include <vector>
#include "itkSize.h"

namespace itk
{
/** \class CSVNumericObjectFileWriter
 * \brief Writes out numeric itk data objects to a csv file.
 *
 * CSVNumericObjectFileWriter writes numeric data from an itk object into a
 * csv file. It is templated over the type of data being written, the number
 * of rows and the number of columns. The writer uses the datablock member from
 * vnl_matrix or vnl_matrix_fixed. As of now the objects itkMatrix, itkArray2D,
 * and any vnl_matrix based objects are supported.
 *
 * The user specifies the file name and  the field delimiter character using
 * the Set macro method for each. The user can also write out row and column
 * headers. The methods ColumnHeadersPushBack() and RowHeadersPushBack() can be
 * used to push strings into the vectors for row and column headers. The row
 * and column headers can also be set using the Set macro methods for each if
 * vectors for the headers are already set. The SetInput() method is overloaded
 * to allow various itk objects to be set as input.
 *
 * The method Write() is used to output the object data to a csv file.
 *
 * The writer will output a warning if the number of row headers is not
 * consistent with the number of rows in the input object or if the number of
 * column headers is not consistent with the number of columns in the input
 * object. It is up to the user to check this.
 *
 *
 * \ingroup ITKIOCSV
 */
template <typename TValue, unsigned int NRows = 0, unsigned int NColumns = 0>
class ITK_TEMPLATE_EXPORT CSVNumericObjectFileWriter:public LightProcessObject
{
public:
  /** Standard class typedefs */
  typedef CSVNumericObjectFileWriter    Self;
  typedef LightProcessObject            Superclass;
  typedef SmartPointer <Self>           Pointer;
  typedef SmartPointer <const Self>     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CSVNumericObjectFileWriter, LightProcessObject);

  // Matrix types
  typedef vnl_matrix<TValue>                        vnlMatrixType;
  typedef vnl_matrix_fixed<TValue, NRows, NColumns> vnlFixedMatrixType;
  typedef itk::Matrix<TValue,NRows,NColumns>        itkMatrixType;

  typedef std::vector<std::string> StringVectorType;

  typedef itk::Size<2>::SizeValueType SizeValueType;

  /* Specify the name of the output file */
  itkSetStringMacro(FileName);
  itkSetMacro(FieldDelimiterCharacter,char);

  /** Set the input object if the matrix is of vnl_matrix type or Array2D. */
  void SetInput(const vnlMatrixType* obj);

  /** Set the input object if the matrix is of vnl_matrix_fixed type. */
  void SetInput(const vnlFixedMatrixType* obj);

  /** Set the input object if the matrix is of itkMatrixType. */
  void SetInput(const itkMatrixType* obj);

  void ColumnHeadersPushBack(const std::string & );
  void RowHeadersPushBack(const std::string & );
  void SetColumnHeaders(const StringVectorType & columnheaders);
  void SetRowHeaders(const StringVectorType & rowheaders);

  /* Checks that all essential components are plugged in */
  void PrepareForWriting();

  /** Write out the object */
  virtual void Write();

  /** Aliased to the Write() method to be consistent with the rest of the
   * pipeline. */
  virtual void Update();

protected:

  CSVNumericObjectFileWriter();
  virtual ~CSVNumericObjectFileWriter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream &os, Indent indent) const ITK_OVERRIDE;

private:
  std::string               m_FileName;
  TValue                   *m_InputObject;
  char                      m_FieldDelimiterCharacter;
  SizeValueType             m_Rows;
  SizeValueType             m_Columns;
  StringVectorType          m_ColumnHeaders;
  StringVectorType          m_RowHeaders;

  ITK_DISALLOW_COPY_AND_ASSIGN(CSVNumericObjectFileWriter);
};

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCSVNumericObjectFileWriter.hxx"
#endif

#endif
