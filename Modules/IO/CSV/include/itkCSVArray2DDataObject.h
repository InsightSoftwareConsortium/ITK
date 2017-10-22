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

#ifndef itkCSVArray2DDataObject_h
#define itkCSVArray2DDataObject_h

#include "itkObjectFactory.h"
#include "itkDataObject.h"
#include "itkArray2D.h"
#include "itkMacro.h"
#include <vector>

namespace itk
{
/** \class CSVArray2DDataObject
 * \brief Stores parsed data from csv files.
 *
 * CSVArray2DDataObject contains methods for accessing data from the
 * Array2D object. It is used in the CSVFileToArray2DReader class for storing
 * parsed data. The CSVArray2DDataObject comprises of an Array2D object
 * and two std string vectors for storing row and column headers. Individual
 * rows, columns and data fields can be accessed either by the names of the
 * headers or by indices. There are also some mutator methods that can be used
 * to modify values in the Array2D object as well as PushBack functions for
 * pushing column and row headers into their respective vectors.
 *
 * \ingroup ITKIOCSV
 */

template <typename TData>
class ITK_TEMPLATE_EXPORT CSVArray2DDataObject:public DataObject
{
public:
  /* Standard class typedefs */
  typedef CSVArray2DDataObject        Self;
  typedef DataObject                  Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CSVArray2DDataObject, DataObject);

  /* Vector typedefs. */
  typedef typename std::vector<TData>              NumericVectorType;
  typedef typename std::vector<std::string>        StringVectorType;

  /** Typedef for the Array2D object. */
  typedef typename itk::Array2D<TData>  MatrixType;

  /** Set macros */
  itkSetMacro(HasColumnHeaders,bool);
  itkSetMacro(HasRowHeaders,bool);
  itkBooleanMacro(HasColumnHeaders);
  itkBooleanMacro(HasRowHeaders);

  /** Get macros for Column and Row headers. */
  itkGetConstMacro(HasColumnHeaders, bool);
  itkGetConstMacro(HasRowHeaders, bool);

  /** Get macro for the matrix. */
  itkGetMacro(Matrix,MatrixType);

  /** Returns the Column Headers. */
  StringVectorType GetColumnHeaders() const;

  /** Returns the Row Headers. */
  StringVectorType GetRowHeaders() const;

  /** Returns a Row index by name. */
  unsigned int GetRowIndexByName(const std::string &) const;

  /** Returns a Column index by name. */
  unsigned int GetColumnIndexByName(const std::string &) const;

  /** Returns a row. Input to the method is a row header string. */
  NumericVectorType GetRow(const std::string &) const;

  /** Returns a Row. Input to the method is a row index. */
  NumericVectorType GetRow(const unsigned int & ) const;

  /** Returns a Column. Input to the method is a column header string. */
  NumericVectorType GetColumn(const std::string & ) const;

  /** Get Column method. Input to the method is a column index. */
  NumericVectorType GetColumn(const unsigned int &) const;

  /** Method to access a data field from the Array2D object. Inputs are row and
  *  column header strings in that order. */
  TData GetData(const std::string &, const std::string &) const;

  /** Method to access a data field from the Array2D object. Inputs are row and
  *  column indices in that order. */
  TData GetData(const unsigned int &, const unsigned int &) const;

  /** Method to access a data field from a particular column. Inputs are the
  *  column header string and the row index. */
  TData GetColumnData(const std::string &, const unsigned int &) const;

  /** Method to access a data field from a particular row. Inputs are the row
  *  header string and the column index. */
  TData GetRowData(const std::string &, const unsigned int &) const;

  /** Method to access a data field from the Array2D object using the ()
  *  operator.Inputs are the row and column header strings in that order. */
  TData operator()(const std::string &, const std::string &) const;

  /** Method to access a data field from the Array2D object using the ()
  *  operator. Inputs are the row and column indices in that order. */
  TData operator()(const unsigned int &, const unsigned int &) const;

  /** Method to set the size of the Array2D object. */
  void SetMatrixSize(unsigned int, unsigned int);

  /** Method to fill the Array2D object with a value. */
  void FillMatrix(TData value);

  /** Method to set the Array2D object with data at particular row and column
  *  indices. */
  void SetMatrixData(unsigned int, unsigned int, TData);

  /** Method to add a row header to the vector of row headers. */
  void RowHeadersPushBack(const std::string &);

  /** Method to add a column header to the vector of column headers. */
  void ColumnHeadersPushBack(const std::string &);

  /** Method to erase the first column header if it is the name of the table. */
  void EraseFirstColumnHeader();

protected:

  CSVArray2DDataObject();
  virtual ~CSVArray2DDataObject() ITK_OVERRIDE {}
  /** Print method */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  MatrixType             m_Matrix;
  StringVectorType       m_ColumnHeaders;
  StringVectorType       m_RowHeaders;
  bool                   m_HasRowHeaders;
  bool                   m_HasColumnHeaders;

  ITK_DISALLOW_COPY_AND_ASSIGN(CSVArray2DDataObject);
};

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCSVArray2DDataObject.hxx"
#endif

#endif
