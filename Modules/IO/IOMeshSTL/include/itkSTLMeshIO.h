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
#ifndef __itkSTLMeshIO_h
#define __itkSTLMeshIO_h

#include "itkMeshIOBase.h"

#include <fstream>

namespace itk
{
/** \class STLMeshIO
 * \brief This class defines how to read and write STL file format.
 *
 * \author Luis Ibanez, Kitware Inc.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 */
class ITKIOMesh_EXPORT STLMeshIO : public MeshIOBase
{
public:
  /** Standard "Self" typedef. */
  typedef STLMeshIO                Self;
  typedef MeshIOBase               Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(STLMeshIO, MeshIOBase);

  /**-------- This part of the interfaces deals with reading data. ----- */

  /** Determine if the file can be read with this MeshIO implementation.
   * \param FileNameToRead The name of the file to test for reading.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can read the file specified.
   */
  virtual bool
  CanReadFile(const char * FileNameToRead);

  /** Read the content of the file into a Mesh. */
  virtual void
  Read();

  /** Set the spacing and dimension information for the set filename. */
  virtual void
  ReadMeshInformation();

  /** Reads the data from disk into the memory buffer provided. */
  virtual void
  ReadPoints(void * buffer);

  virtual void
  ReadCells(void * buffer);

  /** STL files do not carry information in points or cells.
   * Therefore the following two methods are implemented as null
   * operations. */
  virtual void
  ReadPointData(void * itkNotUsed(buffer)) {};
  virtual void
  ReadCellData(void * itkNotUsed(buffer)) {};

  /*-------- This part of the interfaces deals with writing data. ----- */
  /** Determine if the file can be written with this MeshIO implementation.
   * \param FileNameToWrite The name of the file to test for writing.
   * \post Sets classes MeshIOBase::m_FileName variable to be FileNameToWrite
   * \return Returns true if this MeshIO can write the file specified.
   */
  virtual bool
  CanWriteFile(const char * FileNameToWrite);

  /** Write header of the STL file */
  virtual void
  WriteMeshInformation();

  /** Write the content of the Mesh into an STL file. */
  virtual void
  Write();

  /** The STL format stores point coordinates repeatedly as part of every
   * triangle. Therefore point coordinates are writen as part of the
   * WriteCells() method, and not as an independent operation.
   * Consequently, this method only performs an internal copy of the Point
   * coordinates data, than then is used in the WriteCells() method.
   */
  virtual void
  WritePoints(void * buffer);

  /** The WriteCells() method does most of the work. It writes
   * out every triangle in the mesh. For every triangle, it
   * writes out its normal, followed by the coordinates of its
   * three vertices.
   *
   * A typical cell looks as follows in an ASCII STL file
   *
   *        facet normal 0.357406 0.862856 0.357406
   *         outer loop
   *          vertex 0 1 0
   *          vertex 0 0.707107 0.707107
   *          vertex 0.707107 0.707107 0
   *         endloop
   *        endfacet
   *
   */
  virtual void
  WriteCells(void * buffer);

  /** STL files do not carry information in points or cells.
   * Therefore the following two methods are implemented as null
   * operations. */
  virtual void
  WritePointData(void * itkNotUsed(buffer)) {};
  virtual void
  WriteCellData(void * itkNotUsed(buffer)) {};


protected:
  STLMeshIO();
  virtual ~STLMeshIO() {}

  void
  PrintSelf(std::ostream & os, Indent indent) const;

private:
  STLMeshIO(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  std::ofstream m_OutputStream; // output file
  std::ifstream m_InputStream;  // input file

  typedef float PointValueType; // type to represent point coordinates

  typedef Point<PointValueType, 3>           PointType;
  typedef Vector<PointValueType, 3>          VectorType;
  typedef CovariantVector<PointValueType, 3> NormalType;

  typedef std::vector<PointType> PointContainerType;

  PointContainerType m_Points;
};
} // end namespace itk

#endif // __itkSTLMeshIO_h
