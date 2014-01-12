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
#include <set>

namespace itk
{
/** \class STLMeshIO
 * \brief This class defines how to read and write STL file format.
 *
 * \author Luis Ibanez, Kitware Inc.
 * \ingroup IOFilters
 * \ingroup ITKIOMesh
 * \ingroup ITKIOSTLMesh
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

  /** Stores the point data into the memory buffer provided. */
  virtual void
  ReadPoints(void * buffer);

  /** Stores the cell data into the memory buffer provided. */
  virtual void
  ReadCells(void * buffer);

  /** Indicates whether ReadPoints() should be called. */
  virtual bool
  GetUpdatePoints() const;

  /** Indicates whether ReadCells() should be called. */
  virtual bool
  GetUpdateCells() const;

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

  /** Templated version of write points method, that is aware of the specific
   * type used to represent the point coordinates. */
  template <typename PointValueType>
  void
  WritePointsTyped(const PointValueType * buffer)
  {

    const unsigned int pointDimension = this->GetPointDimension();

    if (pointDimension != 3)
    {
      itkExceptionMacro("STL only supports 3D points");
    }

    const PointValueType * pointCoordinates = buffer;

    this->m_Points.clear();

    const IdentifierType numberOfPoints = this->GetNumberOfPoints();

    this->m_Points.resize(numberOfPoints);

    for (IdentifierType pi = 0; pi < numberOfPoints; ++pi)
    {
      for (unsigned int i = 0; i < pointDimension; ++i)
      {
        m_Points[pi][i] = *pointCoordinates++;
      }
    }
  }


private:
  STLMeshIO(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  std::ofstream m_OutputStream; // output file
  std::ifstream m_InputStream;  // input file

  std::string m_InputLine; // helper during reading

  typedef float PointValueType; // type to represent point coordinates

  typedef Point<PointValueType, 3>           PointType;
  typedef Vector<PointValueType, 3>          VectorType;
  typedef CovariantVector<PointValueType, 3> NormalType;

  typedef std::vector<PointType> PointContainerType;

  /** Helper functions to write elements to binary file */
  void
  WriteInt32AsBinary(int32_t value);
  void
  WriteInt16AsBinary(int16_t value);
  void
  WriteNormalAsBinary(const NormalType & normal);
  void
  WritePointAsBinary(const PointType & point);

  /** Helper functions to read elements from ASCII and BINARY files. */
  void
  ReadMeshInternalFromAscii();
  void
  ReadMeshInternalFromBinary();

  /** Helper functions to read elements from binary file */
  void
  ReadInt32AsBinary(int32_t & value);
  void
  ReadInt16AsBinary(int16_t & value);
  void
  ReadNormalAsBinary(NormalType & normal);
  void
  ReadPointAsBinary(PointType & point);

  /** Helper functions to read elements from ASCII files. */
  void
  ReadStringFromAscii(const std::string & keyword);
  void
  ReadPointAsAscii(PointType & point);
  bool
  CheckStringFromAscii(const std::string & expected);

  /** Functions to create set of points and disambiguate them. */
  void
  InsertPointIntoSet(const PointType & point);

  PointContainerType m_Points;

  unsigned int m_InputLineNumber;

  struct PointCompare
  {
    bool
    operator()(const PointType & p1, const PointType & p2) const
    {
      if (p1[0] != p2[0])
      {
        return p1[0] < p2[0];
      }
      else
      {
        if (p1[1] != p2[1])
        {
          return p1[1] < p2[1];
        }
      }

      return p1[2] < p2[2];
    }
  };

  typedef std::map<PointType, IdentifierType, PointCompare> PointsMapType;

  PointsMapType m_PointsMap;

  // Helper variable to put Ids in points as they are read
  IdentifierType m_LatestPointId;

  // Triplet to hold the Ids of points in a triagle as they are being read
  class TripletType
  {
  public:
    IdentifierType p0;
    IdentifierType p1;
    IdentifierType p2;
  };

  TripletType  m_TrianglePointIds;
  unsigned int m_PointInTriangleCounter;

  typedef std::vector<TripletType> CellsVectorType;
  CellsVectorType                  m_CellsVector;
};
} // end namespace itk

#endif // __itkSTLMeshIO_h
