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
#ifndef __itkMedialNodePairCorrespondenceProcess_h
#define __itkMedialNodePairCorrespondenceProcess_h

#include "itkImage.h"
#include "itkProcessObject.h"
#include "itkBloxCoreAtomImage.h"
#include "itkMatrixResizeableDataObject.h"
#include "itkBinaryMedialNodeMetric.h"

#include "itkCorrespondenceDataStructure.h"
#include "itkCorrespondingMedialNodeClique.h"
#include "itkCorrespondenceDataStructureIterator.h"

namespace itk
{
/** \class MedialNodePairCorrespondenceProcess
 * \brief This process takes as inputs two core atom images, the distance matrices
 * of the two images, and the unary correspondence matrix between the two images
 * in order to produce an CorrespondenceDataStructure containing
 * correspondences between pairs (node cliques of size 2) in the images.
 * \ingroup ITK-Blox
 */
template< typename TSourceImage >
class ITK_EXPORT MedialNodePairCorrespondenceProcess:public ProcessObject
{
public:
  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Standard class typedefs */
  typedef MedialNodePairCorrespondenceProcess Self;
  typedef ProcessObject                       Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information. (and related methods) */
  itkTypeMacro(MedialNodePairCorrespondenceProcess, ProcessObject);

  /** Typedef for core atom image. */
  typedef TSourceImage                             CoreAtomImageType;
  typedef typename CoreAtomImageType::Pointer      CoreAtomImagePointer;
  typedef typename CoreAtomImageType::RegionType   CoreAtomImageRegionType;
  typedef typename CoreAtomImageType::PixelType    CoreAtomImagePixelType;
  typedef typename CoreAtomImageType::ConstPointer CoreAtomImageConstPointer;

  /** Typedef for distance matrix. */
  typedef MatrixResizeableDataObject< double > DistanceMatrixType;
  typedef typename DistanceMatrixType::Pointer DistanceMatrixPointer;

  /** Typedef for correspondence matrix */
  typedef MatrixResizeableDataObject< double >       CorrespondenceMatrixType;
  typedef typename CorrespondenceMatrixType::Pointer CorrespondenceMatrixPointer;

  /** Typedef for correspondence data structure. (output) */
  typedef CorrespondingMedialNodeClique< itkGetStaticConstMacro(NDimensions), 2 > NodeType;
  typedef CorrespondenceDataStructure< NodeType, 2 >                              DataStructureType;
  typedef typename CorrespondenceDataStructure< NodeType, 2 >::Pointer            DataStructurePointerType;
  typedef CorrespondenceDataStructureIterator< DataStructureType >                IteratorType;

  /** Typedef for binary node metric. */
  typedef BinaryMedialNodeMetric< itkGetStaticConstMacro(NDimensions) >                   BinaryMetricType;
  typedef typename BinaryMedialNodeMetric< itkGetStaticConstMacro(NDimensions) >::Pointer BinaryMetricPointer;

  /** Typedef for medial nodes. */
  typedef BloxCoreAtomPixel< itkGetStaticConstMacro(NDimensions) > MedialNodeType;

  /** The type used to store the position of the BloxPixel. */
  typedef Point< double, itkGetStaticConstMacro(NDimensions) > PositionType;

  /** Get the image output of this process object.  */
  DataStructureType * GetOutput(void);

  DataStructureType * GetOutput(unsigned int idx);

  /* This is important to note...the inputs for this process are somewhat
     complicated, and there are a lot of them.  You need all 5 inputs for
     this process to perform its task */

  /** Set the first core atom image. */
  void SetCoreAtomImageA(const CoreAtomImageType *CoreAtomImageA);

  /** Set the second core atom image. */
  void SetCoreAtomImageB(const CoreAtomImageType *CoreAtomImageB);

  /** Set the first distance matrix. */
  void SetDistanceMatrixA(const DistanceMatrixType *DistanceMatrixA);

  /** Set the second distance matrix. */
  void SetDistanceMatrixB(const DistanceMatrixType *DistanceMatrixB);

  /** Get number of node pairs. */
  int GetNumberOfNodePairs() { return m_NumberOfNodePairs; }

  /** Get number of node basepairs. */
  int GetNumberOfNodeBasePairs() { return m_NumberOfNodeBasePairs; }

  /** Set the correspondence matrix. */
  void SetCorrespondenceMatrix(const CorrespondenceMatrixType *CorrespondenceMatrix);

  virtual void Update() { this->GenerateData(); }

  virtual DataObjectPointer MakeOutput(unsigned int idx);

protected:
  MedialNodePairCorrespondenceProcess();
  virtual ~MedialNodePairCorrespondenceProcess(){}

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Method for forming the Pair Correspondence Data Structure. */
  void GenerateData();

  /** Methods to get core atom images. */
  TSourceImage * GetCoreAtomImageA();

  TSourceImage * GetCoreAtomImageB();

  /** Methods to get distance matrices. */
  DistanceMatrixType * GetDistanceMatrixA();

  DistanceMatrixType * GetDistanceMatrixB();

  /** Method to get correspondence matrix. */
  CorrespondenceMatrixType * GetCorrespondenceMatrix();

private:
  MedialNodePairCorrespondenceProcess(const Self &); //purposely not implemented
  void operator=(const Self &);                      //purposely not implemented

  CoreAtomImagePointer m_CoreAtomImageA;
  CoreAtomImagePointer m_CoreAtomImageB;

  DistanceMatrixPointer m_DistanceMatrixA;
  DistanceMatrixPointer m_DistanceMatrixB;

  CorrespondenceMatrixPointer m_CorrespondenceMatrix;

  DataStructurePointerType m_DataStructure;

  BinaryMetricPointer m_BinaryMetric;

  int m_Rows;
  int m_Columns;

  int m_NumberOfNodePairs;
  int m_NumberOfNodeBasePairs;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMedialNodePairCorrespondenceProcess.txx"
#endif

#endif
