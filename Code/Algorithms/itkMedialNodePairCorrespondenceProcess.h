/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedialNodePairCorrespondenceProcess.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMedialNodePairCorrespondenceProcess_h
#define __itkMedialNodePairCorrespondenceProcess_h

#include "itkImage.h"
#include "itkProcessObject.h"
#include "itkDataObject.h"
#include "itkBloxCoreAtomImage.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkMatrixResizeableDataObject.h"
#include "itkBinaryMedialNodeMetric.h"

#include "itkPNGImageIO.h"
#include "itkImageFileWriter.h"
#include "itkRawImageIO.h"

#include "itkCorrespondenceDataStructure.h"
#include "itkSecondaryNodeList.h"
#include "itkCorrespondingList.h"
#include "itkCorrespondingMedialNodeClique.h"
#include "itkCorrespondenceDataStructureIterator.h"

namespace itk
{
  
/** \class MedialNodePairCorrespondenceProcess
 * \brief This process takes as inputs two core atom images, the distance matrices 
 * of the two images, and the unary correspondence matrix between the two images 
 * in order to produce an itkCorrespondenceDataStructure containing 
 * correspondences between pairs (node cliques of size 2) in the images.
 *
 * \ingroup 
 */
template< typename TSourceImage >
class MedialNodePairCorrespondenceProcess : public ProcessObject
{
public:
  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Standard class typedefs */
  typedef MedialNodePairCorrespondenceProcess  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information. (and related methods) */
  itkTypeMacro(MedialNodePairCorrespondenceProcess, ProcessObject);

  /** Typedef for core atom image. */
  typedef TSourceImage CoreAtomImageType;
  typedef typename CoreAtomImageType::Pointer CoreAtomImagePointer;
  typedef typename CoreAtomImageType::RegionType CoreAtomImageRegionType; 
  typedef typename CoreAtomImageType::PixelType CoreAtomImagePixelType; 
  typedef typename CoreAtomImageType::ConstPointer CoreAtomImageConstPointer;

  /** Typedef for distance matrix. */
  typedef MatrixResizeableDataObject<double> DistanceMatrixType;
  typedef typename DistanceMatrixType::Pointer DistanceMatrixPointer;

  /** Typedef for correspondence matrix */
  typedef MatrixResizeableDataObject<double> CorrespondenceMatrixType;
  typedef typename CorrespondenceMatrixType::Pointer CorrespondenceMatrixPointer;

  /** Typedef for correspondence data structure. (output) */
  typedef CorrespondingMedialNodeClique<NDimensions,2> NodeType;
  typedef CorrespondenceDataStructure<NodeType,2> DataStructureType;
  typedef typename CorrespondenceDataStructure<NodeType,2>::Pointer DataStructurePointerType;
  typedef CorrespondenceDataStructureIterator<DataStructureType> IteratorType;

  /** Typedef for binary node metric. */
  typedef BinaryMedialNodeMetric<NDimensions> BinaryMetricType;
  typedef typename BinaryMedialNodeMetric<NDimensions>::Pointer BinaryMetricPointer;

  /** Typedef for medial nodes. */
  typedef BloxCoreAtomPixel<NDimensions> MedialNodeType;

  /** The type used to store the position of the BloxPixel. */
  typedef Point<double, NDimensions> PositionType;

  /** Get the image output of this process object.  */
  DataStructureType * GetOutput(void);
  DataStructureType * GetOutput(unsigned int idx);

  /* This is important to note...the inputs for this process are somewhat 
     complicated, and there are a lot of them.  You need all 5 inputs for 
     this process to perform its task */

  /** Set the first core atom image. */
  void SetCoreAtomImageA( const CoreAtomImageType * CoreAtomImageA );

  /** Set the second core atom image. */
  void SetCoreAtomImageB( const CoreAtomImageType * CoreAtomImageB );

  /** Set the first distance matrix. */
  void SetDistanceMatrixA( const DistanceMatrixType * DistanceMatrixA );

  /** Set the second distance matrix. */
  void SetDistanceMatrixB( const DistanceMatrixType * DistanceMatrixB );

  /** Get number of node pairs. */
  int GetNumberOfNodePairs() {return m_NumberOfNodePairs;}  

  /** Get number of node basepairs. */
  int GetNumberOfNodeBasePairs() {return m_NumberOfNodeBasePairs;}  

  /** Set the correspondence matrix. */
  void SetCorrespondenceMatrix( const CorrespondenceMatrixType * CorrespondenceMatrix );

  virtual void Update() {this->GenerateData();}

  virtual DataObjectPointer MakeOutput(unsigned int idx);

protected:
  MedialNodePairCorrespondenceProcess();
  virtual ~MedialNodePairCorrespondenceProcess(){} 

  void PrintSelf(std::ostream& os, Indent indent) const;

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
  MedialNodePairCorrespondenceProcess(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

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

  /** Flag to output test png files. Off by default. */
  bool m_OutputPNG;

  /** Flag to output a text file detailing the creation o fthe pair data structure.  Be aware that
    * This file can become VERY large when working with moderate to large sets of medial nodes.  Off by default. 
    */
  bool m_CreateOutputFile;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMedialNodePairCorrespondenceProcess.txx"
#endif

#endif
