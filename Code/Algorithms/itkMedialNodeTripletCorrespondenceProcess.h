/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMedialNodeTripletCorrespondenceProcess.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMedialNodeTripletCorrespondenceProcess_h
#define __itkMedialNodeTripletCorrespondenceProcess_h

#include "itkImage.h"
#include "itkProcessObject.h"
#include "itkBloxCoreAtomImage.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkMatrixResizeableDataObject.h"

#include "itkPNGImageIO.h"
#include "itkImageFileWriter.h"
#include "itkRawImageIO.h"

#include "itkCorrespondenceDataStructure.h"
#include "itkCorrespondenceDataStructureIterator.h"

namespace itk
{
  
/** \class MedialNodeTripletCorrespondenceProcess
 * \brief This process takes as inputs two core atom images, a pair correspondence 
 * data structure for the two images, and the distance matrices of the two images 
 * in order to produce an itkCorrespondenceDataStructure containing correspondences 
 * between triplets (node cliques of size 3) in the images.
 *
 * \ingroup 
 */
template< typename TSourceImage >
class MedialNodeTripletCorrespondenceProcess : public ProcessObject
{
public:
 
  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Standard class typedefs. */
  typedef MedialNodeTripletCorrespondenceProcess  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MedialNodeTripletCorrespondenceProcess, ProcessObject);

  /** Typedef for core atom image. */
  typedef TSourceImage CoreAtomImageType;
  typedef typename CoreAtomImageType::Pointer CoreAtomImagePointer;
  typedef typename CoreAtomImageType::RegionType CoreAtomImageRegionType; 
  typedef typename CoreAtomImageType::PixelType CoreAtomImagePixelType; 
  typedef typename CoreAtomImageType::ConstPointer CoreAtomImageConstPointer;

  /** Typedef for pair correspondence data structure. (input) */
  typedef CorrespondingMedialNodeClique<itkGetStaticConstMacro(NDimensions),2> InputNodeType;
  typedef CorrespondenceDataStructure<InputNodeType,2> InputDataStructureType;
  typedef typename CorrespondenceDataStructure<InputNodeType,2>::Pointer InputDataStructurePointerType;
  typedef CorrespondenceDataStructureIterator<InputDataStructureType> InputIteratorType;

  /** Typedef for triplet correspondence data structure. (output) */
  typedef CorrespondingMedialNodeClique<itkGetStaticConstMacro(NDimensions),3> OutputNodeType;
  typedef CorrespondenceDataStructure<OutputNodeType,3> OutputDataStructureType;
  typedef typename CorrespondenceDataStructure<OutputNodeType,3>::Pointer OutputDataStructurePointerType;
  typedef CorrespondenceDataStructureIterator<OutputDataStructureType> OutputIteratorType;

  /** Typedef for distance matrix. */
  typedef MatrixResizeableDataObject<double> DistanceMatrixType;
  typedef typename DistanceMatrixType::Pointer DistanceMatrixPointer;

  /** Get the image output of this process object.  */
  OutputDataStructureType * GetOutput(void);
  OutputDataStructureType * GetOutput(unsigned int idx);

  /** Set the input pair data structure. */
  void SetPairDataStructure(const InputDataStructureType * InputDataStructure );
  
  /** Set the first core atom image. */
  void SetCoreAtomImageA( const CoreAtomImageType * CoreAtomImageA );

  /** Set the second core atom image. */
  void SetCoreAtomImageB( const CoreAtomImageType * CoreAtomImageB );

  /** Set the first distance matrix. */
  void SetDistanceMatrixA( const DistanceMatrixType * DistanceMatrixA );

  /** Set the second distance matrix. */
  void SetDistanceMatrixB( const DistanceMatrixType * DistanceMatrixB );

  /** Update() to allow for pipeline use. */
  virtual void Update() {this->GenerateData();}

  virtual DataObjectPointer MakeOutput(unsigned int idx);

  /** Functions to get the number of triplets and the number of represented by the data structure. */
  int GetNumberOfNodeTriplets() {return m_NumberOfTriplets;}
  int GetNumberOfNodeTripletBases() {return m_NumberOfNodeBaseTriplets;}

protected:

  MedialNodeTripletCorrespondenceProcess();
  virtual ~MedialNodeTripletCorrespondenceProcess(){} 

  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for forming the DistanceMatrix. */
  void GenerateData();

  /** Methods to get core atom images. */
  TSourceImage * GetCoreAtomImageA();
  TSourceImage * GetCoreAtomImageB();

  /** Methods to get distance matrices. */
  DistanceMatrixType * GetDistanceMatrixA();
  DistanceMatrixType * GetDistanceMatrixB();

private:
  MedialNodeTripletCorrespondenceProcess(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CoreAtomImagePointer m_CoreAtomImageA;
  CoreAtomImagePointer m_CoreAtomImageB;

  DistanceMatrixPointer m_DistanceMatrixA;
  DistanceMatrixPointer m_DistanceMatrixB;

  InputDataStructurePointerType m_InputDataStructure;
  OutputDataStructurePointerType m_OutputDataStructure;

  int m_NumberOfTriplets;
  int m_NumberOfNodeBaseTriplets;

  bool m_CreateOutputFile;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMedialNodeTripletCorrespondenceProcess.txx"
#endif

#endif
