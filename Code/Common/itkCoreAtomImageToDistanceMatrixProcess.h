/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoreAtomImageToDistanceMatrixProcess.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCoreAtomImageToDistanceMatrixProcess_h
#define __itkCoreAtomImageToDistanceMatrixProcess_h

#include "itkBloxCoreAtomImage.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkDataObject.h"
#include "itkImage.h"
#include "itkMatrixResizeableDataObject.h"
#include "itkProcessObject.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class CoreAtomImageToDistanceMatrixProcess
 * \brief Computes the distance between all medial
 * nodes (voted core atoms) in a core atom image (input) and stores
 * them in a matrix data object (output)
 *
 */
template< typename TSourceImage >
class CoreAtomImageToDistanceMatrixProcess : public ProcessObject
{
public:
  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Standard class typedefs */
  typedef CoreAtomImageToDistanceMatrixProcess  Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(itkCoreAtomImageToDistanceMatrixProcess, ProcessObject);

  //Get macro for m_NumNodes
  itkGetMacro(NumberOfNodes, int);

  /** Typedef for core atom image */
  typedef TSourceImage CoreAtomImageType;
  typedef typename CoreAtomImageType::Pointer CoreAtomImagePointer;
  typedef typename CoreAtomImageType::RegionType CoreAtomImageRegionType; 
  typedef typename CoreAtomImageType::PixelType CoreAtomImagePixelType; 
  typedef typename CoreAtomImageType::ConstPointer CoreAtomImageConstPointer;

  /** Typedef for distance matrix */
  typedef MatrixResizeableDataObject<double> DistanceMatrixType;
  typedef typename DistanceMatrixType::Pointer DistanceMatrixPointer;

  //MedialNode typedef
  typedef BloxCoreAtomPixel<NDimensions> MedialNodeType;

  /** The type used to store the position of the BloxPixel. */
  typedef Point<double, NDimensions> PositionType;

  /** Get the image output of this process object.  */
  DistanceMatrixType * GetOutput(void);
  DistanceMatrixType * GetOutput(unsigned int idx);

  /** Set the blurred original image */
  void SetInput1( const CoreAtomImageType * CoreAtomImageA );

  virtual void Update() {this->GenerateData();}

  virtual DataObjectPointer MakeOutput(unsigned int idx);

protected:
  CoreAtomImageToDistanceMatrixProcess();
  virtual ~CoreAtomImageToDistanceMatrixProcess(){} 

  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for forming the DistanceeMatrix */
  void GenerateData();

  /** Methods to get input image */
  TSourceImage * GetInput1();

private:
  CoreAtomImageToDistanceMatrixProcess(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CoreAtomImagePointer m_CoreAtomImage;
  DistanceMatrixPointer m_DistanceMatrix;

  int m_NumberOfNodes;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCoreAtomImageToDistanceMatrixProcess.txx"
#endif

#endif
