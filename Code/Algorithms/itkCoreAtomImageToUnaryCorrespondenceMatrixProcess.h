/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCoreAtomImageToUnaryCorrespondenceMatrixProcess.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCoreAtomImageToUnaryCorrespondenceMatrixProcess_h
#define __itkCoreAtomImageToUnaryCorrespondenceMatrixProcess_h

#include "itkImage.h"
#include "itkProcessObject.h"
#include "itkDataObject.h"
#include "vnl/vnl_matrix.h"
#include "itkBloxCoreAtomImage.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkMatrixResizeableDataObject.h"
#include "itkUnaryMedialNodeMetric.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIO.h"

namespace itk
{
  
/** \class CoreAtomImageToUnaryCorrespondenceMatrixProcess
 * \brief 
 * 
 * This process takes in two itkBloxCoreAtomImages and runs the itkUnaryMedialNodeMetric on them.  
 * It returns a unary correspondence matrix for the images in the form of an itkMatrixResizeableDataObject.
 *
 * \ingroup 
 */
template< typename TSourceImage >
class CoreAtomImageToUnaryCorrespondenceMatrixProcess : public ProcessObject
{
public:
  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Standard class typedefs. */
  typedef CoreAtomImageToUnaryCorrespondenceMatrixProcess  Self;
  typedef ProcessObject Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CoreAtomImageToUnaryCorrespondenceMatrixProcess, ProcessObject);

  /** GetMacros for number of rows and columns in the unary correspondence matrix. */
  itkGetMacro(Rows, int);
  itkGetMacro(Columns, int);

  /** Typedef for core atom image. */
  typedef TSourceImage CoreAtomImageType;
  typedef typename CoreAtomImageType::Pointer CoreAtomImagePointer;
  typedef typename CoreAtomImageType::RegionType CoreAtomImageRegionType; 
  typedef typename CoreAtomImageType::PixelType CoreAtomImagePixelType; 
  typedef typename CoreAtomImageType::ConstPointer CoreAtomImageConstPointer;

  /** Typedef for correspondence matrix. */
  typedef MatrixResizeableDataObject<double> CorrespondenceMatrixType;
  typedef typename CorrespondenceMatrixType::Pointer CorrespondenceMatrixPointer;

  /** MedialNode typedef. */
  typedef BloxCoreAtomPixel<itkGetStaticConstMacro(NDimensions)> MedialNodeType;

  /** UnaryMetric typedef. */
  typedef UnaryMedialNodeMetric<itkGetStaticConstMacro(NDimensions)> UnaryMetricType;
  typedef typename UnaryMedialNodeMetric<itkGetStaticConstMacro(NDimensions)>::Pointer UnaryMetricPointer;

  /** Get the image output of this process object. */
  CorrespondenceMatrixType * GetOutput(void);
  CorrespondenceMatrixType * GetOutput(unsigned int idx);

  /** Set the blurred original image. */
  void SetInput1( const CoreAtomImageType * CoreAtomImageA );

  /** Set the boundary point image. */
  void SetInput2( const CoreAtomImageType * CoreAtomImageB );

  /** Update() to allow for pipeline use. */
  virtual void Update() {this->GenerateData();}

  virtual DataObjectPointer MakeOutput(unsigned int idx);

protected:
  CoreAtomImageToUnaryCorrespondenceMatrixProcess();
  virtual ~CoreAtomImageToUnaryCorrespondenceMatrixProcess(){}

  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for forming the CorrespondenceMatrix. */
  void GenerateData();

  /** Methods to get input images. */
  TSourceImage * GetInput1();
  TSourceImage * GetInput2();

private:
  CoreAtomImageToUnaryCorrespondenceMatrixProcess(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CoreAtomImagePointer m_CoreAtomImageA;
  CoreAtomImagePointer m_CoreAtomImageB;

  int m_Rows;
  int m_Columns;

  CorrespondenceMatrixPointer m_CorrespondenceMatrix;

  UnaryMetricPointer m_Metric;

  /** Flag to output test png files. Off by default. */
  bool m_OutputPNG;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCoreAtomImageToUnaryCorrespondenceMatrixProcess.txx"
#endif

#endif
