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
#ifndef __itkCoreAtomImageToDistanceMatrixProcess_h
#define __itkCoreAtomImageToDistanceMatrixProcess_h

#include "itkBloxCoreAtomImage.h"
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
 * \ingroup ITK-Blox
 */
template< typename TSourceImage >
class CoreAtomImageToDistanceMatrixProcess:public ProcessObject
{
public:
  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Standard class typedefs */
  typedef CoreAtomImageToDistanceMatrixProcess Self;
  typedef ProcessObject                        Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Smart Pointer type to a DataObject. */
  typedef DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CoreAtomImageToDistanceMatrixProcess, ProcessObject);

  //Get macro for m_NumNodes
  itkGetConstMacro(NumberOfNodes, int);

  /** Typedef for core atom image */
  typedef TSourceImage                             CoreAtomImageType;
  typedef typename CoreAtomImageType::Pointer      CoreAtomImagePointer;
  typedef typename CoreAtomImageType::RegionType   CoreAtomImageRegionType;
  typedef typename CoreAtomImageType::PixelType    CoreAtomImagePixelType;
  typedef typename CoreAtomImageType::ConstPointer CoreAtomImageConstPointer;

  /** Typedef for distance matrix */
  typedef MatrixResizeableDataObject< double > DistanceMatrixType;
  typedef typename DistanceMatrixType::Pointer DistanceMatrixPointer;

  //MedialNode typedef
  typedef BloxCoreAtomPixel< itkGetStaticConstMacro(NDimensions) > MedialNodeType;

  /** The type used to store the position of the BloxPixel. */
  typedef Point< double, itkGetStaticConstMacro(NDimensions) > PositionType;

  /** Get the image output of this process object.  */
  DistanceMatrixType * GetOutput(void);

  DistanceMatrixType * GetOutput(unsigned int idx);

  /** Set the blurred original image */
  void SetInput1(const CoreAtomImageType *CoreAtomImageA);

  virtual void Update() { this->GenerateData(); }

  virtual DataObjectPointer MakeOutput(unsigned int idx);

protected:
  CoreAtomImageToDistanceMatrixProcess();
  virtual ~CoreAtomImageToDistanceMatrixProcess(){}

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Method for forming the DistanceeMatrix */
  void GenerateData();

  /** Methods to get input image */
  TSourceImage * GetInput1();

private:
  CoreAtomImageToDistanceMatrixProcess(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not

  // implemented

  CoreAtomImagePointer  m_CoreAtomImage;
  DistanceMatrixPointer m_DistanceMatrix;

  int m_NumberOfNodes;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCoreAtomImageToDistanceMatrixProcess.txx"
#endif

#endif
