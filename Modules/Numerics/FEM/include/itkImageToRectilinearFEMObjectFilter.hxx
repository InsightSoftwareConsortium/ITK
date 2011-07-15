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
#ifndef __itkImageToRectilinearFEMObjectFilter_hxx
#define __itkImageToRectilinearFEMObjectFilter_hxx

#include "itkImageToRectilinearFEMObjectFilter.h"
#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "itkFEMElement3DC0LinearHexahedron.h"
#include <math.h>
namespace itk
{
namespace fem
{

/*
 * Default constructor for Filter
 */
template <class TInputImage>
ImageToRectilinearFEMObjectFilter<TInputImage>
::ImageToRectilinearFEMObjectFilter()
{
  this->m_NumberOfElements.set_size( NDimensions );
  this->m_NumberOfElements.fill( 0 );
  this->m_PixelsPerElement.set_size( NDimensions );
  this->m_PixelsPerElement.fill( 1 );
  this->m_Material = NULL;
  this->m_Element = NULL;
  this->ProcessObject::SetNthOutput(0, this->MakeOutput() );
}

template <class TInputImage>
void
ImageToRectilinearFEMObjectFilter<TInputImage>
::SetInput(InputImageType *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0,
                                   const_cast<InputImageType *>( image ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage>
void
ImageToRectilinearFEMObjectFilter<TInputImage>
::SetInput( unsigned int index, InputImageType * image )
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index,
                                   const_cast<InputImageType *>( image ) );
}

/**
 *
 */
template <class TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::InputImageType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetInput(void)
{
  if( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast<InputImageType *>
    (this->ProcessObject::GetInput(0) );
}

/**
 *
 */
template <class TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::InputImageType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetInput(unsigned int idx)
{
  return static_cast<InputImageType *>
    (this->ProcessObject::GetInput(idx) );
}

/**
 *
 */
template <class TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::DataObjectPointer
ImageToRectilinearFEMObjectFilter<TInputImage>
::MakeOutput()
{
  return static_cast<DataObject *>(FEMObjectType::New().GetPointer() );
}

/**
 *
 */
template <class TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::FEMObjectType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetOutput()
{
  if( this->GetNumberOfOutputs() < 1 )
    {
    return 0;
    }

  return static_cast<FEMObjectType *>
    (this->ProcessObject::GetOutput(0) );
}

/**
 *
 */
template <class TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::FEMObjectType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetOutput(unsigned int idx)
{
  FEMObjectType* out = dynamic_cast<FEMObjectType *>
    (this->ProcessObject::GetOutput(idx) );

  if( out == NULL )
    {
    itkWarningMacro( << "dynamic_cast to output type failed" );
    }
  return out;
}

template <class TInputImage>
void
ImageToRectilinearFEMObjectFilter<TInputImage>
::GenerateData()
{

  if( this->GetNumberOfInputs() < 1 )
    {
    itkWarningMacro( << "GenerateData() found no input objects" );
    }

  if( NDimensions == 2 )
    {
    Generate2DRectilinearMesh();
    }
  else
    {
    Generate3DRectilinearMesh();
    }
}

/**
 * Generate a rectangular mesh of quadrilateral elements
 */
template <class TInputImage>
void
ImageToRectilinearFEMObjectFilter<TInputImage>
::Generate2DRectilinearMesh()
{
  ImageConstPointer image = this->GetInput();
  ImageRegionType   region = image->GetLargestPossibleRegion();
  ImageSizeType     size  = region.GetSize();

  this->m_NumberOfElements[0] = size[0] / m_PixelsPerElement[0];
  this->m_NumberOfElements[1] = size[1] / m_PixelsPerElement[1];

  FEMObjectPointer femObject = this->GetOutput();
  femObject->GetLoadContainer()->Initialize();
  femObject->GetElementContainer()->Initialize();
  femObject->GetNodeContainer()->Initialize();

  // Create nodes
  Element::Node::Pointer  n;
  ImageIndexType nodeIndex;
  ImagePointType nodePoint;

  int gn = 0; // number of node
  for( double j = 0; j <= m_NumberOfElements[1]; j++ )
    {
    nodeIndex[1] = j * size[1] / m_PixelsPerElement[1];
    for( double i = 0; i <= m_NumberOfElements[0]; i++ )
      {
      nodeIndex[0] = i * size[0] / m_PixelsPerElement[0];
      image->TransformIndexToPhysicalPoint(nodeIndex, nodePoint);
      n = new Element::Node(nodePoint[0], nodePoint[1]);
      n->SetGlobalNumber(gn);
      gn++;
      femObject->AddNextNode(n);
      }
    }

  // Create elements
  gn = 0; // global number of the element
  Element2DC0LinearQuadrilateral::Pointer e;
  for( unsigned int j = 0; j < m_NumberOfElements[1]; j++ )
    {
    for( unsigned int i = 0; i < m_NumberOfElements[0]; i++ )
      {
      e = dynamic_cast<Element2DC0LinearQuadrilateral *>( &*m_Element->CreateAnother() );
      e->SetNode( 0, &*femObject->GetNode( (unsigned int)( i +  ( m_NumberOfElements[0] + 1 ) * j ) ) );
      e->SetNode( 1, &*femObject->GetNode( (unsigned int)( i + 1 + ( m_NumberOfElements[0] + 1 ) * j ) ) );
      e->SetNode( 2, &*femObject->GetNode( (unsigned int)( i + 1 + ( m_NumberOfElements[0] + 1 ) * ( j + 1 ) ) ) );
      e->SetNode( 3, &*femObject->GetNode( (unsigned int)( i +  ( m_NumberOfElements[0] + 1 ) * ( j + 1 ) ) ) );
      e->SetGlobalNumber(gn);
      gn++;
      femObject->AddNextElement(&*e);
      }
    }
}

/**
 * Generate a rectangular mesh of hexahedron elements
 */
template <class TInputImage>
void
ImageToRectilinearFEMObjectFilter<TInputImage>
::Generate3DRectilinearMesh()
{
  ImageConstPointer image = this->GetInput();
  ImageRegionType   region = image->GetLargestPossibleRegion();
  ImageSizeType     size  = region.GetSize();

  this->m_NumberOfElements[0] = size[0] / m_PixelsPerElement[0];
  this->m_NumberOfElements[1] = size[1] / m_PixelsPerElement[1];
  this->m_NumberOfElements[2] = size[2] / m_PixelsPerElement[2];

  FEMObjectPointer femObject = this->GetOutput();
  femObject->GetLoadContainer()->Initialize();
  femObject->GetElementContainer()->Initialize();
  femObject->GetNodeContainer()->Initialize();

  // Create nodes
  Element::Node::Pointer  n;
  ImageIndexType nodeIndex;
  ImagePointType nodePoint;
  int            gn = 0; // number of node
  for( double k = 0; k <= m_NumberOfElements[2]; k++ )
    {
    nodeIndex[2] = k * size[2] / m_PixelsPerElement[2];
    for( double j = 0; j <= m_NumberOfElements[1]; j++ )
      {
      nodeIndex[1] = j * size[1] / m_PixelsPerElement[1];
      for( double i = 0; i <= m_NumberOfElements[0]; i++ )
        {
        nodeIndex[0] = i * size[0] / m_PixelsPerElement[0];
        image->TransformIndexToPhysicalPoint(nodeIndex, nodePoint);
        n = new Element::Node(nodePoint[0], nodePoint[1], nodePoint[2]);
        n->SetGlobalNumber(gn);
        gn++;
        femObject->AddNextNode(n);
        }
      }
    }

  // Create elements
  gn = 0; // global number of the element
  itk::fem::Element3DC0LinearHexahedron::Pointer e;
  for( unsigned int k = 0; k < m_NumberOfElements[2]; k++ )
    {
    for( unsigned int j = 0; j < m_NumberOfElements[1]; j++ )
      {
      for( unsigned int i = 0; i < m_NumberOfElements[0]; i++ )
        {
        e = dynamic_cast<Element3DC0LinearHexahedron *>( &*m_Element->CreateAnother() );
        e->SetNode( 0,
                    &*femObject->GetNode( (unsigned int)( i
                                                          +  ( m_NumberOfElements[0]
                                                               + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 1,
                    &*femObject->GetNode( (unsigned int)( i + 1
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 2,
                    &*femObject->GetNode( (unsigned int)( i + 1
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 3,
                    &*femObject->GetNode( (unsigned int)( i
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 4,
                    &*femObject->GetNode( (unsigned int)( i
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetNode( 5,
                    &*femObject->GetNode( (unsigned int)( i + 1
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetNode( 6,
                    &*femObject->GetNode( (unsigned int)( i + 1
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetNode( 7,
                    &*femObject->GetNode( (unsigned int)( i
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetGlobalNumber(gn);
        gn++;
        femObject->AddNextElement(&*e);
        }
      }
    }
}

/**
 * PrintSelf
 */
template <class TInputImage>
void
ImageToRectilinearFEMObjectFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Number of Elements: " << m_NumberOfElements << std::endl;
  os << indent << "Pixels Per Element: " << m_PixelsPerElement << std::endl;
  os << indent << "Material: " << m_Material << std::endl;
  os << indent << "Element: " << m_Element << std::endl;
}

}
}  // end namespace itk::fem
#endif // __itkImageToRectilinearFEMObjectFilter_hxx
