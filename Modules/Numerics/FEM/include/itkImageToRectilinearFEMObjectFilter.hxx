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
#ifndef itkImageToRectilinearFEMObjectFilter_hxx
#define itkImageToRectilinearFEMObjectFilter_hxx

#include "itkImageToRectilinearFEMObjectFilter.h"
#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "itkFEMElement3DC0LinearHexahedron.h"
#include <cmath>
namespace itk
{
namespace fem
{

/*
 * Default constructor for Filter
 */
template <typename TInputImage>
ImageToRectilinearFEMObjectFilter<TInputImage>
::ImageToRectilinearFEMObjectFilter()
{
  this->m_NumberOfElements.set_size( NDimensions );
  this->m_NumberOfElements.fill( 0 );
  this->m_PixelsPerElement.set_size( NDimensions );
  this->m_PixelsPerElement.fill( 1 );
  this->m_Material = ITK_NULLPTR;
  this->m_Element = ITK_NULLPTR;
  this->ProcessObject::SetNthOutput(0, this->MakeOutput(0) );
}

template <typename TInputImage>
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
template <typename TInputImage>
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
template <typename TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::InputImageType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetInput(void)
{
  if( this->GetNumberOfInputs() < 1 )
    {
    return ITK_NULLPTR;
    }

  return itkDynamicCastInDebugMode<InputImageType *>(this->ProcessObject::GetInput(0) );
}

/**
 *
 */
template <typename TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::InputImageType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetInput(unsigned int idx)
{
  return itkDynamicCastInDebugMode<InputImageType *>(this->ProcessObject::GetInput(idx) );
}

/**
 *
 */
template <typename TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::DataObjectPointer
ImageToRectilinearFEMObjectFilter<TInputImage>
::MakeOutput(DataObjectPointerArraySizeType itkNotUsed(idx))
{
  return FEMObjectType::New().GetPointer();
}

/**
 *
 */
template <typename TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::FEMObjectType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetOutput()
{
  if( this->GetNumberOfOutputs() < 1 )
    {
    return ITK_NULLPTR;
    }

  return itkDynamicCastInDebugMode<FEMObjectType *>(this->ProcessObject::GetOutput(0) );
}

/**
 *
 */
template <typename TInputImage>
typename ImageToRectilinearFEMObjectFilter<TInputImage>::FEMObjectType *
ImageToRectilinearFEMObjectFilter<TInputImage>
::GetOutput(unsigned int idx)
{
  FEMObjectType* out = dynamic_cast<FEMObjectType *>
    (this->ProcessObject::GetOutput(idx) );

  if( out == ITK_NULLPTR )
    {
    itkWarningMacro( << "dynamic_cast to output type failed" );
    }
  return out;
}

template <typename TInputImage>
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
template <typename TInputImage>
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
  femObject->GetModifiableLoadContainer()->Initialize();
  femObject->GetModifiableElementContainer()->Initialize();
  femObject->GetModifiableNodeContainer()->Initialize();

  // Create nodes
  Element::Node::Pointer  n;
  ImageIndexType nodeIndex;
  ImagePointType nodePoint;

  if ( this->m_Material )
    {
                femObject->AddNextMaterial( this->m_Material );
                }

  int gn = 0; // number of node
  for( typename ImageSizeType::SizeValueType j = 0; j <= m_NumberOfElements[1]; j++ )
    {
    nodeIndex[1] = j * m_PixelsPerElement[1];
    for( typename ImageSizeType::SizeValueType i = 0; i <= m_NumberOfElements[0]; i++ )
      {
      nodeIndex[0] = i * m_PixelsPerElement[0];
      image->TransformIndexToPhysicalPoint(nodeIndex, nodePoint);
      Element::VectorType pt(2);
      pt[0] = nodePoint[0]; pt[1] = nodePoint[1];
      n =  Element::Node::New();
      n->SetCoordinates(pt);
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
      e = dynamic_cast<Element2DC0LinearQuadrilateral *>( m_Element->CreateAnother().GetPointer() );
      e->SetNode( 0, femObject->GetNode( (unsigned int)( i +  ( m_NumberOfElements[0] + 1 ) * j ) ) );
      e->SetNode( 1, femObject->GetNode( (unsigned int)( i + 1 + ( m_NumberOfElements[0] + 1 ) * j ) ) );
      e->SetNode( 2, femObject->GetNode( (unsigned int)( i + 1 + ( m_NumberOfElements[0] + 1 ) * ( j + 1 ) ) ) );
      e->SetNode( 3, femObject->GetNode( (unsigned int)( i +  ( m_NumberOfElements[0] + 1 ) * ( j + 1 ) ) ) );
      e->SetGlobalNumber(gn);
      if ( this->m_Material )
        {
        e->SetMaterial( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(0).GetPointer() ) );
        }
      gn++;
      femObject->AddNextElement(e.GetPointer());
      }
    }
}

/**
 * Generate a rectangular mesh of hexahedron elements
 */
template <typename TInputImage>
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
  femObject->GetModifiableLoadContainer()->Initialize();
  femObject->GetModifiableElementContainer()->Initialize();
  femObject->GetModifiableNodeContainer()->Initialize();

  if ( this->m_Material )
    {
                femObject->AddNextMaterial( this->m_Material );
                }

  // Create nodes
  Element::Node::Pointer  n;
  ImageIndexType nodeIndex;
  ImagePointType nodePoint;
  int            gn = 0; // number of node
  for( unsigned int k = 0; k <= m_NumberOfElements[2]; ++k )
    {
    nodeIndex[2] = k * m_PixelsPerElement[2];
    for( unsigned int j = 0; j <= m_NumberOfElements[1]; ++j )
      {
      nodeIndex[1] = j * m_PixelsPerElement[1];
      for( unsigned int i = 0; i <= m_NumberOfElements[0]; ++i )
        {
        nodeIndex[0] = i * m_PixelsPerElement[0];
        image->TransformIndexToPhysicalPoint(nodeIndex, nodePoint);
        Element::VectorType pt(3);
        pt[0] = nodePoint[0]; pt[1] = nodePoint[1]; pt[2] = nodePoint[2];
        n =  Element::Node::New();
        n->SetCoordinates(pt);
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
        e = dynamic_cast<Element3DC0LinearHexahedron *>( m_Element->CreateAnother().GetPointer() );
        e->SetNode( 0,
                    femObject->GetNode( (unsigned int)( i +  ( m_NumberOfElements[0]
                                                               + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 1,
                    femObject->GetNode( (unsigned int)( i + 1 + ( m_NumberOfElements[0]
                                                                  + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 2,
                    femObject->GetNode( (unsigned int)( i + 1 + ( m_NumberOfElements[0]
                                                                  + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 3,
                    femObject->GetNode( (unsigned int)( i + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * k ) ) ) );
        e->SetNode( 4,
                    femObject->GetNode( (unsigned int)( i + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetNode( 5,
                    femObject->GetNode( (unsigned int)( i + 1
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j  + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetNode( 6,
                    femObject->GetNode( (unsigned int)( i + 1
                                                          + ( m_NumberOfElements[0]
                                                              + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetNode( 7,
                    femObject->GetNode( (unsigned int)( i
                                                        + ( m_NumberOfElements[0]
                                                            + 1 ) * ( j + 1 + ( m_NumberOfElements[1] + 1 ) * ( k + 1 ) ) ) ) );
        e->SetGlobalNumber(gn);
        if ( this->m_Material )
          {
          e->SetMaterial( dynamic_cast<itk::fem::MaterialLinearElasticity *>( femObject->GetMaterial(0).GetPointer() ) );
          }
        gn++;
        femObject->AddNextElement(e.GetPointer());
        }
      }
    }
}

/**
 * PrintSelf
 */
template <typename TInputImage>
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
#endif // itkImageToRectilinearFEMObjectFilter_hxx
