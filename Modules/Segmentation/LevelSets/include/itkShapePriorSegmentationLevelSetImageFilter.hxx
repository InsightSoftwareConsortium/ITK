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
#ifndef itkShapePriorSegmentationLevelSetImageFilter_hxx
#define itkShapePriorSegmentationLevelSetImageFilter_hxx

#include "itkShapePriorSegmentationLevelSetImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ShapeFunction: "        << m_ShapeFunction.GetPointer() << std::endl;
  os << indent << "Optimizer: "            << m_Optimizer.GetPointer() << std::endl;
  os << indent << "CostFunction: "         << m_CostFunction.GetPointer() << std::endl;
  os << indent << "InitialParameters: "    << m_InitialParameters << std::endl;
  os << indent << "CurrentParameters: "    << m_CurrentParameters << std::endl;
  os << indent << "ShapePriorSegmentationFunction: " << m_ShapePriorSegmentationFunction << std::endl;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::ShapePriorSegmentationLevelSetImageFilter()
{
  m_ShapeFunction = ITK_NULLPTR;
  m_Optimizer = ITK_NULLPTR;
  m_CostFunction = ITK_NULLPTR;
  m_ShapePriorSegmentationFunction = ITK_NULLPTR;
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::SetShapePriorSegmentationFunction(ShapePriorSegmentationFunctionType *s)
{
  if ( s != m_ShapePriorSegmentationFunction )
    {
    m_ShapePriorSegmentationFunction = s;
    this->SetSegmentationFunction(s);
    this->Modified();
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::SetShapeFunction(ShapeFunctionType *s)
{
  if ( s != m_ShapeFunction )
    {
    m_ShapeFunction = s;
    this->Modified();
    }
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::InitializeIteration()
{
  if ( this->GetShapePriorScaling() != 0.0 )
    {
    /**
     * Estimate the shape and pose parameters.
     */

    // Get the nodes of the active region and plug it into the cost function
    NodeContainerPointer nodes = NodeContainerType::New();
    this->ExtractActiveRegion(nodes);

    // Setup the cost function
    m_CostFunction->SetShapeFunction(m_ShapeFunction);
    m_Optimizer->SetCostFunction(m_CostFunction);
    m_CostFunction->SetActiveRegion(nodes);
    m_CostFunction->SetFeatureImage( this->GetFeatureImage() );
    m_CostFunction->Initialize();

    // Setup and start the optimization
    m_Optimizer->SetInitialPosition(m_CurrentParameters);
    m_Optimizer->StartOptimization();

    m_CurrentParameters = m_Optimizer->GetCurrentPosition();
    m_ShapeFunction->SetParameters(m_CurrentParameters);
    }

  Superclass::InitializeIteration();
}

template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::GenerateData()
{
  if ( !m_ShapeFunction )
    {
    itkExceptionMacro(<< "ShapeFunction is not present");
    }

  m_ShapeFunction->Initialize();

  // Set up the level set evolution components
  m_ShapePriorSegmentationFunction->SetShapeFunction(m_ShapeFunction);

  // Check if cost function and optimizers are present
  if ( !m_CostFunction )
    {
    itkExceptionMacro(<< "CostFunction is not present");
    }

  if ( !m_Optimizer )
    {
    itkExceptionMacro(<< "Optimizer is not present");
    }

  if ( m_InitialParameters.Size() != m_ShapeFunction->GetNumberOfParameters() )
    {
    itkExceptionMacro(
      << "InitialParameters size does not match "
      << "the number of parameters required by ShapeFunction");
    }

  m_CurrentParameters = m_InitialParameters;

  // Start the solver
  Superclass::GenerateData();
}

/**
 * Populate a NodeContainer with nodes at each layer of the sparse field.
 */
template< typename TInputImage, typename TFeatureImage, typename TOutputPixelType >
void
ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, TOutputPixelType >
::ExtractActiveRegion(NodeContainerType *ptr)
{
  // clear the container
  ptr->Initialize();

  const typename Superclass::FiniteDifferenceFunctionType::Pointer df =
    this->GetDifferenceFunction();

  typename Superclass::LayerType::ConstIterator layerIt;
  NeighborhoodIterator< OutputImageType > outputIt( df->GetRadius(),
                                                    this->GetOutput(), this->GetOutput()->GetRequestedRegion() );

  unsigned int counter = 0;
  for ( unsigned int k = 0; k < this->GetNumberOfLayers(); k++ )
    {
    for ( layerIt = this->m_Layers[k]->Begin(); layerIt != this->m_Layers[k]->End(); ++layerIt )
      {
      NodeType node;
      outputIt.SetLocation(layerIt->m_Value);
      node.SetIndex( outputIt.GetIndex() );
      node.SetValue( outputIt.GetCenterPixel() );
      ptr->InsertElement(counter++, node);
      }
    }
}
} // end namespace itk

#endif
