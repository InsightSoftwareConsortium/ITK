/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorSegmentationLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePriorSegmentationLevelSetImageFilter_txx_
#define __itkShapePriorSegmentationLevelSetImageFilter_txx_

#include "itkShapePriorSegmentationLevelSetImageFilter.h"

namespace itk {

template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ShapeFunction: "        << m_ShapeFunction.GetPointer() << std::endl;
  os << indent << "Optimizer: "            << m_Optimizer.GetPointer() << std::endl;
  os << indent << "CostFunction: "         << m_CostFunction.GetPointer() << std::endl;
  os << indent << "InitialParameters: "    << m_InitialParameters << std::endl;
  os << indent << "CurrentParameters: "    << m_CurrentParameters << std::endl;
  os << indent << "ShapePriorSegmentationFunction: " << m_ShapePriorSegmentationFunction << std::endl;
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType>
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::ShapePriorSegmentationLevelSetImageFilter()
{
  m_ShapeFunction = NULL;
  m_Optimizer = NULL;
  m_CostFunction = NULL;
  m_ShapePriorSegmentationFunction = NULL;
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::SetShapePriorSegmentationFunction(ShapePriorSegmentationFunctionType *s)
{
  if ( s != m_ShapePriorSegmentationFunction )
    {
    m_ShapePriorSegmentationFunction = s;
    this->SetSegmentationFunction( s );
    this->Modified();
    }
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::SetShapeFunction(ShapeFunctionType *s)
{
  if ( s != m_ShapeFunction )
    {
    m_ShapeFunction = s;
    this->Modified();
    }
}

template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::InitializeIteration()
{

  if ( this->GetShapePriorScaling() != 0.0 )
    {
    /**
    * Estimate the shape and pose parameters. 
    */

    // Get the nodes of the active region and plug it into the cost function
    NodeContainerPointer nodes = NodeContainerType::New();
    this->ExtractActiveRegion( nodes );

    // Setup the cost function
    m_CostFunction->SetShapeFunction( m_ShapeFunction );
    m_Optimizer->SetCostFunction( m_CostFunction );
    m_CostFunction->SetActiveRegion( nodes );
    m_CostFunction->SetFeatureImage( this->GetFeatureImage() );
    m_CostFunction->Initialize();

    // Setup and start the optimization
    m_Optimizer->SetInitialPosition( m_CurrentParameters );
    m_Optimizer->StartOptimization();

    m_CurrentParameters = m_Optimizer->GetCurrentPosition();
    m_ShapeFunction->SetParameters( m_CurrentParameters );

    }

  Superclass::InitializeIteration();

}


template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::GenerateData()
{
  if ( !m_ShapeFunction )
    {
    itkExceptionMacro( << "ShapeFunction is not present");
    } 

  m_ShapeFunction->Initialize();

  // Set up the level set evolution components
  m_ShapePriorSegmentationFunction->SetShapeFunction( m_ShapeFunction );

  // Check if cost function and optimizers are present
  if ( !m_CostFunction )
    { 
    itkExceptionMacro( << "CostFunction is not present" );
    }

  if ( !m_Optimizer )
    {
    itkExceptionMacro( << "Optimizer is not present" );
    }

  if( m_InitialParameters.Size() != m_ShapeFunction->GetNumberOfParameters() )
    {
    itkExceptionMacro( << 
      "InitialParameters size does not match " <<
      "the number of parameters required by ShapeFunction" );
    }

  m_CurrentParameters = m_InitialParameters;

  // Start the solver
  Superclass::GenerateData();
}

/**
 * Populate a NodeContainer with nodes at each layer of the sparse field.
 */
template <class TInputImage, class TFeatureImage, class TOutputPixelType>
void
ShapePriorSegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType>
::ExtractActiveRegion( NodeContainerType * ptr )
{
  // clear the container
  ptr->Initialize();

  const typename FiniteDifferenceFunctionType::Pointer df
    = this->GetDifferenceFunction();

  typename LayerType::ConstIterator layerIt;
  NeighborhoodIterator<OutputImageType> outputIt(df->GetRadius(),
                this->GetOutput(), this->GetOutput()->GetRequestedRegion());

  unsigned int counter = 0;
  for ( unsigned int k = 0; k < this->GetNumberOfLayers(); k++ )
    {
    for (layerIt = m_Layers[k]->Begin(); layerIt != m_Layers[k]->End(); ++layerIt)
      {
        NodeType node;
        outputIt.SetLocation(layerIt->m_Value);
        node.SetIndex( outputIt.GetIndex() );
        node.SetValue( outputIt.GetCenterPixel() );
        ptr->InsertElement( counter++, node );
      }
    }

}

} // end namespace itk

#endif
