/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPCAShapeSignedDistanceFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPCAShapeSignedDistanceFunction_txx
#define _itkPCAShapeSignedDistanceFunction_txx

#include "itkPCAShapeSignedDistanceFunction.h"


namespace itk
{


// Constructor with default arguments
template<typename TCoordRep, unsigned int VSpaceDimension>
PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::PCAShapeSignedDistanceFunction()
{
  m_NumberOfPrincipalComponents = 0;
  m_NumberOfTransformParameters = 0;

  m_MeanImage = NULL;
  m_PrincipalComponentImages.resize(0);
  m_PrincipalComponentStandardDeviations.resize(0);

  m_Transform = TranslationTransform<TCoordRep, SpaceDimension>::New();
  m_Interpolators.resize(0);
  m_Extrapolators.resize(0);
  m_Selectors.resize(0);

  m_WeightOfPrincipalComponents.resize(0);
  m_TransformParameters.resize(0);
  m_Parameters.resize(0);
}
    

// Set the number of principal components
template<typename TCoordRep, unsigned int VSpaceDimension>
void
PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::SetNumberOfPrincipalComponents(unsigned int n)
{
  m_NumberOfPrincipalComponents = n;

  m_PrincipalComponentImages.resize(n);
  m_PrincipalComponentStandardDeviations.resize(n);

  m_WeightOfPrincipalComponents.resize(n);
}


// Set the parameters
template<typename TCoordRep, unsigned int VSpaceDimension>
void
PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::SetParameters( const ParametersType & parameters )
{
  m_Parameters = parameters;

  // set the shape parameters
  unsigned int i;
  for(i=0; i<m_NumberOfPrincipalComponents; i++)
    { m_WeightOfPrincipalComponents[i] = parameters[i]; }

  // set the transform parameters
  m_NumberOfTransformParameters = 
    parameters.size() - m_NumberOfPrincipalComponents;
  m_TransformParameters.resize(m_NumberOfTransformParameters);

  for(i=0; i<m_NumberOfTransformParameters; i++)
    {m_TransformParameters[i] = parameters[m_NumberOfPrincipalComponents+i];}
}


// Print self
template<typename TCoordRep, unsigned int VSpaceDimension>
void
PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "WeightOfPrincipalComponents: " 
    << m_WeightOfPrincipalComponents << std::endl;

  os << indent << "TransformParameters: " 
    << m_TransformParameters << std::endl;
}


// Initialize the function
template<typename TCoordRep, unsigned int VSpaceDimension>
void
PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::Initialize() throw ( ExceptionObject )
{
  // verify mean image
  if ( !m_MeanImage )
    { itkExceptionMacro( << "MeanImage is not present." ); }

  // verify principal component images
  if ( m_PrincipalComponentImages.size()==0 )
    { itkExceptionMacro( << "PrincipalComponentImages are not present." ); }

  // verify image size 
  typename ImageType::SizeType meanImageSize = 
    m_MeanImage->GetBufferedRegion().GetSize();
  typename ImageType::SizeType pcImageSize; 

  for (unsigned int i=0; i<m_PrincipalComponentImages.size(); i++)
    {
    if ( !m_PrincipalComponentImages[i] )
      {
      itkExceptionMacro( << "PrincipalComponentImages[" 
        << i << "] is not present." );
      }

    pcImageSize=m_PrincipalComponentImages[i]->GetBufferedRegion().GetSize();
    for (unsigned int j=0; i<ImageType::ImageDimension; i++)
      {
        if (pcImageSize[j] != meanImageSize[j])
        {
        itkExceptionMacro( << "The size of the PrincipalComponentImages[" 
          << i << "] is different from the MeanImage." );
        }
      }
    }


  // set up the transform
  m_Transform->SetParameters(m_TransformParameters);

  // set up the interpolators/extrapolators for each of the mean and pc images
  m_Interpolators.resize(m_NumberOfPrincipalComponents + 1);
  m_Extrapolators.resize(m_NumberOfPrincipalComponents + 1);
  m_Selectors.resize(m_NumberOfPrincipalComponents+1);

  // interpolator/extrapolator for mean image
  m_Interpolators[0] = 
    NearestNeighborInterpolateImageFunction<ImageType, double>::New();
  m_Interpolators[0]->SetInputImage(m_MeanImage);

  m_Extrapolators[0] = 
    NearestNeighborExtrapolateImageFunction<ImageType, double>::New();
  m_Extrapolators[0]->SetInputImage(m_MeanImage);

  // interpolators/extrapolators for pc images
  for (unsigned int k=1; k<=m_NumberOfPrincipalComponents; k++)
    {
    m_Interpolators[k] = 
      NearestNeighborInterpolateImageFunction<ImageType, double>::New();
    m_Interpolators[k]->SetInputImage(m_PrincipalComponentImages[k-1]);

    m_Extrapolators[k] = 
      NearestNeighborExtrapolateImageFunction<ImageType, double>::New();
    m_Extrapolators[k]->SetInputImage(m_PrincipalComponentImages[k-1]);
    }
}


// Evaluate the signed distance
template<typename TCoordRep, unsigned int VSpaceDimension>
typename PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::OutputType
PCAShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
::Evaluate( const PointType& point ) const
{
  // transform the point into the shape model space
  PointType mappedPoint = m_Transform->TransformPoint(point);

  itkDebugMacro(<< "mappedPoint:" << mappedPoint);

  if(!m_Interpolators[0]->IsInsideBuffer(mappedPoint))
    {
    for(unsigned int i=0; i<=m_NumberOfPrincipalComponents; i++)
      { m_Selectors[i] = m_Extrapolators[i]; }
    itkDebugMacro(<< "use extrapolator");
    }
  else
    {
    for(unsigned int i=0; i<=m_NumberOfPrincipalComponents; i++)
      { m_Selectors[i] = m_Interpolators[i]; }
    itkDebugMacro(<< "use interpolator");
    }

  typedef typename NumericTraits<OutputType>::RealType RealType;
  RealType output = m_Selectors[0]->Evaluate(mappedPoint);

  for(unsigned int i=0; i<m_NumberOfPrincipalComponents; i++)
    {
    output += m_Selectors[i+1]->Evaluate(mappedPoint) *
      m_PrincipalComponentStandardDeviations[i] *
      m_WeightOfPrincipalComponents[i] ;
    }

  return output;
}
  

} // namespace

#endif
