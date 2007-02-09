/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMissingExplicitIntantiations.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkPointSet.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkPath.h"
#include "itkSymmetricEigenAnalysis.h"
#include "itkImageMomentsCalculator.h"
#include "itkVersorRigid3DTransform.h"
#include "itkCenteredTransformInitializer.h"
#include "itkBoundingBox.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkCastImageFilter.h"

typedef itk::Matrix<float,3,3> MatrixF33;

typedef itk::Point<double,2> PointD2;
typedef itk::Point<double,3> PointD3;
typedef itk::Point<double,4> PointD4;
typedef itk::Point<float,3> PointF3;

typedef itk::Vector<float,2> VectorF2;
typedef itk::Vector<float,3> VectorF3;

typedef itk::Image<int,2> ImageI2;
typedef itk::Image<short,2> ImageS2;
typedef itk::Image<double,2> ImageD2;
typedef itk::Image<double,6> ImageD6;
typedef itk::Image<float,2> ImageF2;
typedef itk::Image<float,3> ImageF3;
typedef itk::Image<float,4> ImageF4;
typedef itk::Image<unsigned char,3> ImageUC3;
typedef itk::Image<VectorF2,2> ImageVF22;
typedef itk::Image<VectorF3,2> ImageVF32;

typedef itk::DefaultStaticMeshTraits<double, 2, 2, double, double, double> DefaultStaticMeshTraits22DDD;
typedef itk::DefaultStaticMeshTraits<double, 3, 3, double, double, double> DefaultStaticMeshTraits33DDD;
typedef itk::DefaultStaticMeshTraits<double, 4, 4, double, double, double> DefaultStaticMeshTraits44DDD;

typedef itk::ContinuousIndex<double,2> ContinuousIndexD2;
typedef itk::VectorContainer<unsigned long, PointF3> VectorContainerULPF3;

typedef itk::FixedArray<float,3> FixedArrayF3;
typedef itk::FixedArray<unsigned char,4> FixedArrayUC4;
typedef itk::VersorRigid3DTransform<double> VersorRigid3DTransformD;
typedef itk::Functor::Cast<float, double> FunctorCastFD;

template class ITKCommon_EXPORT itk::CenteredTransformInitializer<VersorRigid3DTransformD,ImageUC3,ImageUC3>;
template class ITKCommon_EXPORT itk::ImageMomentsCalculator<ImageUC3>;
template class ITKCommon_EXPORT itk::Neighborhood<VectorF3*>;
template class ITKCommon_EXPORT itk::Neighborhood<VectorF2*>;
template class ITKCommon_EXPORT itk::Neighborhood<FixedArrayUC4*,3>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageI2>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageS2>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageD6>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageF2>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageF3>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageF4>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageVF22>;
template struct ITKCommon_EXPORT itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<ImageVF32>;
template class ITKCommon_EXPORT itk::Path<double,ContinuousIndexD2,2>;
template class ITKCommon_EXPORT itk::PointLocator<unsigned long, 3, float, VectorContainerULPF3>;
template class ITKCommon_EXPORT itk::PointSet<PointD2,2,DefaultStaticMeshTraits22DDD>;
template class ITKCommon_EXPORT itk::PointSet<PointD3,3,DefaultStaticMeshTraits33DDD>;
template class ITKCommon_EXPORT itk::PointSet<PointD4,4,DefaultStaticMeshTraits44DDD>;
template class ITKCommon_EXPORT itk::SymmetricEigenAnalysis<MatrixF33,FixedArrayF3,MatrixF33>;
template class ITKCommon_EXPORT itk::UnaryFunctorImageFilter<ImageF2,ImageD2,FunctorCastFD>;
template class ITKCommon_EXPORT itk::VectorContainer<unsigned int,ContinuousIndexD2>;
template class ITKCommon_EXPORT itk::BoundingBox<unsigned long,3,float,VectorContainerULPF3>;
