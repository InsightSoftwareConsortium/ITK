/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNewTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkMesh.h"
#include "itkCellInterfaceVisitor.h"
#include "itkCreateObjectFunction.h"
#include "itkDataAccessor.h"
#include "itkBackwardDifferenceOperator.h"
#include "itkForwardDifferenceOperator.h"
#include "itkAddImageFilter.h"
#include "itkBinaryImageFilter.h"

// #include "itkDerivativeImageFilter.h"
// #include "itkDiscreteGaussianImageFilter.h"
// #include "itkGradientMagnitudeImageFilter.h"
// #include "itkMultiplyImageFilter.h"
// #include "itkRecursiveGaussianImageFilter.h"
// #include "itkFirstDerivativeRecursiveGaussianImageFilter.h"
// #include "itkSecondDerivativeRecursiveGaussianImageFilter.h"
// #include "itkSubtractImageFilter.h"
// #include "itkTernaryImageFilter.h"
// #include "itkTernaryAddImageFilter.h"
// #include "itkTernaryMagnitudeImageFilter.h"
// #include "itkTernaryMagnitudeSquaredImageFilter.h"
// #include "itkRelabelWatershedImageFilter.h"
// #include "itkWatershedImageFilter.h"
// #include "itkGaussianOperator.h"
// #include "itkImageAdaptor.h"
// #include "itkImageContainerInterface.h"
// #include "itkImageLinearIterator.h"
// #include "itkImageMomentsCalculator.h"
// #include "itkImageSliceIterator.h"
// #include "itkKalmanLinearEstimator.h"
// #include "itkMeshRegion.h"
// #include "itkNonThreadedShrinkImageFilterFilter.h"
// #include "itkNumericTraits.h"
// #include "itkRandomAccessNeighborhoodIterator.h"
// #include "itkRegion.h"
// #include "itkRegistrationMapper.h"
// #include "itkRegistrationMapperImage.h"
// #include "itkRegistrationMapperProcrustes.h"
// #include "itkRegistrationTransform.h"
// #include "itkSimilarityRegistrationMetric.h"
// #include "itkProcrustesSimilarityRegistrationMetric.h"
// #include "itkOptimizer.h"
// #include "itkAmoebaOptimizer.h"
// #include "itkConjugateGradientOptimizer.h"
// #include "itkLBFGSOptimizer.h"
// #include "itkLevenbergMarquardtOptimizer.h"
// #include "itkNonLinearOptimizer.h"
// #include "itkRegistrationTransformation.h"
// #include "itkAffineTransform.h"
// #include "itkRegistrator3D2D.h"
// #include "itkRegistrator3D2DBatch.h"
// #include "itkRegistrator3D2DRecursive.h"
// #include "itkRGBPixel.h"
// #include "itkScalarVector.h"
// #include "itkVersion.h"




  class Bogus
  {
   public:
    // typedef Bogus Self;
    // typedef SmartPointer<Self> Pointer;
    // itkNewMacro(Self);
//     static Bogus* New() { return new Bogus(); };
//     void Register() {};
//     void UnRegister() {};

    float operator() ( double d, double d2 ) { return (float) d; };
    void Visit ( int, Bogus* ) {};
    int GetCellTopologyId() { return 1; };
    int GetTopologyId() { return 1; };
  };

int main ( int argc, char* argv[] )
{
  // Call New and Print on as many classes as possible

  // CellInterfaceVisitorImplementation
  itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellType, Bogus, Bogus>::Pointer CIVI = itk::CellInterfaceVisitorImplementation<float, itk::Mesh<float>::CellType, Bogus, Bogus>::New();

  // CreateObjectFunction
  // itk::CreateObjectFunction<itk::Mesh<int> >::Pointer COF = itk::CreateObjectFunction<itk::Mesh<int> >::New();
  // itk::Mesh<int>::Pointer B = COF->CreateObject();

  // DataAccessor
  float f = 100.0;
  double d = itk::DataAccessor<float, double>::Get ( f );

  // BackwardDifferenceOperator
  // Bad
  // typedef itk::BackwardDifferenceOperator<double> iDHBO;
  // iDHBO::Pointer dhbo = iDHBO::New();

  // ForwardDifferenceOperator
  // Bad
  // typedef itk::ForwardDifferenceOperator<double> iDHFO;
  // iDHFO::Pointer dhfo = iDHFO::New();

  // AddImageFilter
  typedef itk::AddImageFilter<itk::Image<double>, itk::Image<double>, itk::Image<double> > iFIA;
  iFIA::Pointer FIA = iFIA::New();
  
  // BinaryImageFilter
  typedef itk::BinaryImageFilter<itk::Image<double>, itk::Image<double>, itk::Image<double>, Bogus > iFIB;
  iFIB::Pointer FIB = iFIB::New();
  
  return 0;
  
}
