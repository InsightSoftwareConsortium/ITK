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
#include "itkDerivativeHalfBackwardOperator.h"
#include "itkDerivativeHalfForwardOperator.h"
#include "itkFilterImageAdd.h"
#include "itkFilterImageBinary.h"

// #include "itkFilterImageDerivative.h"
// #include "itkFilterImageDiscreteGaussian.h"
// #include "itkFilterImageGradientMagnitude.h"
// #include "itkFilterImageMult.h"
// #include "itkFilterImageRecursiveGaussian.h"
// #include "itkFilterImageRecursiveGaussianFirstDerivative.h"
// #include "itkFilterImageRecursiveGaussianSecondDerivative.h"
// #include "itkFilterImageSub.h"
// #include "itkFilterImageTernary.h"
// #include "itkFilterImageTernaryAdd.h"
// #include "itkFilterImageTernaryModulus.h"
// #include "itkFilterImageTernaryModulusSquare.h"
// #include "itkFilterImageVectorValuedAnisotropicDiffusion.h"
// #include "itkFilterImageWatershedLevelAdaptor.h"
// #include "itkFilterImageWatershedSegment.h"
// #include "itkGaussianOperator.h"
// #include "itkImageAdaptor.h"
// #include "itkImageContainerInterface.h"
// #include "itkImageLinearIterator.h"
// #include "itkImageMoments.h"
// #include "itkImageSliceIterator.h"
// #include "itkKalmanLinearEstimator.h"
// #include "itkMeshRegion.h"
// #include "itkNonThreadedShrinkImage.h"
// #include "itkNumericTraits.h"
// #include "itkRandomAccessNeighborhoodIterator.h"
// #include "itkRegion.h"
// #include "itkRegistrationMapper.h"
// #include "itkRegistrationMapperImage.h"
// #include "itkRegistrationMapperProcrustes.h"
// #include "itkRegistrationMethod.h"
// #include "itkRegistrationMetric.h"
// #include "itkRegistrationMetricProcrustes.h"
// #include "itkRegistrationOptimizer.h"
// #include "itkRegistrationOptimizerAmoeba.h"
// #include "itkRegistrationOptimizerConjugateGradient.h"
// #include "itkRegistrationOptimizerLBFGS.h"
// #include "itkRegistrationOptimizerLevenbergMarquardt.h"
// #include "itkRegistrationOptimizerNonLinear.h"
// #include "itkRegistrationTransformation.h"
// #include "itkRegistrationTransformationAffine.h"
// #include "itkRegistrator3D2D.h"
// #include "itkRegistrator3D2DBatch.h"
// #include "itkRegistrator3D2DRecursive.h"
// #include "itkRGB.h"
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

  // DerivativeHalfBackwardOperator
  // Bad
  // typedef itk::DerivativeHalfBackwardOperator<double> iDHBO;
  // iDHBO::Pointer dhbo = iDHBO::New();

  // DerivativeHalfForwardOperator
  // Bad
  // typedef itk::DerivativeHalfForwardOperator<double> iDHFO;
  // iDHFO::Pointer dhfo = iDHFO::New();

  // FilterImageAdd
  typedef itk::FilterImageAdd<itk::Image<double>, itk::Image<double>, itk::Image<double> > iFIA;
  iFIA::Pointer FIA = iFIA::New();
  
  // FilterImageBinary
  typedef itk::FilterImageBinary<itk::Image<double>, itk::Image<double>, itk::Image<double>, Bogus > iFIB;
  iFIB::Pointer FIB = iFIB::New();
  
  return 0;
  
}
