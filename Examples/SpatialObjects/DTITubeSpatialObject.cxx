/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DTITubeSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// \index{itk::DTITubeSpatialObject}
//
// \doxygen{DTITubeSpatialObject} derives from \doxygen{TubeSpatialObject}. 
// It represents a fiber tracts from Diffusion Tensor Imaging.
// A DTITubeSpatialObject is described as a list of centerline points which 
// have a position, a radius, normals, the fractional anisotropy (FA) value, the ADC value,
// the geodesic anisotropy (GA) value, the eigen values and vectors as well as the full
// tensor matrix.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkDTITubeSpatialObject.h"
#include "itkDTITubeSpatialObjectPoint.h"
// Software Guide : EndCodeSnippet

int main( int , char *[] )
{

// Software Guide : BeginLatex
//
// DTITubeSpatialObject is templated over the dimension of the space.  A
// DTITubeSpatialObject contains a list of DTITubeSpatialObjectPoints.
//
// First we define some type definitions and we create the tube.
//
// Software Guide : EndLatex 

  unsigned int i;

// Software Guide : BeginCodeSnippet
  typedef itk::DTITubeSpatialObject<3>            DTITubeType;
  typedef itk::DTITubeSpatialObjectPoint<3>       DTITubePointType;

  DTITubeType::Pointer dtiTube = DTITubeType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We create a point list and we set:
// \begin{enumerate}
// \item The position of each point in the local coordinate system using the 
// \code{SetPosition()} method.
// \item The radius of the tube at this position using \code{SetRadius()}.
// \item The FA value using \code{SetFA()}.
// \item The ADC value using \code{SetADC()}.
// \item The GA value using \code{SetGA()}.
// \item The Eigen values value using \code{SetLambda1()},\code{SetLambda2()} and \code{SetLambda3()}.
// \item The Eigen vectors value using \code{SetMinEigenVector()},\code{SetMedEigenVector()} and \code{SetMaxEigenVector()}.
// \item The MRI value using \code{SetMRI()}.
// \item The full tensor matrix supposed to be symetric definite positive value using \code{SetTensorMatrix()}.
// \item The interpolation value using \code{SetInterpolation()}.
// \item The color of the point is set to red in our case.
// \end{enumerate}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  DTITubeType::PointListType list;
  for( i=0; i<5; i++)
    {
    DTITubePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetRadius(1);
    p.SetFA(i);
    p.SetADC(2*i);
    p.SetGA(3*i);
    p.SetLambda1(4*i);
    p.SetLambda2(5*i);
    p.SetLambda3(6*i);
    float* v = new float[3];
    v[0] = i;
    v[1] = 2*i;
    v[2] = 3*i;
    p.SetMinEigenVector(v);
    p.SetMedEigenVector(v);
    p.SetMaxEigenVector(v);
    delete v;
    v = new float[5];
    for(unsigned int j=0;j<5;j++)
      {
      v[j] = j;
      }
    p.SetMRI(v);
    delete v;
    v = new float[6];
    for(unsigned int k=0;k<6;k++)
      {
      v[k] = k;
      }
    p.SetTensorMatrix(v);
    delete v;
    p.SetInterpolation(1);
    p.SetColor(1,0,0,1);
    list.push_back(p);
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next, we create the tube and set its name using \code{SetName()}. We also
// set its identification number with \code{SetId()} and, at the end, we add
// the list of points previously created.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  dtiTube->GetProperty()->SetName("DTITube");
  dtiTube->SetId(1);
  dtiTube->SetPoints(list);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{GetPoints()} method return a reference to the internal list of
// points of the object.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  DTITubeType::PointListType pointList = dtiTube->GetPoints();
  std::cout << "Number of points representing the fiber tract: ";
  std::cout << pointList.size() << std::endl;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we can access the points using STL iterators.  \code{GetPosition()}
// and \code{GetColor()} functions return respectively the position and the
// color of the point.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  DTITubeType::PointListType::const_iterator it = dtiTube->GetPoints().begin();
  i=0;
  while(it != dtiTube->GetPoints().end())
    {
    std::cout << std::endl;
    std::cout << "Point #" << i << std::endl;
    std::cout << "Position: " << (*it).GetPosition() << std::endl;
    std::cout << "Radius: " << (*it).GetRadius() << std::endl;
    std::cout << "FA: " << (*it).GetFA() << std::endl;
    std::cout << "ADC: " << (*it).GetADC() << std::endl;
    std::cout << "GA: " << (*it).GetGA() << std::endl;
    std::cout << "Lambda1: " << (*it).GetLambda1() << std::endl;
    std::cout << "Lambda2: " << (*it).GetLambda2() << std::endl;
    std::cout << "Lambda3: " << (*it).GetLambda3() << std::endl;
    std::cout << "GetMinEigenVector: " << (*it).GetMinEigenVector()[0] << " : " << (*it).GetMinEigenVector()[1] << " : " << (*it).GetMinEigenVector()[2] << std::endl;
    std::cout << "GetMedEigenVector: " << (*it).GetMedEigenVector()[0] << " : " << (*it).GetMedEigenVector()[1] << " : " << (*it).GetMedEigenVector()[2] << std::endl;
    std::cout << "GetMaxEigenVector: " << (*it).GetMaxEigenVector()[0] << " : " << (*it).GetMaxEigenVector()[1] << " : " << (*it).GetMaxEigenVector()[2] << std::endl;   
    std::cout << "MRI: " << (*it).GetMRI()[0] << " : " << (*it).GetMRI()[1] << " : " << (*it).GetMRI()[2] << " : " << (*it).GetMRI()[3] << " : " << (*it).GetMRI()[4] << std::endl;
    std::cout << "TensorMatrix: " << (*it).GetTensorMatrix()[0] << " : ";
    std::cout << (*it).GetTensorMatrix()[1] << " : ";
    std::cout << (*it).GetTensorMatrix()[2] << " : ";
    std::cout << (*it).GetTensorMatrix()[3] << " : ";
    std::cout << (*it).GetTensorMatrix()[4] << " : ";
    std::cout << (*it).GetTensorMatrix()[5] << std::endl;
    std::cout << "Interpolation: " <<(*it).GetInterpolation() << std::endl;  
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    it++;
    i++;
    }
// Software Guide : EndCodeSnippet

  return 0;
}
