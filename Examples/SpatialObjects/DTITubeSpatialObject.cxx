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

// Software Guide : BeginLatex
//
// \index{itk::DTITubeSpatialObject}
//
// \doxygen{DTITubeSpatialObject} derives from \doxygen{TubeSpatialObject}.
// It represents a fiber tracts from Diffusion Tensor Imaging.
// A DTITubeSpatialObject is described as a list of centerline points which
// have a position, a radius, normals, the fractional anisotropy (FA) value, the ADC value,
// the geodesic anisotropy (GA) value, the eigenvalues and vectors as well as the full
// tensor matrix.
//
// Let's start by including the appropriate header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkDTITubeSpatialObject.h"
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
// \item The FA value using \code{AddField(DTITubePointType::FA)}.
// \item The ADC value using \code{AddField(DTITubePointType::ADC)}.
// \item The GA value using \code{AddField(DTITubePointType::GA)}.
// \item The full tensor matrix supposed to be symmetric definite positive value using \code{SetTensorMatrix()}.
// \item The color of the point is set to red in our case.
// \end{enumerate}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  DTITubeType::PointListType list;
  for (i=0; i<5; ++i)
    {
    DTITubePointType p;
    p.SetPosition(i,i+1,i+2);
    p.SetRadius(1);
    p.AddField(DTITubePointType::FA,i);
    p.AddField(DTITubePointType::ADC,2*i);
    p.AddField(DTITubePointType::GA,3*i);
    p.AddField("Lambda1",4*i);
    p.AddField("Lambda2",5*i);
    p.AddField("Lambda3",6*i);
    float* v = new float[6];
    for(unsigned int k=0;k<6;k++)
      {
      v[k] = k;
      }
    p.SetTensorMatrix(v);
    delete[] v;
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
    std::cout << "FA: " << (*it).GetField(DTITubePointType::FA) << std::endl;
    std::cout << "ADC: " << (*it).GetField(DTITubePointType::ADC) << std::endl;
    std::cout << "GA: " << (*it).GetField(DTITubePointType::GA) << std::endl;
    std::cout << "Lambda1: " << (*it).GetField("Lambda1") << std::endl;
    std::cout << "Lambda2: " << (*it).GetField("Lambda2") << std::endl;
    std::cout << "Lambda3: " << (*it).GetField("Lambda3") << std::endl;
    std::cout << "TensorMatrix: " << (*it).GetTensorMatrix()[0] << " : ";
    std::cout << (*it).GetTensorMatrix()[1] << " : ";
    std::cout << (*it).GetTensorMatrix()[2] << " : ";
    std::cout << (*it).GetTensorMatrix()[3] << " : ";
    std::cout << (*it).GetTensorMatrix()[4] << " : ";
    std::cout << (*it).GetTensorMatrix()[5] << std::endl;
    std::cout << "Color = " << (*it).GetColor() << std::endl;
    ++it;
    ++i;
    }
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}
