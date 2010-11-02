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
#include "itkNeighborhood.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkNeighborhood);
  namespace wrappers
  {
    typedef itk::Neighborhood<float, 2 >::Self itkNeighborhoodF2;
    typedef itk::Neighborhood<float, 3 >::Self itkNeighborhoodF3;
    typedef itk::Neighborhood<unsigned char, 2 >::Self itkNeighborhoodUC2;
    typedef itk::Neighborhood<unsigned char, 3 >::Self itkNeighborhoodUC3;
    typedef itk::Neighborhood<unsigned short, 2 >::Self itkNeighborhoodUS2;
    typedef itk::Neighborhood<unsigned short, 3 >::Self itkNeighborhoodUS3;
  }
}
#endif
