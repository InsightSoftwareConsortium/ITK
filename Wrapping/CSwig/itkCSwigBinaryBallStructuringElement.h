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
#include "itkBinaryBallStructuringElement.h"

namespace neighborhood
{

}

namespace structuringElement
{
  typedef itk::BinaryBallStructuringElement<float, 2 >::Self             F2;
  typedef itk::BinaryBallStructuringElement<float, 3 >::Self             F3;
  typedef itk::BinaryBallStructuringElement<unsigned char, 2 >::Self     UC2;
  typedef itk::BinaryBallStructuringElement<unsigned char, 3 >::Self     UC3;
  typedef itk::BinaryBallStructuringElement<unsigned short, 2 >::Self    US2;
  typedef itk::BinaryBallStructuringElement<unsigned short, 3 >::Self    US3;
}


