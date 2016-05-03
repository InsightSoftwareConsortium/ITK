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

#include "itkSpatialObjectTreeContainer.h"
#include "itkPostOrderTreeIterator.h"

//
// This test does next to nothing, but in response to
// https://www.itk.org/Bug/view.php?id=7455 -- it is a test that
// fails if the offending header itkSpatialObjectTreecontainer.h
// isn't fixed
int
itkSpatialObjectTreeContainerIteratorTest(int, char* [])
{
  typedef itk::SpatialObjectTreeContainer<3>     SOTreeType;
  typedef itk::PostOrderTreeIterator<SOTreeType> PostOrderItType;

  SOTreeType::Pointer tree = SOTreeType::New();

  PostOrderItType it(tree);
  return EXIT_SUCCESS;
}
