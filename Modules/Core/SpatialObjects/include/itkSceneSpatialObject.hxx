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
#ifndef itkSceneSpatialObject_hxx
#define itkSceneSpatialObject_hxx

#include "itkSceneSpatialObject.h"
#include <algorithm>

namespace itk
{
/** Constructor */
template< unsigned int TSpaceDimension >
SceneSpatialObject< TSpaceDimension >
::SceneSpatialObject()
{
  // Exactly the same as a group spatial object
  // Except the TypeName
  this->SetTypeName( "SceneSpatialObject" );
}

/** Destructor */
template< unsigned int TSpaceDimension >
SceneSpatialObject< TSpaceDimension >
::~SceneSpatialObject() = default;

/** Print the object */
template< unsigned int TSpaceDimension >
void
SceneSpatialObject< TSpaceDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end of namespace itk

#endif
