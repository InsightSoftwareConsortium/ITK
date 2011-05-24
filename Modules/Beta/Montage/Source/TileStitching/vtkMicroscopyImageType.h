/*=========================================================================
 *
 *  Copyright Kitware Inc.
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

#ifndef __vtkMicroscopyImageType_h
#define __vtkMicroscopyImageType_h

#define ENABLE_DEBUG 1

namespace vtkMicroscopy {

static const double EPSILON = 0.0000000001;
static const int FILE_NODE_OFFSET = 2;

typedef enum
  {
  UNKNOWNPIXELTYPE,
  SCALAR,
  VECTOR,
  RGB
  } PixelType;

typedef enum
  {
  UNKNOWNCOMPONENTTYPE,
  UNSIGNED_CHAR,
  UNSIGNED_SHORT,
  SHORT,
  FLOAT,
  DOUBLE
  } ComponentType;

enum
  {
  UNKNOWNSCANORDERTYPE,
  COLUMNFIRST,  // y-->x-->z
  ROWFIRST,     // x-->y-->z
  DEPTHFIRST    // z-->x-->y
  };

}

#endif
