/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkOpenCVTestHelper_h
#define itkOpenCVTestHelper_h

#include <opencv2/core/types_c.h>
#include <opencv2/core/version.hpp>

#if CV_VERSION_MAJOR > 3
// This is a shim to support tests that depend on loading IplImages via the
// obsolete C API that was dropped in OpenCV 4.
IplImage *
cvLoadImage(const char * filename, int iscolor);
#endif

#endif // itkOpenCVTestHelper_h
