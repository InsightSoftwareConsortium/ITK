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
#include "itkOpenCVTestHelper.h"

#include <map>

#include "opencv2/core/version.hpp"
#include "opencv2/imgcodecs.hpp" // cv::imread

#if CV_VERSION_MAJOR > 3 // OpenCV >= 4.0
#  include "opencv2/imgcodecs/legacy/constants_c.h"
#  include <opencv2/core/core_c.h>
#else                                        // OpenCV < 4.0
#  include "opencv2/imgcodecs/imgcodecs_c.h" // CV_LOAD_IMAGE_COLOR
#endif

#if CV_VERSION_MAJOR > 3
// Based on https://stackoverflow.com/a/13683381/471839
IplImage *
cvLoadImage(const char * filename, int iscolor)
{
  std::map<int, int> matDepthToIplDepth{ { CV_8U, IPL_DEPTH_8U },   { CV_8S, IPL_DEPTH_8S },
                                         { CV_16U, IPL_DEPTH_16U }, { CV_16S, IPL_DEPTH_16S },
                                         { CV_32S, IPL_DEPTH_32S }, { CV_32F, IPL_DEPTH_32F },
                                         { CV_64F, IPL_DEPTH_64F } };
  cv::Mat            mat = cv::imread(filename, iscolor);
  IplImage *         ipl = cvCreateImage(cvSize(mat.cols, mat.rows), matDepthToIplDepth[mat.depth()], mat.channels());
  IplImage           iplTemp = mat;
  cvCopy(&iplTemp, ipl);
  return ipl;
}
#endif
