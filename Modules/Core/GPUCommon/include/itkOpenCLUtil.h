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
#ifndef itkOpenCLUtil_h
#define itkOpenCLUtil_h

#include <cstring>
#include <cstdlib>
#include <cstdio>

#include <iostream>
#include <sstream>

#include <itkVector.h>

#ifdef __APPLE__

#include <OpenCL/opencl.h>

#else

#include <CL/opencl.h>

#endif

#include <itkMacro.h>

namespace itk
{
/** Get the local block size based on the desired Image Dimension
 * currently set as follows:
 * OpenCL workgroup (block) size for 1/2/3D - needs to be tuned based on the GPU architecture
 * 1D : 256
 * 2D : 16x16 = 256
 * 3D : 4x4x4 = 64
 */
int OpenCLGetLocalBlockSize(unsigned int ImageDim);

/** Get the devices that are available */
cl_device_id* OpenCLGetAvailableDevices(cl_platform_id platform, cl_device_type devType, cl_uint* numAvailableDevices);

/** Get the device that has the maximum FLOPS in the current context */
cl_device_id OpenCLGetMaxFlopsDev(cl_context cxGPUContext);

/** Print device name and info */
void OpenCLPrintDeviceInfo(cl_device_id device, bool verbose=false);

/** Find the OpenCL platform that matches the "name" */
cl_platform_id OpenCLSelectPlatform(const char* name);

/** Check OpenCL error */
void OpenCLCheckError(cl_int error, const char* filename="", int lineno=0, const char* location="");

/** Check if OpenCL-enabled GPU is present. */
bool IsGPUAvailable();

/** Get Typename */
std::string GetTypename(const std::type_info& intype);

/** Get Typename in String if a valid type */
bool GetValidTypename(const std::type_info& intype, const std::vector<std::string>& validtypes, std::string& retTypeName);

/** Get 64-bit pragma */
std::string Get64BitPragma();

/** Get Typename in String */
void GetTypenameInString( const std::type_info& intype, std::ostringstream& ret );

/** Get pixel dimension (number of channels).
 * For high-dimensional pixel format, only itk::Vector< type, 2/3 > is acceptable. */
int GetPixelDimension( const std::type_info& intype );

}

#endif
