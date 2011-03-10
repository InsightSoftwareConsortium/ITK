#ifndef __itkOclUtil_h
#define __itkOclUtil_h

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef __APPLE__

#include <OpenCL/cl.h>
#include <OpenCL/opencl.h>
#include <OpenCL/cl_gl.h>
#include <OpenCL/cl_ext.h>

#else

#include <CL/cl.h>
#include <CL/opencl.h>
#include <CL/cl_gl.h>
#include <CL/cl_ext.h>

#endif

//
// Get the devices that are available
//
cl_device_id* OclGetAvailableDevices(cl_platform_id platform, cl_device_type devType, cl_uint* numAvailableDevices);

//
// Get the device that has the maximum FLOPS in the current context
//
cl_device_id OclGetMaxFlopsDev(cl_context cxGPUContext);

//
// Print device name
//
void OclPrintDeviceName(cl_device_id device);

//
// Find the OpenCL platform that matches the "name"
//
cl_platform_id OclSelectPlatform(const char* name);

//
// Check OpenCL error
//
void OclCheckError(cl_int error);

#endif
