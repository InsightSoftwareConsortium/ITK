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
#ifndef itkPyITKCommonCAPI_h
#define itkPyITKCommonCAPI_h

#include "itkSingleton.h"

/* Header file for the _ITKCommonPython C API exposed via an PyCapsule.
 *
 *  W A R N I N G
 *  -------------
 *
 * This file is not part of the ITK API.  It exists purely as an
 * implementation detail.  This header file may change from version to
 * version without notice, or even be removed.
 *
 * We mean it.
 */

#ifdef __cplusplus
extern "C" {
#endif

/* C API functions */
#define _ITKCommonPython_GetGlobalSingletonIndex_NUM 0
#define _ITKCommonPython_GetGlobalSingletonIndex_RETURN itk::SingletonIndex *
#define _ITKCommonPython_GetGlobalSingletonIndex_PROTO ()

/* Total number of C API pointers */
#define _ITKCommonPython_API_pointers 1


#ifdef _ITKCommonPython_MODULE
/* This section is used when compiling ITKCommonPython.cpp */

static _ITKCommonPython_GetGlobalSingletonIndex_RETURN _ITKCommonPython_GetInstance _ITKCommonPython_GetGlobalSingletonIndex_PROTO;

#else
/* This section is used in modules that use _ITKCommonPython's C API */

static void **_ITKCommonPython_API;

#define _ITKCommonPython_GetGlobalSingletonIndex \
 (*(_ITKCommonPython_GetGlobalSingletonIndex_RETURN (*)_ITKCommonPython_GetGlobalSingletonIndex_PROTO) _ITKCommonPython_API[_ITKCommonPython_GetGlobalSingletonIndex_NUM])
/* Return -1 on error, 0 on success.
 * PyCapsule_Import will set an exception if there's an error.
 */
static int
import__ITKCommonPython()
{
    _ITKCommonPython_API = (void **)PyCapsule_Import("_ITKCommonPython._C_API", 0);
    return (_ITKCommonPython_API != NULL) ? 0 : -1;
}

#endif

#ifdef __cplusplus
}
#endif

#endif // itkPyITKCommonCAPI_h
