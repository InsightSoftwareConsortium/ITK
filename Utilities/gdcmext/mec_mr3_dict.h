/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef MEC_MR3_DICT_H
#define MEC_MR3_DICT_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void check_mec_mr3_dict(void);
bool check_mec_mr3_info(uint8_t group, uint32_t key, uint32_t type);
const char *get_mec_mr3_info_name(uint8_t group, uint32_t key);

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // MEC_MR3_DICT_H
