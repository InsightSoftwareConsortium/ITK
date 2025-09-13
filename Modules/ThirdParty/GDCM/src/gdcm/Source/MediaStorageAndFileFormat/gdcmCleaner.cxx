/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmCleaner.h"
#include "gdcmAnonymizeEvent.h"
#include "gdcmAttribute.h"
#include "gdcmCSAHeader.h"
#include "gdcmDataSetHelper.h"
#include "gdcmDicts.h"
#include "gdcmEvent.h"
#include "gdcmGlobal.h"
#include "gdcmMEC_MR3.h"

#include "gdcmext/csa.h"
#include "gdcmext/mec_mr3.h"

namespace gdcm {

static const PrivateTag part15_table_E_1_1[] = {
    PrivateTag(0x0019, 0x0C, "SIEMENS MR HEADER"),
    PrivateTag(0x0019, 0x0D, "SIEMENS MR HEADER"),
    PrivateTag(0x0019, 0x0E, "SIEMENS MR HEADER"),
    PrivateTag(0x0019, 0x23, "GEMS_ACQU_01"),
    PrivateTag(0x0019, 0x24, "GEMS_ACQU_01"),
    PrivateTag(0x0019, 0x27, "GEMS_ACQU_01"),
    PrivateTag(0x0019, 0x27, "SIEMENS MR HEADER"),
    PrivateTag(0x0019, 0x9E, "GEMS_ACQU_01"),
    PrivateTag(0x0025, 0x01, "Philips ST80i"),
    PrivateTag(0x0025, 0x07, "GEMS_SERS_01"),
    PrivateTag(0x0043, 0x27, "GEMS_PARM_01"),
    PrivateTag(0x0043, 0x39, "GEMS_PARM_01"),
    PrivateTag(0x0043, 0x6F, "GEMS_PARM_01"),
    PrivateTag(0x0045, 0x01, "GEMS_HELIOS_01"),
    PrivateTag(0x0045, 0x02, "GEMS_HELIOS_01"),
    PrivateTag(0x0099, 0x01, "NQHeader"),
    PrivateTag(0x0099, 0x02, "NQHeader"),
    PrivateTag(0x0099, 0x04, "NQHeader"),
    PrivateTag(0x0099, 0x05, "NQHeader"),
    PrivateTag(0x0099, 0x10, "NQHeader"),
    PrivateTag(0x0099, 0x20, "NQHeader"),
    PrivateTag(0x0099, 0x21, "NQHeader"),
    PrivateTag(0x00E1, 0x21, "ELSCINT1"),
    PrivateTag(0x00E1, 0x50, "ELSCINT1"),
    PrivateTag(0x0119, 0x00, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x01, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x02, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x03, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x04, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x05, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x06, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x07, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x08, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x09, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x10, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x11, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x12, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x13, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0119, 0x21, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x00, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x02, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x03, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x04, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x05, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x06, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x07, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x08, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x09, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x10, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x11, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x12, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x20, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x21, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x22, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x29, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0129, 0x30, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0139, 0x01, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0149, 0x01, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0149, 0x02, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0149, 0x03, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x0199, 0x01, "NQLeft"),
    PrivateTag(0x0199, 0x02, "NQLeft"),
    PrivateTag(0x0199, 0x03, "NQLeft"),
    PrivateTag(0x0199, 0x04, "NQLeft"),
    PrivateTag(0x0199, 0x05, "NQLeft"),
    PrivateTag(0x0199, 0x06, "NQLeft"),
    PrivateTag(0x0199, 0x07, "NQLeft"),
    PrivateTag(0x0199, 0x08, "NQLeft"),
    PrivateTag(0x0199, 0x09, "NQLeft"),
    PrivateTag(0x0199, 0x0A, "NQLeft"),
    PrivateTag(0x0199, 0x0B, "NQLeft"),
    PrivateTag(0x0199, 0x0C, "NQLeft"),
    PrivateTag(0x0199, 0x0D, "NQLeft"),
    PrivateTag(0x0199, 0x0E, "NQLeft"),
    PrivateTag(0x0199, 0x0F, "NQLeft"),
    PrivateTag(0x0199, 0x10, "NQLeft"),
    PrivateTag(0x0199, 0x11, "NQLeft"),
    PrivateTag(0x0199, 0x12, "NQLeft"),
    PrivateTag(0x0199, 0x13, "NQLeft"),
    PrivateTag(0x0199, 0x14, "NQLeft"),
    PrivateTag(0x0199, 0x15, "NQLeft"),
    PrivateTag(0x0199, 0x16, "NQLeft"),
    PrivateTag(0x01E1, 0x26, "ELSCINT1"),
    PrivateTag(0x01F1, 0x01, "ELSCINT1"),
    PrivateTag(0x01F1, 0x07, "ELSCINT1"),
    PrivateTag(0x01F1, 0x26, "ELSCINT1"),
    PrivateTag(0x01F1, 0x27, "ELSCINT1"),
    PrivateTag(0x0299, 0x01, "NQRight"),
    PrivateTag(0x0299, 0x02, "NQRight"),
    PrivateTag(0x0299, 0x03, "NQRight"),
    PrivateTag(0x0299, 0x04, "NQRight"),
    PrivateTag(0x0299, 0x05, "NQRight"),
    PrivateTag(0x0299, 0x06, "NQRight"),
    PrivateTag(0x0299, 0x07, "NQRight"),
    PrivateTag(0x0299, 0x08, "NQRight"),
    PrivateTag(0x0299, 0x09, "NQRight"),
    PrivateTag(0x0299, 0x0A, "NQRight"),
    PrivateTag(0x0299, 0x0B, "NQRight"),
    PrivateTag(0x0299, 0x0C, "NQRight"),
    PrivateTag(0x0299, 0x0D, "NQRight"),
    PrivateTag(0x0299, 0x0E, "NQRight"),
    PrivateTag(0x0299, 0x0F, "NQRight"),
    PrivateTag(0x0299, 0x10, "NQRight"),
    PrivateTag(0x0299, 0x11, "NQRight"),
    PrivateTag(0x0299, 0x12, "NQRight"),
    PrivateTag(0x0299, 0x13, "NQRight"),
    PrivateTag(0x0299, 0x14, "NQRight"),
    PrivateTag(0x0299, 0x15, "NQRight"),
    PrivateTag(0x0299, 0x16, "NQRight"),
    PrivateTag(0x0903, 0x10, "GEIIS PACS"),
    PrivateTag(0x0903, 0x11, "GEIIS PACS"),
    PrivateTag(0x0903, 0x12, "GEIIS PACS"),
    PrivateTag(0x2001, 0x00, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x01, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x01, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x01, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x01, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x02, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x02, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x02, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x02, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x03, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x03, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x03, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x03, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x04, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x04, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x04, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x04, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x05, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x05, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x05, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x05, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x06, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x06, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x06, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x06, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x07, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x07, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x07, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x07, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x08, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x08, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x08, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x08, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x09, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x09, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x09, "Philips Imaging DD 129"),
    PrivateTag(0x2001, 0x0A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x0A, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x0A, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x0B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x0B, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x0B, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x0C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x0C, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x0D, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x0D, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x0E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x0E, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x0E, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x0F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x0F, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x0F, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x10, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x10, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x11, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x11, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x12, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x12, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x12, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x13, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x13, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x14, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x14, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x15, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x15, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x16, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x16, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x17, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x17, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x17, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x18, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x18, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x18, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x19, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x19, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x19, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x1A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x1A, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x1A, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x1B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x1B, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x1B, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x1C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x1C, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x1C, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x1D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x1D, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x1D, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x1E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x1E, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x1E, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x1F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x1F, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x1F, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x20, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x21, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x21, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x21, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x22, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x22, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x22, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x23, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x23, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x23, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x24, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x24, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x24, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x25, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x25, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x25, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x26, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x26, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x26, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x27, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x27, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x27, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x28, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x28, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x28, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x29, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x29, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x2A, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2A, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x2B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x2B, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2B, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x2C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x2C, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2C, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x2D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x2D, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2D, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x2E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x2E, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2E, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x2F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x2F, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x2F, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x30, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x30, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x30, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x31, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x31, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x31, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x32, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x32, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x32, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x33, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x33, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x34, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x34, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x35, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x35, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x36, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x36, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x36, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x37, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x37, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x37, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x38, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x38, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x38, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x39, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x39, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x39, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x3A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x3A, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x3A, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x3B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x3B, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x3B, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x3C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x3C, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x3C, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x3D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x3D, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x3D, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x3E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x3E, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x3E, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x3F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x3F, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x40, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x40, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x40, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x41, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x41, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x42, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x42, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x43, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x44, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x44, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x45, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x45, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x46, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x46, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x47, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x47, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x48, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x49, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x49, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x4A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x4A, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x4B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x4B, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x4C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x4C, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0x4D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x4E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x4F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x50, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x50, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x51, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x52, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x53, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x53, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x54, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x55, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x56, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x57, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x57, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x58, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x58, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x59, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x5A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x5A, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x5C, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x5D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x5D, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x5E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x5E, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x5F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x5F, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x60, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x61, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x62, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x63, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x63, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x64, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x64, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x65, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x65, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x66, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x66, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x67, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x67, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x68, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x68, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x69, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x6A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x6B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x6B, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x6D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x6E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x6F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x71, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x71, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x72, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x72, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x73, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x73, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x74, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x74, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x75, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x75, "Philips Imaging DD 002"),
    PrivateTag(0x2001, 0x76, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x77, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x79, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x7A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x7B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x7C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x7D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x7E, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x7F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x80, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x81, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x82, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x83, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x84, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x85, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x86, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x87, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x88, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x89, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x8A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x8B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x8C, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x90, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x91, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x92, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x93, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x94, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x9A, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x9B, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x9D, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0x9F, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xA1, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xA1, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA2, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xA2, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA3, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xA3, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA4, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xA4, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA5, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xA5, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA6, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA8, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xA9, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xAA, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xAB, "Philips Imaging DD 097"),
    PrivateTag(0x2001, 0xC0, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xC1, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xC2, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xC3, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xC5, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xC6, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xC7, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xCA, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xCB, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD0, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD1, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD2, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD3, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD4, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD5, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD6, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD7, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD8, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xD9, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xDA, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xDB, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xDC, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xDD, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xDE, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xDF, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xE9, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF1, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF2, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF3, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF4, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF5, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF6, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF7, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xF9, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xFB, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xFC, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xFD, "Philips Imaging DD 001"),
    PrivateTag(0x2001, 0xFF, "Philips Imaging DD 001"),
    PrivateTag(0x2005, 0x0D, "Philips MR Imaging DD 001"),
    PrivateTag(0x2005, 0x0E, "Philips MR Imaging DD 001"),
    PrivateTag(0x7053, 0x00, "Philips PET Private Group"),
    PrivateTag(0x7053, 0x09, "Philips PET Private Group"),
    PrivateTag(0x7E01, 0x01, "HOLOGIC, Inc."),
    PrivateTag(0x7E01, 0x02, "HOLOGIC, Inc."),
    PrivateTag(0x7E01, 0x10, "HOLOGIC, Inc."),
    PrivateTag(0x7E01, 0x11, "HOLOGIC, Inc."),
    PrivateTag(0x7E01, 0x12, "HOLOGIC, Inc."),
    PrivateTag(0x7FD1, 0x01, "SIEMENS SYNGO ULTRA-SOUND TOYON DATA STREAMING"),
    PrivateTag(0x7FD1, 0x01, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x7FD1, 0x09, "SIEMENS SYNGO ULTRA-SOUND TOYON DATA STREAMING"),
    PrivateTag(0x7FD1, 0x09, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x7FD1, 0x10, "SIEMENS SYNGO ULTRA-SOUND TOYON DATA STREAMING"),
    PrivateTag(0x7FD1, 0x10, "SIEMENS Ultrasound SC2000"),
    PrivateTag(0x7FD1, 0x11, "SIEMENS SYNGO ULTRA-SOUND TOYON DATA STREAMING"),
    PrivateTag(0x7FD1, 0x11, "SIEMENS Ultrasound SC2000"),
};

static inline bool in_part15(const PrivateTag &pt) {
  static const size_t len =
      sizeof(part15_table_E_1_1) / sizeof *part15_table_E_1_1;
  for (size_t i = 0; i < len; ++i) {
    if (part15_table_E_1_1[i] == pt) {
      return true;
    }
  }
  return false;
}

typedef std::set<DataElement> DataElementSet;
typedef DataElementSet::const_iterator ConstIterator;

struct Cleaner::impl {
  std::set<CodedEntryData> coded_entry_datas;
  std::set<DPath> preserve_dpaths;
  std::set<DPath> empty_dpaths;
  std::set<DPath> remove_dpaths;
  std::set<DPath> scrub_dpaths;
  std::set<Tag> empty_tags;
  std::set<PrivateTag> empty_privatetags;
  std::set<Tag> remove_tags;
  std::set<PrivateTag> remove_privatetags;
  std::set<Tag> scrub_tags;
  std::set<PrivateTag> scrub_privatetags;
  std::set<VR> empty_vrs;
  std::set<VR> remove_vrs;
  bool AllMissingPrivateCreator;
  bool AllGroupLength;
  bool AllIllegal;
  bool WhenScrubFails;
  impl()
      : AllMissingPrivateCreator(true),
        AllGroupLength(true),
        AllIllegal(true),
        WhenScrubFails(false) {}

  // CODE_MEANING:
  // In general, there are no Code Sequence Attributes in this table, since it
  // is usually safe to assume that coded sequence entries, including private
  // codes, do not contain identifying information. Exceptions are codes for
  // providers and staff.
  // https://dicom.nema.org/medical/dicom/current/output/chtml/part15/chapter_E.html#para_d0913a6e-9386-42b2-b2b3-e87c84cdb304
  enum ACTION { NONE, EMPTY, REMOVE, SCRUB, CODE_MEANING };
  enum ACTION ComputeAction(File const &file, DataSet &ds,
                            const DataElement &de, VR const &ref_dict_vr,
                            const std::string &tag_path);

  bool ProcessDataSet(Subject &s, File &file, DataSet &ds,
                      const std::string &tag_path);

  template <typename T>
  bool CheckVRBeforeInsert(std::set<VR> &empty_or_remove_vrs, T const &t,
                           std::set<T> &set) {
    if (empty_or_remove_vrs.empty()) {
      set.insert(t);
      return true;
    } else {
      // Let's check if VR of tag is already contained in VR
      static const Global &g = GlobalInstance;
      static const Dicts &dicts = g.GetDicts();
      const DictEntry &entry = dicts.GetDictEntry(t);
      const VR &refvr = entry.GetVR();
      if (empty_or_remove_vrs.find(refvr) != empty_or_remove_vrs.end()) {
        gdcmWarningMacro(
            "Tag: " << t << " is also cleanup with VR cleaning. skipping");
      } else if (refvr == VR::INVALID) {
        gdcmWarningMacro("inserting unknown tag "
                         << t << ". no check on VR is done");
        set.insert(t);
      } else {
        set.insert(t);
      }
      return true;
    }
  }
  bool Empty(Tag const &t) {
    if (t.IsPublic() && !t.IsGroupLength()) {
      return CheckVRBeforeInsert(empty_vrs, t, empty_tags);
    }
    return false;
  }
  bool Empty(PrivateTag const &pt) {
    const char *owner = pt.GetOwner();
    if (pt.IsPrivate() && *owner) {
      if (in_part15(pt)) {
        gdcmErrorMacro("Cannot add Part 15 attribute for now");
        return false;
      }
      return CheckVRBeforeInsert(empty_vrs, pt, empty_privatetags);
    }
    return false;
  }
  bool Empty(DPath const &dpath) {
    empty_dpaths.insert(dpath);
    return true;
  }
  bool Empty(VR const &vr) {
    if (vr == VR::PN) {
      empty_vrs.insert(vr);
      return true;
    }
    return false;
  }

  bool Remove(Tag const &t) {
    if (t.IsPublic() && !t.IsGroupLength()) {
      return CheckVRBeforeInsert(remove_vrs, t, remove_tags);
    }
    return false;
  }
  bool Remove(PrivateTag const &pt) {
    const char *owner = pt.GetOwner();
    if (pt.IsPrivate() && *owner) {
      if (in_part15(pt)) {
        gdcmErrorMacro("Cannot add Part 15 attribute for now");
        return false;
      }
      return CheckVRBeforeInsert(remove_vrs, pt, remove_privatetags);
    }
    return false;
  }
  bool Remove(DPath const &dpath) {
    remove_dpaths.insert(dpath);
    return true;
  }
  bool Remove(VR const &vr) {
    if (vr == VR::PN) {
      remove_vrs.insert(vr);
      return true;
    }
    return false;
  }

  bool Scrub(Tag const & /*t*/) { return false; }
  bool Scrub(PrivateTag const &pt) {
    static const PrivateTag &csa1 = CSAHeader::GetCSAImageHeaderInfoTag();
    static const PrivateTag &csa2 = CSAHeader::GetCSASeriesHeaderInfoTag();
    const PrivateTag mec_mr3(0x700d, 0x08, "TOSHIBA_MEC_MR3");
    static const PrivateTag &pmtf1 = gdcm::MEC_MR3::GetPMTFInformationDataTag();
    static const PrivateTag &pmtf2 = gdcm::MEC_MR3::GetToshibaMECMR3Tag();
    static const PrivateTag &pmtf3 = gdcm::MEC_MR3::GetCanonMECMR3Tag();

    if (pt == csa1 || pt == csa2 || pt == mec_mr3 || pt == pmtf1 ||
        pt == pmtf2 || pt == pmtf3) {
      scrub_privatetags.insert(pt);
      return true;
    }
    return false;
  }
  bool Scrub(DPath const &dpath) {
    scrub_dpaths.insert(dpath);
    return true;
  }

  bool Scrub(VR const & /*vr*/) { return false; }

  bool ReplaceCodeMeaning(CodedEntryData const &ced) { 
      coded_entry_datas.insert(ced);
      return true; 
  }
  
  bool Preserve(DPath const &dpath) {
    preserve_dpaths.insert(dpath);
    return true;
  }

  void RemoveAllMissingPrivateCreator(bool remove) {
    AllMissingPrivateCreator = remove;
  }
  bool RemoveMissingPrivateCreator(Tag const & /*t*/) { return false; }
  void RemoveAllGroupLength(bool remove) { AllGroupLength = remove; }
  void RemoveAllIllegal(bool remove) { AllIllegal = remove; }
  void EmptyWhenScrubFails(bool empty) { WhenScrubFails = empty; }

  bool CleanCSAImage(DataSet &ds, const DataElement &de);
  bool CleanCSASeries(DataSet &ds, const DataElement &de);
};

static VR ComputeDictVR(File &file, DataSet &ds, DataElement const &de) {
  VR dict_vr = de.GetVR();
  const Tag &tag = de.GetTag();
  bool compute_dict_vr = true;
  if (tag.IsPublic() || tag.IsGroupLength() || tag.IsPrivateCreator()) {
  } else {
    const PrivateTag pt = ds.GetPrivateTag(tag);
    const char *owner = pt.GetOwner();
    gdcm_assert(owner);
    compute_dict_vr = *owner != 0;
  }
  if (compute_dict_vr) dict_vr = DataSetHelper::ComputeVR(file, ds, tag);

  if (de.GetVR() == VR::SQ) {
    gdcm_assert(dict_vr != VR::UN);
    if (!dict_vr.Compatible(de.GetVR())) {
      gdcmErrorMacro("Impossible. Dict states VR is: "
                     << dict_vr << " which is impossible for SQ");
      dict_vr = VR::SQ;
    }
  }
  if (dict_vr != VR::SQ) {
    if (de.GetVL().IsUndefined()) {
      if (dict_vr == VR::UN) {
        // implicit dataset, where SQ is undefined length
      } else {
        const Tag pixelData(0x7fe0, 0x0010);
        gdcm_assert(dict_vr == VR::OB);
        if (tag != pixelData) {
          gdcmErrorMacro("Impossible happen: " << de);
          return VR::SQ;
        }
      }
    }
  }
  return dict_vr;
}

static inline std::string tostring(uint16_t const val, int const width = 4) {
  std::ostringstream oss;
  oss.setf(std::ios::right);
  oss << std::hex << std::setw(width) << std::setfill('0') << val;
  return oss.str();
}

static std::vector<std::string> tag2strings(DataSet const &ds, Tag const &tag) {
  std::vector<std::string> ret;
  if (tag.IsPublic() || tag.IsPrivateCreator() || tag.IsGroupLength()) {
    ret.push_back(tostring(tag.GetGroup()));
    ret.push_back(tostring(tag.GetElement()));
  } else {
    const PrivateTag pt = ds.GetPrivateTag(tag);
    ret.push_back(tostring(pt.GetGroup()));
    ret.push_back(tostring(pt.GetElement(), 2));
    ret.push_back(pt.GetOwner());
  }
  return ret;
}

template <typename T>
static void print_contents(std::ostream &oss, const std::vector<T> &v,
                           const char *const separator = ",") {
  if (!v.empty()) {
    std::copy(v.begin(), --v.end(), std::ostream_iterator<T>(oss, separator));
    oss << v.back();
  }
}

static bool isAllZero(const char *buffer, size_t len) {
  while (len-- > 0) {
    if (buffer[len] != 0) return false;
  }
  return true;
}

enum CSAImageHeaderType {
  IMAGE_UNK = -1,
  HG_IRECORD = 0,  // label:=Standard, no PHI
  IMAGE_NUM_4,     // SV10
  IMAGE_MR,        // SV10
  NONIMAGE_NUM_4,  // SV10
  NUC_FLOOD,       // binary data, no PHI
  PET_NUM_4,       // SV10
  SOM_5,           // Explicit LE, "END!      ", no PHI
};

static const char *CSAImageHeaderTypeStrings[]{
    "HG IRECORD",      //
    "IMAGE NUM 4",     //
    "MR",              //
    "NONIMAGE NUM 4",  //
    "NUC FLOOD",       //
    "PET NUM 4",       //
    "SOM 5"            //
};

enum CSASeriesHeaderType {
  SERIES_UNK = -1,
  HG_RECORD_SERIES =
      0,           // => 0029,xx40 contains a sequence with "HG RECORD", no PHI
  SERIES_MR,       // SV10
  ParameterBlock,  // <?xml version=\"1.0\" ?>, no PHI
  PET_REPLAY_PARAM,  // <pds><com>, no PHI
  PT,                // SV10
  SOM_7_DEV,         // ORIGINALSERIES=, no PHI
};

static const char *CSASeriesHeaderTypeStrings[]{
    "HG RECORD SERIES",  // HG_RECORD_SERIES
    "MR",                // SERIES_MR
    "ParameterBlock",    // ParameterBlock
    "PET_REPLAY_PARAM",  //
    "PT",                //
    "SOM 7 DEV"          // SOM_7_DEV
};

template <typename T, int N>
static T GetCSAType(std::string &ref, const DataSet &ds, const PrivateTag &pt,
                    const char *(&array)[N]) {
  T series_type = (T)-1;  // UNK
  ref = "";
  if (ds.FindDataElement(pt)) {
    const gdcm::DataElement &de1 = ds.GetDataElement(pt);
    Element<VR::CS, VM::VM1> el = {};
    el.SetFromDataElement(de1);
    ref = el.GetValue().Trim();
    for (int i = 0; i < N; i++) {
      if (strcmp(array[i], ref.c_str()) == 0) {
        series_type = (T)(i);
      }
    }
  }
  return series_type;
}

// byte-swapped memcmp implementation:
static inline int bs_memcmp(const void *s1, const void *s2, size_t n) {
  size_t i;
  const unsigned char *us1 = (const unsigned char *)s1;
  const unsigned char *us2 = (const unsigned char *)s2;
  gdcm_assert(n % 2 == 0);

  for (i = 0; i < n; i += 2, us1 += 2, us2 += 2) {
    if (*us1 < *(us2 + 1)) {
      return -1;
    } else if (*us1 > *(us2 + 1)) {
      return 1;
    }

    if (*(us1 + 1) < *us2) {
      return -1;
    } else if (*(us1 + 1) > *us2) {
      return 1;
    }
  }
  return 0;
}

static inline bool is_signature(const ByteValue *bv, const char *str) {
  const size_t len = strlen(str);
  if (bv->GetLength() >= len && memcmp(bv->GetPointer(), str, len) == 0) {
    return true;
  }
  return false;
}

static inline bool bs_is_signature(const ByteValue *bv, const char *str) {
  const size_t len = strlen(str);
  if (bv->GetLength() >= len && bs_memcmp(bv->GetPointer(), str, len) == 0) {
    return true;
  }
  return false;
}

bool Cleaner::impl::CleanCSAImage(DataSet &ds, const DataElement &de) {
  const ByteValue *bv = de.GetByteValue();
  // fast path:
  if (!bv) return true;

  CSAImageHeaderType image_type = IMAGE_UNK;
  std::string ref;
  {
    const PrivateTag ihtTag(0x0029, 0x08, "SIEMENS CSA HEADER");
    image_type = GetCSAType<CSAImageHeaderType>(ref, ds, ihtTag,
                                                CSAImageHeaderTypeStrings);
  }
  static const char sv10[] = "SV10\4\3\2\1";  // 8
  // what if a dumb anonymizer removed the CSA Image Header Type:
  bool isSV10 = false;
  if (image_type == IMAGE_UNK) {
    if (is_signature(bv, sv10)) {
      if (!ref.empty()) {
        gdcmWarningMacro(
            "Please report. SV10 Header found for new type: " << ref);
      }
      isSV10 = true;
    }
  }

  // easy case: recognized keywords:
  if (image_type == IMAGE_NUM_4        // MR Image Storage / NUMARIS/4
      || image_type == IMAGE_MR        // Enhanced SR Storage
      || image_type == NONIMAGE_NUM_4  // CSA Non-Image Storage
      || image_type == PET_NUM_4  // Positron Emission Tomography Image Storage
      || isSV10) {
    DataElement clean(de.GetTag());
    clean.SetVR(de.GetVR());
    std::vector<char> v;
    v.resize(bv->GetLength());
    if (csa_memcpy(v.data(), bv->GetPointer(), bv->GetLength())) {
      clean.SetByteValue(v.data(), (uint32_t)v.size());
      ds.Replace(clean);
      return true;
    }
    // we failed to clean CSA, let's check possible well known errors
    if (bs_is_signature(bv, sv10)) {
      gdcmWarningMacro("Found byte-swapped SV10. Skipping.");
      return true;
    } else if (isAllZero(bv->GetPointer(), bv->GetLength())) {
      gdcmDebugMacro("Zero-out CSA header");
      return true;
    }
    // fallback logic:
    if (WhenScrubFails && is_signature(bv, sv10)) {
      // so SV10 header has been identified, but we failed to 'scrub', let's
      // empty it:
      ds.Replace(clean);
      return true;
    }
    gdcmErrorMacro("Failure to call CleanCSAImage");
    return false;
  }
  // else
  // add a dummy check for SV10 signature
  if (is_signature(bv, sv10)) {
    gdcmErrorMacro("Failure to clean SV10 Header for type: " << ref);
    return false;
  }
  return true;
}

bool Cleaner::impl::CleanCSASeries(DataSet &ds, const DataElement &de) {
  const ByteValue *bv = de.GetByteValue();
  // fast path:
  if (!bv) return true;

  CSASeriesHeaderType series_type = SERIES_UNK;
  std::string ref;
  {
    const PrivateTag shtTag(0x0029, 0x18, "SIEMENS CSA HEADER");
    series_type = GetCSAType<CSASeriesHeaderType>(ref, ds, shtTag,
                                                  CSASeriesHeaderTypeStrings);
  }
  static const char sv10[] = "SV10\4\3\2\1";  // 8
  // what if a dumb anonymizer removed the CSA Series Header Type:
  bool isSV10 = false;
  if (series_type == SERIES_UNK) {
    if (is_signature(bv, sv10)) {
      if (!ref.empty()) {
        gdcmWarningMacro(
            "Please report. SV10 Header found for new type: " << ref);
      }
      isSV10 = true;
    }
  }

  // easy case: recognized keywords:
  if (series_type == PT || series_type == SERIES_MR || isSV10) {
    DataElement clean(de.GetTag());
    clean.SetVR(de.GetVR());
    std::vector<char> v;
    v.resize(bv->GetLength());
    if (csa_memcpy(v.data(), bv->GetPointer(), bv->GetLength())) {
      clean.SetByteValue(v.data(), (uint32_t)v.size());
      ds.Replace(clean);
      return true;
    }
    // we failed to clean CSA, let's check possible well known errors
    if (bs_is_signature(bv, sv10)) {
      gdcmWarningMacro("Found byte-swapped SV10. Skipping.");
      return true;
    } else if (isAllZero(bv->GetPointer(), bv->GetLength())) {
      gdcmDebugMacro("Zero-out CSA header");
      return true;
    }
    // fallback logic:
    if (WhenScrubFails && is_signature(bv, sv10)) {
      // so SV10 header has been identified, but we failed to 'scrub', let's
      // empty it:
      ds.Replace(clean);
      return true;
    }
    gdcmErrorMacro("Failure to call CleanCSASeries");
    return false;
  }
  // else
  // add a dummy check for SV10 signature
  if (is_signature(bv, sv10)) {
    gdcmErrorMacro("Failure to clean SV10 Header for type: " << ref);
    return false;
  }
  return true;
}

static bool CleanMEC_MR3(DataSet &ds, const DataElement &de) {
  const ByteValue *bv = de.GetByteValue();
  // fast path:
  if (!bv) return true;

  DataElement clean(de.GetTag());
  clean.SetVR(de.GetVR());
  std::vector<char> v;
  v.resize(bv->GetLength());
  {
    // check a pseudo magic value here:
    uint32_t magic = 0xffff;
    if (bv->GetLength() > 4) {
      memcpy(&magic, bv->GetPointer(), sizeof magic);
    }
    if (magic > 512) {
      gdcmWarningMacro("Cannot handle MEC_MR3");
      return true;
    }
  }
  if (mec_mr3_memcpy(v.data(), bv->GetPointer(), bv->GetLength())) {
    clean.SetByteValue(v.data(), (uint32_t)v.size());
    ds.Replace(clean);
    return true;
  }
  gdcmErrorMacro("Failure to call CleanMEC_MR3");
  return false;
}

static bool CleanPMTF(DataSet &ds, const DataElement &de) {
  const ByteValue *bv = de.GetByteValue();
  // fast path:
  if (!bv) return true;
  const char *input = bv->GetPointer();
  const size_t len = bv->GetLength();

  gdcm::Cleaner cleaner;
  gdcm::File &file = cleaner.GetFile();
  gdcm::DataSet &revds = file.GetDataSet();
  try {
    std::istringstream is;
    {
      std::vector<char> copy(input, input + len);
      std::reverse(copy.begin(), copy.end());
      std::string dup(copy.data(), copy.size());
      is.str(dup);
    }

    // FIXME gdcm::Cleaner will by default change defined length SQ into undef
    // length...there is a risk of incompatibily with vendor
    gdcm::FileMetaInformation &fmi = file.GetHeader();
    fmi.SetDataSetTransferSyntax(gdcm::TransferSyntax::ExplicitVRLittleEndian);
    revds.Read<gdcm::ExplicitDataElement, gdcm::SwapperNoOp>(is);
  } catch (...) {
    gdcmDebugMacro("Unhanded file format");
    return true;
  }

  bool success = true;
  try {
    gdcm::VR vr = VR::PN;
    cleaner.Empty(vr);
    if (!cleaner.Clean()) {
      success = false;
    } else {
      std::ostringstream os;
      revds.Write<gdcm::ExplicitDataElement, gdcm::SwapperNoOp>(os);
      const std::string str = os.str();
      std::vector<char> v(str.c_str(), str.c_str() + str.size());
      std::reverse(v.begin(), v.end());

      DataElement clean(de.GetTag());
      clean.SetVR(de.GetVR());
      clean.SetByteValue(v.data(), (uint32_t)v.size());
      ds.Replace(clean);
    }
  } catch (...) {
    success = false;
  }

  if (success) {
    return true;
  }
  gdcmErrorMacro("Failure to call CleanPMTF");
  return false;
}

static DPath ConstructDPath(std::string const &tag_path, const DataSet &ds,
                            const Tag &tag) {
  DPath dpath;
  std::ostringstream oss;
  oss << tag_path;
  const std::vector<std::string> tag_strings = tag2strings(ds, tag);
  print_contents(oss, tag_strings);
  dpath.ConstructFromString(oss.str().c_str());
  return dpath;
}

static bool IsDPathInSet(std::set<DPath> const &aset, DPath const dpath) {
  bool found = false;
  for (std::set<DPath>::const_iterator it = aset.begin();
       found == false && it != aset.end(); ++it) {
    found = it->Match(dpath);
  }

  return found;
}

Cleaner::impl::ACTION Cleaner::impl::ComputeAction(
    File const & /*file*/, DataSet &ds, const DataElement &de,
    VR const &ref_dict_vr, const std::string &tag_path) {
  const Tag &tag = de.GetTag();
  // Group Length & Illegal cannot be preserved so it is safe to do them now:
  if (tag.IsGroupLength()) {
    if (AllGroupLength) return Cleaner::impl::REMOVE;
  } else if (tag.IsIllegal()) {
    if (AllIllegal) return Cleaner::impl::REMOVE;
  }

  if (tag.IsPublic()) {
    // CodeMeaning
    if (tag == gdcm::Tag(0x0008, 0x0104)) {
      Attribute<0x0008, 0x0100> codeValue{};
      codeValue.SetFromDataSet(ds);
      Attribute<0x0008, 0x0102> codingSchemeDesignator{};
      codingSchemeDesignator.SetFromDataSet(ds);
      Attribute<0x0008, 0x0103> codingSchemeVersion{};
      codingSchemeVersion.SetFromDataSet(ds);
      const std::string codeValueStr = codeValue.GetValue().Trim();
      const std::string codingSchemeDesignatorStr = codingSchemeDesignator.GetValue().Trim();
      const std::string codingSchemeVersionStr = codingSchemeVersion.GetValue().Trim();
      {
        CodedEntryData ced;
        ced = std::make_tuple(codeValueStr, codingSchemeDesignatorStr,
                              codingSchemeVersionStr);
        if( coded_entry_datas.find(ced) != coded_entry_datas.end() )
          return Cleaner::impl::CODE_MEANING;
      }
        
      return Cleaner::impl::NONE;
    }
    const DPath dpath = ConstructDPath(tag_path, ds, tag);

    // Preserve
    if (IsDPathInSet(preserve_dpaths, dpath)) return Cleaner::impl::NONE;
    // Scrub
    if (scrub_tags.find(tag) != scrub_tags.end() ||
        IsDPathInSet(scrub_dpaths, dpath)) {
      return Cleaner::impl::SCRUB;
    }
    // Empty
    if (empty_tags.find(tag) != empty_tags.end() ||
        IsDPathInSet(empty_dpaths, dpath)) {
      gdcm_assert(!tag.IsGroupLength());
      gdcm_assert(!tag.IsPrivateCreator());
      gdcm_assert(ds.FindDataElement(tag));
      return Cleaner::impl::EMPTY;
    }
    // Remove
    if (remove_tags.find(tag) != remove_tags.end() ||
        IsDPathInSet(remove_dpaths, dpath)) {
      return Cleaner::impl::REMOVE;
    }
  }

  if (tag.IsPrivate() && !tag.IsPrivateCreator() && !tag.IsGroupLength()) {
    const PrivateTag pt = ds.GetPrivateTag(tag);
    const char *owner = pt.GetOwner();
    gdcm_assert(owner);
    if (*owner == 0 && AllMissingPrivateCreator) {
      return Cleaner::impl::REMOVE;
    }
    // At this point we have a private creator, it makes sense to check for
    // preserve: Preserve
    const DPath dpath = ConstructDPath(tag_path, ds, tag);
    if (IsDPathInSet(preserve_dpaths, dpath)) return Cleaner::impl::NONE;
    // Scrub
    if (scrub_privatetags.find(pt) != scrub_privatetags.end() ||
        IsDPathInSet(scrub_dpaths, dpath)) {
      return Cleaner::impl::SCRUB;
    }
    // Empty
    if (empty_privatetags.find(pt) != empty_privatetags.end() ||
        IsDPathInSet(empty_dpaths, dpath)) {
      return Cleaner::impl::EMPTY;
    }
    // Remove
    if (remove_privatetags.find(pt) != remove_privatetags.end() ||
        IsDPathInSet(remove_dpaths, dpath)) {
      return Cleaner::impl::REMOVE;
    }
  }

  // VR cleanup
  if (!empty_vrs.empty() || !remove_vrs.empty()) {
    VR vr = de.GetVR();
    gdcm_assert(ref_dict_vr != VR::INVALID);
    // be careful with vr handling since we must always prefer the one from
    // the dict in case of attribute written as 'OB' but dict states 'PN':
    if (ref_dict_vr != VR::UN /*&& ref_dict_vr != VR::INVALID*/) {
      // we want to clean VR==PN; but this is a problem for implicit transfer
      // syntax, so let's be nice to the user and prefer dict_vr. however for
      // explicit, do not assume value in dict can take over the read VR
      if (vr == VR::UN || vr == VR::INVALID) {
        vr = ref_dict_vr;
      }
      if (vr != ref_dict_vr) {
        // gdcm_assert(vr == VR::OB || vr == VR::OW);
        vr = ref_dict_vr;
      }
    }
    // Empty
    if (empty_vrs.find(vr) != empty_vrs.end()) {
      return Cleaner::impl::EMPTY;
    }
    // Remove
    if (remove_vrs.find(vr) != remove_vrs.end()) {
      return Cleaner::impl::REMOVE;
    }
  }

  // default action:
  return Cleaner::impl::NONE;
}

bool Cleaner::impl::ProcessDataSet(Subject &subject, File &file, DataSet &ds,
                                   const std::string &tag_path) {
  subject.InvokeEvent(IterationEvent());
  ConstIterator it = ds.GetDES().begin();

  for (; it != ds.GetDES().end(); /*++it*/) {
    const DataElement &de = *it;
    ++it;  // 'Remove/Empty' may invalidate iterator
    const Tag &tag = de.GetTag();
    AnonymizeEvent ae;
    ae.SetTag(tag);

    VR dict_vr = ComputeDictVR(file, ds, de);
    Cleaner::impl::ACTION action =
        Cleaner::impl::ComputeAction(file, ds, de, dict_vr, tag_path);

    if (action == Cleaner::impl::NONE) {
      // nothing to do, but recurse in nested-dataset:
      if (dict_vr == VR::SQ) {
        SmartPointer<SequenceOfItems> sqi = de.GetValueAsSQ();
        if (sqi) {
          SequenceOfItems::SizeType s = sqi->GetNumberOfItems();
          for (SequenceOfItems::SizeType i = 1; i <= s; ++i) {
            Item &item = sqi->GetItem(i);

            DataSet &nestedds = item.GetNestedDataSet();
            const std::vector<std::string> tag_strings = tag2strings(ds, tag);

            std::ostringstream os;
            os << tag_path;  // already padded with trailing '/'
            print_contents(os, tag_strings);
            os << '/';
            os << '*';  // no need for item numbering
            os << '/';

            if (!ProcessDataSet(subject, file, nestedds, os.str())) {
              gdcmErrorMacro("Error processing Item #" << i);
              return false;
            }
            // Simple memcmp to avoid recomputation of Item Length: make them
            // undefined length. TODO would be nice to only do this when
            // strictly needed.
            item.SetVLToUndefined();
          }
          // Simple mechanism to avoid recomputation of Sequence Length: make
          // them undefined length
          DataElement dup(de.GetTag());
          dup.SetVR(VR::SQ);
          dup.SetValue(*sqi);
          dup.SetVLToUndefined();
          ds.Replace(dup);
        } else {
          // SmartPointer<SequenceOfItems> sqi = de.GetValueAsSQ();
          if (!de.IsEmpty()) {
            gdcmWarningMacro(
                "Please report. Dictionary states this should be a SQ. But "
                "we "
                "failed to load it as such. Passing-through as-is"
                << de);
          }
        }
      }
    } else if (action == Cleaner::impl::EMPTY) {
      DataElement clean(de.GetTag());
      clean.SetVR(de.GetVR());
      ds.Replace(clean);
      subject.InvokeEvent(ae);
    } else if (action == Cleaner::impl::REMOVE) {
      ds.Remove(tag);
      subject.InvokeEvent(ae);
    } else if (action == Cleaner::impl::SCRUB) {
      const PrivateTag pt = ds.GetPrivateTag(tag);

      static const PrivateTag &csa1 = CSAHeader::GetCSAImageHeaderInfoTag();
      static const PrivateTag &csa2 = CSAHeader::GetCSASeriesHeaderInfoTag();
      const PrivateTag mec_mr3(0x700d, 0x08, "TOSHIBA_MEC_MR3");
      static const PrivateTag &pmtf1 =
          gdcm::MEC_MR3::GetPMTFInformationDataTag();
      static const PrivateTag &pmtf2 = gdcm::MEC_MR3::GetToshibaMECMR3Tag();
      static const PrivateTag &pmtf3 = gdcm::MEC_MR3::GetCanonMECMR3Tag();

      if (pt == csa1) {
        const bool ret = CleanCSAImage(ds, de);
        if (!ret) return false;
      } else if (pt == csa2) {
        const bool ret = CleanCSASeries(ds, de);
        if (!ret) return false;
      } else if (pt == mec_mr3) {
        const bool ret = CleanMEC_MR3(ds, de);
        if (!ret) return false;
      } else if (pt == pmtf1) {
        const bool ret = CleanPMTF(ds, de);
        if (!ret) return false;
      } else if (pt == pmtf2) {
        const bool ret = CleanPMTF(ds, de);
        if (!ret) return false;
      } else if (pt == pmtf3) {
        const bool ret = CleanPMTF(ds, de);
        if (!ret) return false;
      } else {
        gdcmErrorMacro(" not implemented");
        return false;
      }
      subject.InvokeEvent(ae);
    } else if (action == Cleaner::impl::CODE_MEANING) {
      // Cannot simply 'empty' public data element in this case:
      // Code Meaning (0008, 0104) 1 Text that conveys the meaning of the Coded Entry.
      // https://dicom.nema.org/medical/dicom/current/output/chtml/part03/sect_8.8.html#table_8.8-1a
      // Action Code 'Z' states:
      // replace with a zero length value, or a non - zero length value that may be a dummy value and consistent with the VR
      DataElement clean(de.GetTag());
      clean.SetVR(de.GetVR());
      static const char clean_str[] = "ALTERED CODE MEANING"; // len=20
      clean.SetByteValue(clean_str, static_cast<uint32_t>(strlen(clean_str)));
      ds.Replace(clean);
      subject.InvokeEvent(ae);
    } else {
      gdcmErrorMacro("Missing handling of action: " << action);
      return false;
    }
  }
  return true;
}

Cleaner::Cleaner() : F(new File), pimpl(new impl) {}

Cleaner::~Cleaner() { delete pimpl; }

bool Cleaner::Empty(Tag const &t) { return pimpl->Empty(t); }
bool Cleaner::Empty(PrivateTag const &pt) { return pimpl->Empty(pt); }
bool Cleaner::Empty(DPath const &dpath) { return pimpl->Empty(dpath); }
bool Cleaner::Empty(VR const &vr) { return pimpl->Empty(vr); }

bool Cleaner::Remove(Tag const &t) { return pimpl->Remove(t); }
bool Cleaner::Remove(PrivateTag const &pt) { return pimpl->Remove(pt); }
bool Cleaner::Remove(DPath const &dpath) { return pimpl->Remove(dpath); }
bool Cleaner::Remove(VR const &vr) { return pimpl->Remove(vr); }

bool Cleaner::Scrub(Tag const &t) { return pimpl->Scrub(t); }
bool Cleaner::Scrub(PrivateTag const &pt) { return pimpl->Scrub(pt); }
bool Cleaner::Scrub(DPath const &dpath) { return pimpl->Scrub(dpath); }
bool Cleaner::Scrub(VR const &vr) { return pimpl->Scrub(vr); }

bool Cleaner::ReplaceCodeMeaning(Cleaner::CodedEntryData const &ced) {
  return pimpl->ReplaceCodeMeaning(ced);
}

bool Cleaner::Preserve(DPath const &dpath) { return pimpl->Preserve(dpath); }

void Cleaner::RemoveAllMissingPrivateCreator(bool remove) {
  pimpl->RemoveAllMissingPrivateCreator(remove);
}
bool Cleaner::RemoveMissingPrivateCreator(Tag const &t) {
  return pimpl->RemoveMissingPrivateCreator(t);
}
void Cleaner::RemoveAllGroupLength(bool remove) {
  pimpl->RemoveAllGroupLength(remove);
}
void Cleaner::RemoveAllIllegal(bool remove) { pimpl->RemoveAllIllegal(remove); }
void Cleaner::EmptyWhenScrubFails(bool empty) {
  pimpl->EmptyWhenScrubFails(empty);
}

bool Cleaner::Clean() {
  DataSet &ds = F->GetDataSet();
  this->InvokeEvent(StartEvent());
  const bool ret = pimpl->ProcessDataSet(*this, *F, ds, "/");
  this->InvokeEvent(EndEvent());
  return ret;
}

}  // end namespace gdcm
