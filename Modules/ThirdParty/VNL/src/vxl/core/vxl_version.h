#ifndef vxl_version_h_
#define vxl_version_h_

//:
// \file
// \brief The VXL version.
// This version number applies to the whole vxl tree, not just the
// core libraries.

//: Major version number.
// This will only increase after major changes, or a large accumulation of
// significant smaller ones.
#define VXL_VERSION_MAJOR 1

//: Minor version number.
// This increments between versions. There is no
// "even = release, odd = development" pattern, or anything like that.
#define VXL_VERSION_MINOR 17

//: Patch number.
// This is only likely to be non-zero if a serious bug is found soon after the
// release of x.y.0. The VXL-maintainers do not usually distinguish between bug fixes
// and feature improvements, so the fix for most known bugs will first be released
// in x.(y+1).0
#define VXL_VERSION_PATCH 0

//: Version date.  This is updated every day.
// Formats are year=CCYY, month=MM, day=DD
#define VXL_VERSION_DATE_YEAR 2017
#define VXL_VERSION_DATE_MONTH 12
#define VXL_VERSION_DATE_DAY 08

//: Helper macros to create strings with the preprocessor.
#define VXL_VERSION_TO_STRING(s) VXL_VERSION_TO_STRING0(s)
#define VXL_VERSION_TO_STRING0(s) #s

//: Version number as a string literal.
// This is in the format "major.minor.patch".
#define VXL_VERSION_STRING \
  VXL_VERSION_TO_STRING(VXL_VERSION_MAJOR.VXL_VERSION_MINOR.VXL_VERSION_PATCH)

//: Version date as a string literal.
// This is in the format "CCYY-MM-DD".
#define VXL_VERSION_DATE \
  VXL_VERSION_TO_STRING(VXL_VERSION_DATE_YEAR-VXL_VERSION_DATE_MONTH-VXL_VERSION_DATE_DAY)

//: Version date accessible from preprocessor.
// This is an integer in the format CCYYMMDD.
#define VXL_VERSION_DATE_FULL \
  VXL_VERSION_DATE_FULL0(VXL_VERSION_DATE_YEAR, \
                         VXL_VERSION_DATE_MONTH, \
                         VXL_VERSION_DATE_DAY)
#define VXL_VERSION_DATE_FULL0(y,m,d) VXL_VERSION_DATE_FULL1(y,m,d)
#define VXL_VERSION_DATE_FULL1(y,m,d) y##m##d

#endif // vxl_version_h_
