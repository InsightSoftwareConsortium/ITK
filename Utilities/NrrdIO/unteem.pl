#
# This helps in converting teem source files into NrrdIO source files,
# by changing the way #includes are done, and by excluding the lines
# delimited by "BEGIN non-NrrdIO" and "END non-NrrdIO"
#

$printing = 1;
while (<>) {
    $printing = 0 if (m/BEGIN non-NrrdIO/);
    s|#include "air.h"|#include "NrrdIO.h"|g;
    s|#include "biff.h"|#include "NrrdIO.h"|g;
    s|#include "nrrd.h"|#include "NrrdIO.h"|g;
    s|#include <teem(.*)>|#include "teem$1"|g;
    s|\/\* NrrdIO-hack-000 \*\/|#define TEEM_BUILD 1|g;
    s|#ifdef _WIN32 \/\* NrrdIO-hack-001 \*\/|#if 1|g;
    print if $printing;
    $printing = 1 if (m/END non-NrrdIO/);
}
