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
    print if $printing;
    $printing = 1 if (m/END non-NrrdIO/);
}
