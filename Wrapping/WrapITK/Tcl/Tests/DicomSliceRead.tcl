#
#  Example on the use of DicomImageIO for reading a single DICOM slice, rescale
#  the intensities and save it in a different file format.
#
package require InsightToolkit

set reader [ itkImageFileReaderIUS2_New ]
set writer [ itkImageFileWriterIUC2_New ]

set filter [ itkRescaleIntensityImageFilterIUS2IUC2_New ]

set dicomIO [ itkDicomImageIO_New ]

$reader     SetImageIO [ $dicomIO GetPointer ]

$filter     SetInput [ $reader  GetOutput ]
$writer     SetInput [ $filter  GetOutput ]

$filter     SetOutputMinimum    0
$filter     SetOutputMaximum  255

$reader SetFileName [lindex $argv 0]
$writer SetFileName [lindex $argv 1]

$writer Update


exit


