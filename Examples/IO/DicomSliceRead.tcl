#
#  Example on the use of DicomImageIO for reading a single DICOM slice, rescale
#  the intensities and save it in a different file format.
#
package require InsightToolkit

set reader [ itkImageFileReaderUS2_New ]
set writer [ itkImageFileWriterUC2_New ]

set filter [ itkRescaleIntensityImageFilterUS2UC2_New ]

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


