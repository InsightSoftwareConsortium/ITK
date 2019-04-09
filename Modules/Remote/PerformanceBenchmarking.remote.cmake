# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(PerformanceBenchmarking
"New classes increase operating system process priority to
minimize the impact of other processes running on the system.

These classes are used by a suite of example ITK benchmarks to
quantify toolkit performance.

For more information, see::

  McCormick M., Kang H.J., Barre S.
  Performance Benchmarking the Insight Toolkit
  The Insight Journal. January-December. 2016.
  http://hdl.handle.net/10380/3557
  http://insight-journal.org/browse/publication/972
"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKPerformanceBenchmarking.git
  GIT_TAG 5bbb55da0e47f19b5c518e5b60eac4e8f3b5cc5c
  )
