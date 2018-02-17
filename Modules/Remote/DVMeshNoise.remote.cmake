# Contact: Davis Marc Vigneault <davis.vigneault@gmail.com>
itk_fetch_module(DVMeshNoise
  "Perturb itk::Mesh and itk::QuadEdgeMesh coordinates with Gaussian noise.
  Please see the following Insight Journal article for an introduction to this module:
  Vigneault, DM.  Perturbing Mesh Vertices with Additive Gaussian Noise.
  http://hdl.handle.net/10380/3567
  "
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/DVMeshNoise
  GIT_TAG 38a49c7282dde9b843e13a17e14bb95ac706a057
  )
