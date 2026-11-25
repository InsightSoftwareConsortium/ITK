# ITKproxTV

This ITK third-party module builds [proxTV](https://github.com/albarji/proxTV),
a toolbox of fast total variation proximity operators, for use by the
`TotalVariation` module.

The library source is not stored in the ITK tree. It is cloned and built at
build time from the InsightSoftwareConsortium proxTV fork via
`ExternalProject`, pinned in `proxTVGitTag.cmake`. The module is
`EXCLUDE_FROM_DEFAULT`; it is only configured and built when a module that
depends on it (`TotalVariation`) is enabled.

Set `ITK_USE_SYSTEM_proxTV` to use an external build instead.
