Introduction
============

ITK version tracking and development is hosted by [Git](https://git-scm.com/).
Please select a task for further instructions:

Main Tasks
----------

  * [Install Git](https://itk.org/Wiki/Git/Download) - Git 2.8.9 or greater is
    preferred (required for development)
  * [Download ITK] - Users start here
  * [Develop ITK] - Contributors start here

Other Tasks
-----------

  * [Test ITK] - CDash client setup
  * [Learn Git](https://itk.org/Wiki/Git/Resources) - Third-party documentation

*The remainder of this page provides reference information and links. It is
not intended to provide instructions.*

Repositories
------------

One may browse the repositories online using the [GitHub] search interface at
[InsightSoftwareConsortium/ITK].

Branches
--------

At the time of this writing the `ITK.git` repository has the following
branches:

  * `master`: Development (default)
  * `release`: Maintenance of latest release
  * `release-3.20`: Maintenance of the ITKv3 series
  * `release-4.13`: Maintenance of the ITKv4 series
  * `nightly-master`: Follows master, updated at 01:00 UTC
  * `hooks`: Local commit hooks (place in `.git/hooks`)
  * `dashboard`: Dashboard script (setup a CDash client)

Actual releases have tags named by the release version number.


[Dashboard]: Dashboard.md
[Develop ITK]: CONTRIBUTING.md
[Download ITK]: Download.md
[InsightSoftwareConsortium/ITK]: https://github.com/InsightSoftwareConsortium/ITK
[Test ITK]: Dashboard.md

[GitHub]: https://github.com/
