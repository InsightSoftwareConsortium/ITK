Access to ITK
=============

This page documents how to setup ITK developer accounts. See our
[CONTRIBUTING](../CONTRIBUTING.md) guide for more information.

Gerrit
------

Anyone may obtain access to the [ITK Gerrit Code Review] site to propose and
review changes. Follow these steps to set up access:

1. We use [GitHub] OAuth, so sign in to your GitHub account.
   **Note**: *OAuth currently requests R/W access to GitHub repositories.
   However, none of the repositories will be modified.*

2. Sign in to http://review.source.kitware.com using the link in the upper
   right and approve when GitHub prompts

3. Set all fields in your profile at http://review.source.kitware.com/#/settings/
   Add your ssh public key at http://review.source.kitware.com/#/settings/ssh-keys


Git
---

Authorized community members may merge patches to the `itk.org` repositories,
but **most community members do not need merge permissions**. All patches from
all community members go through Gerrit Code Review, and patches will be merged
by one of the maintainers after it has been reviewed.

Follow these steps to set up access:

1. Submit your ssh public key to the
   [Kitware Password form](https://www.kitware.com/Admin/SendPassword.cgi)

2. Perform an authentication test:

```sh
   $ ssh git@itk.org info
```


[ITK Gerrit Code Review]: http://review.source.kitware.com/p/ITK

[Git]: http://git-scm.com
[GitHub]: https://github.com/