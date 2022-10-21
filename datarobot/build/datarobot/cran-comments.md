## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

This is the second submission for 2.18.2.

I saw on the repository policy page that a package that is NOT in a mainstream repository can be marked in metadata as obtainable from a separate repo.

Packages on which a CRAN package depends should be available from a mainstream repository: if any mentioned in ‘Suggests’ or ‘Enhances’ fields are not from such a repository, where to obtain them at a repository should be specified in an ‘Additional_repositories’ field of the DESCRIPTION file (as a comma-separated list of repository URLs) or for other means of access, described in the ‘Description’ field.

I've updated the DESCRIPTION to reflect this, and the test code that uses stubthat remains conditionally used.

We are still working on updating the tests themselves -- we have a very large test suite that relies on stubthat.
