# Security policy

## Supported versions

Security fixes are made against the current released finrb line. Older
versions may receive a fix when practical, but they are not guaranteed to be
maintained. Users should reproduce an issue on the newest release before
reporting it when possible.

JRuby and TruffleRuby are informational compatibility targets. A
runtime-specific vulnerability may also need to be reported to the relevant
Ruby implementation or dependency maintainers.

## Reporting a vulnerability

Do not disclose a suspected vulnerability in a public issue, discussion, or
pull request. Use GitHub's private vulnerability reporting for finrb:

<https://github.com/ncs1/finrb/security/advisories/new>

If private reporting is unavailable, contact the maintainer through the email
listed in the gem metadata. Include only enough information in the initial
message to establish a private channel.

A useful report contains:

- affected finrb and Ruby versions;
- the affected API or packaged artifact;
- minimal reproduction steps;
- the expected and observed impact; and
- any known mitigations or disclosure constraints.

Please avoid including production credentials, private financial data, access
tokens, or other secrets. There is no guaranteed response or remediation
timeline; this project is maintained by one person. Reports will be evaluated
according to reproducibility, impact, and available maintainer capacity.

Numerical disagreement alone is normally a correctness bug rather than a
security vulnerability. Treat it as security-sensitive when it can cross a
trust boundary, bypass validation, enable denial of service, corrupt packaged
artifacts, or predictably cause unsafe downstream financial decisions.
