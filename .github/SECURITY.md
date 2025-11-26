# Security Policy

## Supported Versions

We currently support the following versions with security updates:

| Version | Supported          |
| ------- | ------------------ |
| 0.5.x   | :white_check_mark: |
| < 0.5   | :x:                |

## Reporting a Vulnerability

If you discover a security vulnerability in Sipha, please report it responsibly.

**Please do not** open a public GitHub issue for security vulnerabilities.

Instead, please email security concerns to: **contact@nyaleph.com**

Include the following information:
- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Suggested fix (if you have one)

We will acknowledge receipt of your report within 48 hours and provide an update on our response within 7 days.

## Security Updates

Security updates will be released as patch versions (e.g., 0.5.0 → 0.5.1) and will be documented in the CHANGELOG.

## Security Best Practices

When using Sipha:

- Keep your dependencies up to date
- Review the code you're parsing, especially when parsing untrusted input
- Use appropriate error handling for parsing failures
- Be cautious when using `unsafe` code in your parsers (Sipha itself uses minimal unsafe code)

Thank you for helping keep Sipha and its users safe!

