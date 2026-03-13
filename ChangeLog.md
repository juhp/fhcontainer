# Release Changes

## 0.3.1 (2026-03-13)
- `--mount`: append ":Z" for private namespace mapping
- map n < 8 to centos:n
- Dist parser: ignore container targets with ":"

## 0.3 (2025-11-29)
- use parsec to read distro: support centos devel/minimal
- also handle pure version numbers and eln

## 0.2.2 (2025-06-04)
- no longer use the development channel for c10s

## 0.2.1 (2024-09-24)
- output image date when running a container
- map c10s to centos:stream10-development
- refactored into modules
