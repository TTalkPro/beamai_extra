# Changelog

## [2.0.0] - 2026-10-19

### Changed

- 上游 BeamAI 依赖锁定到 `v2.0.0` tag（commit `08aa844a11791fabc20ff2dab19ea82a71343881`）。
- `beamai_extra` 及全部本地 OTP 应用版本统一升级到 `2.0.0`。
- 跟随上游依赖调整：Gun 升级到 `2.7.0`（Cowlib `2.21.0`），Meck 升级到 `1.2.0`，
  并固定 `rebar3_proper` 和 `rebar3_ex_doc` 插件版本。
- 文档改用 v2.0.0 的 SimpleAgent Memory API 和 11 个内置 LLM provider，
  移除已从上游删除的 Process / DeepAgent 示例与说明。
