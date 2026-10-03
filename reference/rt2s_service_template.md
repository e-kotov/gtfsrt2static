# Generate a Supervised-Service Template for the Collector

Emits a ready-to-edit service definition that keeps
[`rt2s_collect`](https://e-kotov.github.io/gtfsrt2static/reference/rt2s_collect.md)
running unattended: a systemd unit, a launchd plist, a cron @reboot
line, or a Dockerfile.

## Usage

``` r
rt2s_service_template(
  type = c("systemd", "launchd", "cron", "docker"),
  config_path = "/etc/gtfsrt2static/feeds.csv",
  dir = "/var/lib/gtfsrt-archive"
)
```

## Arguments

- type:

  One of `"systemd"`, `"launchd"`, `"cron"`, `"docker"`.

- config_path:

  Path to an R script or RDS/CSV holding the feeds config; referenced in
  the generated command.

- dir:

  Archive root directory used in the generated command.

## Value

The template as a character scalar (also printed with
[`cat()`](https://rdrr.io/r/base/cat.html) for copy-pasting).
